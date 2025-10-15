# api.R — Plumber endpoints backed by nflreadr (schema-robust + prefers display name)

library(plumber)
library(nflreadr)
library(dplyr)
library(stringr)
library(jsonlite)
library(rlang)   # for sym() if ever needed

# -------- Helpers --------

# Normalize names: lowercase, strip punctuation, drop common suffixes, squish spaces
norm_name <- function(x) {
  x |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^a-z0-9 ]", " ") |>
    stringr::str_replace_all("\\b(ii|iii|iv|jr|sr)\\b", "") |>
    stringr::str_squish()
}

# A simpler key for fuzzy lookups
norm_key <- function(s) stringr::str_squish(tolower(s))

# Load weekly player stats for given seasons and normalize column names across versions
get_weekly <- function(seasons) {
  df <- nflreadr::load_player_stats(seasons = seasons, stat_type = "offense")
  nm <- names(df)

  # Normalize team/opponent columns
  if (!"team" %in% nm && "recent_team" %in% nm) {
    df <- dplyr::rename(df, team = recent_team)
  }
  if (!"opponent_team" %in% nm && "opponent" %in% nm) {
    df <- dplyr::rename(df, opponent_team = opponent)
  }

  # Build a unified 'name' column, preferring full display name when available
  name_col <- if ("player_display_name" %in% nm) "player_display_name"
         else if ("player_name" %in% nm) "player_name"
         else if ("player" %in% nm) "player"
         else NA_character_
  if (is.na(name_col)) stop("No player name column found in load_player_stats output.")

  df <- df %>% mutate(name = .data[[name_col]])

  # Ensure passing_tds exists
  if (!"passing_tds" %in% names(df)) {
    stop("Column 'passing_tds' not found in load_player_stats output.")
  }

  df %>%
    select(any_of(c("season","week","player_id","name","team","opponent_team","passing_tds")))
}

# Find rows for a player using robust matching on unified 'name'
find_player_rows <- function(wk, player) {
  if (!"name" %in% names(wk)) stop("Internal error: 'name' column missing after normalization.")
  p_norm <- norm_name(player)
  wk2 <- wk %>% mutate(name_norm = norm_name(name))

  exact <- wk2 %>% filter(name_norm == p_norm)
  if (nrow(exact) > 0) return(exact %>% select(-name_norm))

  pref <- wk2 %>% filter(str_starts(name_norm, p_norm))
  if (nrow(pref) > 0) return(pref %>% select(-name_norm))

  cont <- wk2 %>% filter(str_detect(name_norm, fixed(p_norm)))
  if (nrow(cont) > 0) return(cont %>% select(-name_norm))

  wk2[0, setdiff(names(wk2), "name_norm"), drop = FALSE]
}

# Up to 5 suggestions to help callers correct input
name_suggestions <- function(wk, player) {
  if (!"name" %in% names(wk)) return(character())
  p_norm <- norm_name(player)
  wk2 <- wk %>% mutate(name_norm = norm_name(name))
  cand <- wk2 %>%
    filter(str_starts(name_norm, p_norm) | str_detect(name_norm, fixed(p_norm))) %>%
    distinct(name) %>%
    arrange(name) %>%
    head(5) %>%
    pull(name)
  unname(cand)
}

# -------- Endpoints --------

#* Health
#* @get /healthz
function() {
  list(
    ok = TRUE,
    nflreadr = as.character(utils::packageVersion("nflreadr"))
  )
}

#* Weekly rows for a player (robust name match, prefers display name)
#* @param player
#* @param seasons Comma-separated years, e.g. 2025,2024
#* @get /qb_weekly
function(player = "", seasons = "") {
  tryCatch({
    if (player == "" || seasons == "") {
      return(list(error = "player and seasons required, e.g. ?player=Patrick%20Mahomes&seasons=2025,2024"))
    }
    yrs <- as.integer(strsplit(seasons, ",", fixed = TRUE)[[1]])
    wk <- get_weekly(yrs)

    qb <- find_player_rows(wk, player) %>% arrange(season, week)
    if (nrow(qb) == 0) {
      return(list(
        error = paste("no rows for", player),
        suggestions = name_suggestions(wk, player)
      ))
    }
    qb
  }, error = function(e) {
    list(error = paste("qb_weekly failed:", conditionMessage(e)))
  })
}

#* Lambda estimate for QB passing TDs (optional opponent adjust)
#* @param player
#* @param seasons Example: 2025,2024
#* @param window:integer Rolling games window (default 16)
#* @param opponent Optional defense abbr to adjust vs league avg allowed (e.g., BAL)
#* @get /qb_lambda
function(player = "", seasons = "", window = 16, opponent = "") {
  tryCatch({
    if (player == "" || seasons == "") {
      return(list(error = "player and seasons required"))
    }
    window <- as.integer(window)
    yrs <- as.integer(strsplit(seasons, ",", fixed = TRUE)[[1]])
    wk <- get_weekly(yrs)

    qb <- find_player_rows(wk, player) %>% arrange(season, week)
    if (nrow(qb) == 0) {
      return(list(
        error = paste("no rows for", player),
        suggestions = name_suggestions(wk, player)
      ))
    }

    # base lambda = mean of last N games; treat NA as 0
    vec <- qb %>% pull(passing_tds)
    vec <- tail(vec, window)
    vec[is.na(vec)] <- 0
    base <- mean(vec)

    # optional opponent adjustment (needs opponent_team)
    if (opponent != "" && "opponent_team" %in% names(wk)) {
      def_allow <- wk %>%
        group_by(season, opponent_team) %>%
        summarise(opp_allow_tdpg = mean(passing_tds, na.rm = TRUE), .groups = "drop") %>%
        rename(defteam = opponent_team)

      lg_avg <- mean(def_allow$opp_allow_tdpg, na.rm = TRUE)
      row <- def_allow %>% filter(defteam == opponent)

      if (nrow(row) > 0 && is.finite(lg_avg) && lg_avg > 0) {
        factor <- as.numeric(row$opp_allow_tdpg[1]) / lg_avg
        base <- base * max(0.6, min(1.4, factor))  # clamp adjustment
      }
    }

    out_lambda <- round(max(0.2, min(3.5, base)), 3)
    list(lambda = out_lambda,
         window = window, seasons = yrs, player = player, opponent = opponent)
  }, error = function(e) {
    list(error = paste("qb_lambda failed:", conditionMessage(e)))
  })
}

#* Fuzzy search for player by name (rosters-first; fallback to players)
#* @param query string: partial or full name (e.g., "mahomes")
#* @param limit integer: number of results (default 5)
#* @param seasons string: optional comma-separated years (e.g., "2025,2024"); defaults to most recent
#* @get /player_search
function(query, limit = 5, seasons = NULL) {
  tryCatch({
    # --- validate ---
    limit <- suppressWarnings(as.integer(limit))
    if (is.na(limit) || limit < 1) limit <- 5
    if (is.null(query) || nchar(query) == 0) {
      return(plumber::response(status = 400, body = list(error = "query is required")))
    }

    # --- helpers ---
    .key <- function(s) stringr::str_squish(tolower(s))
    key  <- .key(query)

    # choose seasons (keep payload small)
    yrs <- tryCatch({
      if (!is.null(seasons) && nchar(seasons) > 0) {
        y <- as.integer(strsplit(seasons, ",", fixed = TRUE)[[1]])
        y[!is.na(y)]
      } else {
        nflreadr::most_recent_season()
      }
    }, error = function(e) nflreadr::most_recent_season())

    # generic search over a data.frame with a known name column
    search_df <- function(df, namecol) {
      # ensure id/team/position exist
      if (!"player_id" %in% names(df)) {
        if ("gsis_id" %in% names(df)) df$player_id <- df$gsis_id else df$player_id <- NA_character_
      }
      if (!"team" %in% names(df) && "recent_team" %in% names(df)) df$team <- df$recent_team
      if (!"team" %in% names(df)) df$team <- NA_character_
      if (!"position" %in% names(df)) df$position <- NA_character_

      # index + match
      df$`__key` <- .key(df[[namecol]])

      res <- df[df$`__key` == key, , drop = FALSE]  # exact
      if (nrow(res) == 0) {                         # fuzzy (edit distance)
        hits <- df[agrepl(key, df$`__key`, max.distance = 0.2, ignore.case = TRUE, useBytes = TRUE), , drop = FALSE]
        if (nrow(hits) == 0) {
          hits <- df[grepl(key, df$`__key`, fixed = TRUE), , drop = FALSE]  # substring
        }
        res <- hits
      }
      if (nrow(res) == 0) return(NULL)

      out <- res[, c("player_id", namecol, "team", "position"), drop = FALSE]
      names(out)[names(out) == namecol] <- "name"

      # return as an array of plain objects (avoid any double-encoding weirdness)
      out <- head(out, limit)
      unname(lapply(seq_len(nrow(out)), function(i) as.list(out[i, , drop = TRUE])))
    }

    # 1) try rosters first (fast/light)
    try_rosters <- tryCatch({
      ro <- nflreadr::load_rosters(yrs)
      if ("full_name" %in% names(ro)) {
        search_df(ro, "full_name")
      } else if ("player_name" %in% names(ro)) {
        search_df(ro, "player_name")
      } else {
        NULL
      }
    }, error = function(e) NULL)

    if (!is.null(try_rosters)) return(try_rosters)

    # 2) fallback: players (heavier)
    try_players <- tryCatch({
      df <- nflreadr::load_players()
      name_col <- if ("display_name" %in% names(df)) "display_name" else if ("player_name" %in% names(df)) "player_name" else NA_character_
      if (is.na(name_col)) stop("No name column in players")
      keep <- intersect(c(name_col, "player_id", "position", "recent_team", "team", "full_name", "gsis_id"), names(df))
      df <- df[, keep, drop = FALSE]
      if (!"team" %in% names(df) && "recent_team" %in% names(df)) df$team <- df$recent_team
      search_df(df, name_col)
    }, error = function(e) NULL)

    if (!is.null(try_players)) return(try_players)

    plumber::response(status = 404, body = list(error = "Player not found"))
  }, error = function(e) {
    plumber::response(status = 500, body = list(error = conditionMessage(e)))
  })
}

#* Canonical NFL team abbreviations
#* @get /team_abbrs
function() {
  teams <- nflreadr::load_teams()
  # Try to find reasonable columns regardless of version
  abbr <- if ("team_abbr" %in% names(teams)) "team_abbr"
          else if ("team" %in% names(teams)) "team"
          else names(teams)[1]
  name <- if ("team_name" %in% names(teams)) "team_name"
          else if ("full_name" %in% names(teams)) "full_name"
          else abbr

  out <- teams |>
    dplyr::select(dplyr::any_of(c(abbr, name))) |>
    dplyr::distinct()

  names(out)[names(out) == abbr] <- "team"
  names(out)[names(out) == name] <- "full_name"

  # Return as plain list-of-objects
  unname(lapply(seq_len(nrow(out)), function(i) as.list(out[i, , drop = TRUE])))
}

#* Last-N games metrics (passing TDs mean, attempts, yds; blend last N and season)
#* @param player string: player name (case-insensitive)
#* @param seasons string: comma-separated years, e.g. "2025,2024"
#* @param window integer: last N games (default 5)
#* @get /qb_recent_form
function(player, seasons, window = 5) {
  yrs <- as.integer(strsplit(seasons, ",", fixed = TRUE)[[1]])
  yrs <- yrs[!is.na(yrs)]
  if (length(yrs) == 0) {
    return(plumber::response(status = 400, body = list(error = "Invalid seasons")))
  }

  weekly <- nflreadr::load_player_stats(yrs)
  name_cols <- intersect(c("player_display_name","player_name","name"), names(weekly))
  if (length(name_cols) == 0) {
    return(plumber::response(status = 500, body = list(error = "Name column not found in dataset")))
  }
  nmcol <- name_cols[1]

  weekly$`__key` <- norm_key(weekly[[nmcol]])
  target <- norm_key(player)
  sub <- weekly[weekly$`__key` == target, , drop = FALSE]
  if ("position" %in% names(sub)) {
    sub <- sub[sub$position == "QB", , drop = FALSE]
  }
  if (nrow(sub) == 0) {
    return(plumber::response(status = 404, body = list(error = "Player not found")))
  }

  # most recent first
  if (all(c("season","week") %in% names(sub))) {
    sub <- sub[order(-sub$season, -sub$week), , drop = FALSE]
  }
  w <- as.integer(window); if (is.na(w) || w < 1) w <- 5
  subN <- head(sub, w)

  td_col  <- c("passing_tds","pass_td")[c("passing_tds","pass_td") %in% names(subN)][1]
  att_col <- c("attempts","passing_attempts")[c("attempts","passing_attempts") %in% names(subN)][1]
  yds_col <- c("passing_yards","pass_yds")[c("passing_yards","pass_yds") %in% names(subN)][1]

  mean_or_null <- function(v) if (is.null(v)) NULL else suppressWarnings(mean(subN[[v]], na.rm = TRUE))
  sd_or_null   <- function(v) if (is.null(v)) NULL else suppressWarnings(sd(subN[[v]], na.rm = TRUE))

  list(
    games = nrow(subN),
    td_mean = mean_or_null(td_col),
    td_std  = sd_or_null(td_col),
    attempts_mean = mean_or_null(att_col),
    yards_mean    = mean_or_null(yds_col)
  )
}

# (Startup is handled by your Docker/entrypoint.)
# pr <- plumb("api.R")
# pr$setDocs("swagger")
# pr$run(host = "0.0.0.0", port = as.integer(Sys.getenv("PORT", 10000)))
