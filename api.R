# api.R — Plumber endpoints backed by nflreadr (schema-robust + prefers display name)
library(plumber)
library(nflreadr)
library(dplyr)
library(stringr)

# -------- Helpers --------

# Load weekly player stats for given seasons and normalize column names across versions
get_weekly <- function(seasons) {
  # offense keeps payload tighter than "all"
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
  # Common possibilities across versions: player_display_name, player_name, player
  name_col <- if ("player_display_name" %in% nm) "player_display_name"
    else if ("player_name" %in% nm) "player_name"
    else if ("player" %in% nm) "player"
    else NA_character_

  if (is.na(name_col)) stop("No player name column found in load_player_stats output.")

  df <- df %>% mutate(name = .data[[name_col]])

  # Ensure passing_tds exists (offense should have it)
  if (!"passing_tds" %in% names(df)) {
    stop("Column 'passing_tds' not found in load_player_stats output.")
  }

  # Keep only what we need
  df %>%
    select(any_of(c("season","week","player_id","name","team","opponent_team","passing_tds")))
}

# Normalize names: lowercase, strip punctuation, drop common suffixes, squish spaces
norm_name <- function(x) {
  x |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^a-z0-9 ]", " ") |>
    stringr::str_replace_all("\\b(ii|iii|iv|jr|sr)\\b", "") |>
    stringr::str_squish()
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

  # nothing
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
