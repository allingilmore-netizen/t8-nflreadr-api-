# api.R — Plumber endpoints backed by nflreadr (schema-robust + name matching)
library(plumber)
library(nflreadr)
library(dplyr)
library(stringr)

# -------- Helpers --------

# Load weekly player stats for given seasons and normalize column names across versions
get_weekly <- function(seasons) {
  # offense = passing/rushing/receiving; keeps payload smaller than "all"
  df <- nflreadr::load_player_stats(seasons = seasons, stat_type = "offense")
  nm <- names(df)

  # normalize player name column
  if (!"player_name" %in% nm && "player_display_name" %in% nm) {
    df <- dplyr::rename(df, player_name = player_display_name)
  }

  # normalize team/opponent columns
  if (!"team" %in% nm && "recent_team" %in% nm) {
    df <- dplyr::rename(df, team = recent_team)
  }
  if (!"opponent_team" %in% nm && "opponent" %in% nm) {
    df <- dplyr::rename(df, opponent_team = opponent)
  }

  # make sure passing_tds exists (it should for offense stats)
  if (!"passing_tds" %in% names(df)) {
    stop("Column 'passing_tds' not found in nflreadr::load_player_stats output.")
  }

  dplyr::select(
    df,
    dplyr::any_of(c("season","week","player_id","player_name",
                    "team","opponent_team","passing_tds"))
  )
}

# Normalize names: lowercase, strip punctuation, drop common suffixes, squish spaces
norm_name <- function(x) {
  x |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^a-z0-9 ]", " ") |>
    stringr::str_replace_all("\\b(ii|iii|iv|jr|sr)\\b", "") |>
    stringr::str_squish()
}

# Find rows for a player using robust matching; falls back to starts-with / contains
find_player_rows <- function(wk, player) {
  # choose the name column (after normalization we expect player_name)
  name_col <- if ("player_name" %in% names(wk)) "player_name"
              else if ("player_display_name" %in% names(wk)) "player_display_name"
              else stop("No player name column found.")
  name_vec <- wk[[name_col]]

  p_norm <- norm_name(player)
  wk <- wk |> dplyr::mutate(player_norm = norm_name(name_vec))

  exact <- wk |> dplyr::filter(player_norm == p_norm)
  if (nrow(exact) > 0) return(dplyr::select(exact, -player_norm))

  pref <- wk |> dplyr::filter(stringr::str_starts(player_norm, p_norm))
  if (nrow(pref) > 0) return(dplyr::select(pref, -player_norm))

  cont <- wk |> dplyr::filter(stringr::str_detect(player_norm, stringr::fixed(p_norm)))
  if (nrow(cont) > 0) return(dplyr::select(cont, -player_norm))

  # nothing
  wk[0, setdiff(names(wk), "player_norm"), drop = FALSE]
}

# Up to 5 name suggestions to help callers correct input
name_suggestions <- function(wk, player) {
  name_col <- if ("player_name" %in% names(wk)) "player_name"
              else if ("player_display_name" %in% names(wk)) "player_display_name"
              else return(character())
  p_norm <- norm_name(player)
  wk2 <- wk |> dplyr::mutate(player_norm = norm_name(.data[[name_col]]))
  cand <- wk2 |>
    dplyr::filter(
      stringr::str_starts(player_norm, p_norm) |
      stringr::str_detect(player_norm, stringr::fixed(p_norm))
    ) |>
    dplyr::distinct(.data[[name_col]]) |>
    dplyr::arrange(.data[[name_col]]) |>
    head(5)

  # pull() with tidyselect fails on computed names; use [[ ]] safely
  if (nrow(cand) == 0) character()
  else unname(cand[[1]])
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

#* Weekly rows for a player (robust name match)
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

    qb <- find_player_rows(wk, player) |> dplyr::arrange(season, week)
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

    qb <- find_player_rows(wk, player) |> dplyr::arrange(season, week)
    if (nrow(qb) == 0) {
      return(list(
        error = paste("no rows for", player),
        suggestions = name_suggestions(wk, player)
      ))
    }

    # base lambda = mean of last N games; treat NA as 0
    vec <- qb |> dplyr::pull(passing_tds)
    vec <- tail(vec, window)
    vec[is.na(vec)] <- 0
    base <- mean(vec)

    # optional opponent adjustment (needs opponent_team column in wk)
    if (opponent != "" && "opponent_team" %in% names(wk)) {
      def_allow <- wk |>
        dplyr::group_by(season, opponent_team) |>
        dplyr::summarise(
          opp_allow_tdpg = mean(passing_tds, na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::rename(defteam = opponent_team)

      lg_avg <- mean(def_allow$opp_allow_tdpg, na.rm = TRUE)
      row <- def_allow |> dplyr::filter(defteam == opponent)

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
