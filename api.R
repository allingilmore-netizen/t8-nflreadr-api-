# api.R — plumber endpoints backed by nflreadr (robust matching + schema-robust)
library(plumber)
library(nflreadr)
library(dplyr)
library(stringr)

# -------- Helpers --------

# Normalize naming differences across nflreadr versions
get_weekly <- function(seasons) {
  df <- nflreadr::load_player_stats(seasons = seasons)
  nm <- names(df)
  if (!"team" %in% nm && "recent_team" %in% nm) {
    df <- dplyr::rename(df, team = recent_team)
  }
  if (!"opponent_team" %in% nm && "opponent" %in% nm) {
    df <- dplyr::rename(df, opponent_team = opponent)
  }
  df %>%
    select(any_of(c("season","week","player_id","player_name",
                    "team","opponent_team","passing_tds")))
}

# Normalize player names (lowercase, strip suffixes/punct, squish spaces)
norm_name <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9 ]", " ") %>%        # remove punctuation
    str_replace_all("\\b(ii|iii|iv|jr|sr)\\b", "") %>% # drop common suffixes
    str_squish()
}

# Find rows for a player using robust matching; fall back to starts-with
find_player_rows <- function(wk, player) {
  p_norm <- norm_name(player)
  wk <- wk %>% mutate(player_norm = norm_name(player_name))
  exact <- wk %>% filter(player_norm == p_norm)
  if (nrow(exact) > 0) return(exact %>% select(-player_norm))

  # fallback: begins-with (handles players with middle/extra tokens)
  pref <- wk %>% filter(str_starts(player_norm, p_norm))
  if (nrow(pref) > 0) return(pref %>% select(-player_norm))

  # fallback: contains (very loose)
  cont <- wk %>% filter(str_detect(player_norm, fixed(p_norm)))
  if (nrow(cont) > 0) return(cont %>% select(-player_norm))

  # nothing found
  wk[0, setdiff(names(wk), "player_norm"), drop = FALSE]
}

# Top-5 suggestions (distinct names) to help the caller correct input
name_suggestions <- function(wk, player) {
  p_norm <- norm_name(player)
  wk2 <- wk %>% mutate(player_norm = norm_name(player_name))
  cand <- wk2 %>%
    filter(
      str_starts(player_norm, p_norm) | str_detect(player_norm, fixed(p_norm))
    ) %>%
    distinct(player_name) %>%
    arrange(player_name) %>%
    head(5) %>%
    pull(player_name)
  unname(cand)
}

# ------------- Endpoints ----------------

#* Health
#* @get /healthz
function() {
  list(ok = TRUE, nflreadr = as.character(utils::packageVersion("nflreadr")))
}

#* Return weekly rows for a player (case-insensitive, robust)
#* @param player
#* @param seasons
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

#* Return lambda estimate for QB passing TDs with optional opponent adjust
#* @param player
#* @param seasons Example: 2025,2024
#* @param window:integer Default 16
#* @param opponent Optional team abbr to adjust vs league avg allowed (e.g., BAL)
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

    # base lambda = mean of last N games, NAs -> 0
    vec <- qb %>% tail(window) %>% pull(passing_tds)
    vec[is.na(vec)] <- 0
    base <- mean(vec)

    # optional opponent adjustment (uses normalized opponent_team)
    if (opponent != "" && "opponent_team" %in% names(wk)) {
      def_allow <- wk %>%
        group_by(season, opponent_team) %>%
        summarise(opp_allow_tdpg = mean(passing_tds, na.rm = TRUE), .groups = "drop") %>%
        rename(defteam = opponent_team)

      lg_avg <- mean(def_allow$opp_allow_tdpg, na.rm = TRUE)
      row <- def_allow %>% filter(defteam == opponent)

      if (nrow(row) > 0 && is.finite(lg_avg) && lg_avg > 0) {
        factor <- as.numeric(row$opp_allow_tdpg[1]) / lg_avg
        base <- base * max(0.6, min(1.4, factor))
      }
    }

    out_lambda <- round(max(0.2, min(3.5, base)), 3)
    list(lambda = out_lambda,
         window = window, seasons = yrs, player = player, opponent = opponent)
  }, error = function(e) {
    list(error = paste("qb_lambda failed:", conditionMessage(e)))
  })
}
