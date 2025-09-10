# api.R — plumber endpoints backed by nflreadr (defensive version)
library(plumber)
library(nflreadr)
library(dplyr)
library(stringr)

# helper: minimal weekly passing TD frame for seasons
get_weekly <- function(seasons) {
  nflreadr::load_player_stats(seasons = seasons) %>%
    select(season, week, player_id, player_name, recent_team, opponent, passing_tds) %>%
    rename(team = recent_team)
}

# ------------- Endpoints ----------------

#* Health
#* @get /healthz
function() {
  list(ok = TRUE, nflreadr = as.character(utils::packageVersion("nflreadr")))
}

#* Return weekly rows for a player (case-insensitive)
#* @param player
#* @param seasons
#* @get /qb_weekly
function(player = "", seasons = "") {
  tryCatch({
    if (player == "" || seasons == "") {
      return(list(error = "player and seasons required, e.g. ?player=Aaron%20Rodgers&seasons=2025,2024"))
    }
    yrs <- as.integer(strsplit(seasons, ",", fixed = TRUE)[[1]])
    wk <- get_weekly(yrs)

    wk %>%
      filter(str_to_lower(player_name) == str_to_lower(player)) %>%
      arrange(season, week)
  }, error = function(e) {
    list(error = paste("qb_weekly failed:", conditionMessage(e)))
  })
}

#* Return lambda estimate for QB passing TDs with optional opponent adjust
#* @param player
#* @param seasons Example: 2025,2024
#* @param window:integer Default 16
#* @param opponent Optional team abbr to adjust vs league avg allowed
#* @get /qb_lambda
function(player = "", seasons = "", window = 16, opponent = "") {
  tryCatch({
    if (player == "" || seasons == "") {
      return(list(error = "player and seasons required"))
    }
    window <- as.integer(window)
    yrs <- as.integer(strsplit(seasons, ",", fixed = TRUE)[[1]])
    wk <- get_weekly(yrs)

    qb <- wk %>%
      filter(str_to_lower(player_name) == str_to_lower(player)) %>%
      arrange(season, week)
    if (nrow(qb) == 0) {
      return(list(error = paste("no rows for", player)))
    }

    # base lambda = mean of last N games, NAs treated as 0
    vec <- qb %>% tail(window) %>% pull(passing_tds)
    vec[is.na(vec)] <- 0
    base <- mean(vec)

    # optional opponent adjustment
    if (opponent != "") {
      def_allow <- wk %>%
        group_by(season, opponent) %>%
        summarise(opp_allow_tdpg = mean(passing_tds, na.rm = TRUE), .groups = "drop") %>%
        rename(defteam = opponent)

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
