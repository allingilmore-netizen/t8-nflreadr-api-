# api.R — plumber endpoints backed by nflreadr
library(plumber)
library(nflreadr)
library(dplyr)
library(stringr)

# helper: minimal weekly passing TD frame for seasons
get_weekly <- function(seasons) {
  # player "weekly" summary via nflverse player stats
  df <- nflreadr::load_player_stats(seasons = seasons) %>%
    select(season, week, player_id, player_name, recent_team, opponent,
           passing_tds) %>%
    rename(team = recent_team)
  df
}

#* Health
#* @get /healthz
function(){ list(ok=TRUE, package=as.character(utils::packageVersion("nflreadr"))) }

#* Return weekly rows for a player (case-insensitive)
#* @param player
#* @param seasons
#* @get /qb_weekly
function(player="", seasons="") {
  if (player == "" || seasons == "") {
    return(list(error="player and seasons required, e.g. ?player=Aaron%20Rodgers&seasons=2024,2025"))
  }
  yrs <- as.integer(strsplit(seasons, ",")[[1]])
  wk <- get_weekly(yrs)
  wk %>%
    filter(str_to_lower(player_name) == str_to_lower(player)) %>%
    arrange(season, week)
}

#* Return lambda estimate for QB passing TDs with optional opponent adjust
#* @param player
#* @param seasons Example: 2025,2024
#* @param window:integer Default 16
#* @param opponent Optional team abbr to adjust vs league avg allowed
#* @get /qb_lambda
function(player="", seasons="", window=16, opponent="") {
  if (player == "" || seasons == "") {
    return(list(error="player and seasons required"))
  }
  window <- as.integer(window)
  yrs <- as.integer(strsplit(seasons, ",")[[1]])
  wk <- get_weekly(yrs)
  qb <- wk %>%
    filter(str_to_lower(player_name) == str_to_lower(player)) %>%
    arrange(season, week)
  if (nrow(qb) == 0) return(list(error=paste("no rows for", player)))

  base <- qb %>% tail(window) %>% pull(passing_tds) %>% replace_na(0) %>% mean()

  if (opponent != "") {
    def_allow <- wk %>%
      group_by(season, opponent) %>%
      summarise(opp_allow_tdpg = mean(passing_tds, na.rm=TRUE), .groups="drop") %>%
      rename(defteam = opponent)
    lg_avg <- mean(def_allow$opp_allow_tdpg, na.rm=TRUE)
    row <- def_allow %>% filter(defteam == opponent)
    if (nrow(row) > 0 && is.finite(lg_avg) && lg_avg > 0) {
      factor <- as.numeric(row$opp_allow_tdpg[1]) / lg_avg
      base <- base * max(0.6, min(1.4, factor))
    }
  }

  list(lambda = round(max(0.2, min(3.5, base)), 3),
       window = window, seasons = yrs, player = player, opponent = opponent)
}
