# api.R — plumber endpoints backed by nflreadr (defensive version)
library(plumber)
library(nflreadr)
library(dplyr)
library(stringr)

# helper: minimal weekly passing TD frame for seasons
get_weekly <- function(seasons) {
  df <- nflreadr::load_player_stats(seasons = seasons) %>%
    select(season, week, player_id, player_name, recent_team, opponent, passing_tds) %>%
    rename(team = recent_team)
  df
}

#------------- Endpoints ----------------

#* Health
#* @get /healthz
function(){ list(ok=TRUE, nflreadr=as.character(utils::packageVersion("nflreadr"))) }

#* Return weekly rows for a player (case-insensitive)
#* @param player
#* @param seasons
#* @get /qb_weekly
function(player="", seasons="") {
  tryCatch({
    if (player == "" || seasons == "") {
      return(list(error="player and seasons required, e.g. ?player=Aaron%20Rodgers&seasons=2025,2024"))
    }
    yrs <- as.integer(strsplit(seasons, ",")[[1]])
    wk <- get_weekly(yrs)
    wk %>%
      filter(str_to_lower(player_name) == str_to_lower(player)) %>%
      arrange(season, week)
  }, error = function(e) {
    list(error=paste("qb_weekly failed:", conditionMessage(e)))
  })
}

#* Return lambda estimate for QB passing TDs with optional opponent adjust
#* @param player
#* @param seasons Example: 2025,2024
#* @param window:integer Default 16
#* @param opponent Optional team abbr to adjust vs league avg allowed
#* @get /qb_lambda
function(player="", seasons="", window=16, opponent="") {
  tryCatch({
    if (player == "" || seasons == "") {
      return(list(error="player and seasons required"))
    }
    window <- as.integer(window)
    yrs <- as.integer(strsplit(seasons, ",")[[1]])
    wk <- get_weekly(yrs)

    qb <- wk %>%
      filter(str_to_lower(player_name) == str_to_lower(player)) %>%
      arrange(season, week)
    if (nrow(qb
