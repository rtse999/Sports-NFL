# ------------------------------------------------------------------------
# Find timeouts taken by the defensive team between 0-6 seconds from the 
#   4th quarter 2 min warning from NFL play by play data
#   https://github.com/ryurko/nflscrapR-data/tree/master/play_by_play_data
#
# Location: /Users/raymondtse/Dropbox/Analysis/Sports/fn_timeout_126sec_4qtr.r
# First created: 16:20 - 15:44 - Sunday 30 December 2018
# Last modified: 23:58 - 15:44 - Sunday 30 December 2018
# ------------------------------------------------------------------------

timeout_126sec_4qtr <- function (pbp_data) {
  pbp_data %>% 
    dplyr::filter(timeout == 1) %>% 
    dplyr::filter(game_seconds_remaining <= 126 & game_seconds_remaining > 120) %>% 
    dplyr::filter(timeout_team == defteam)
}
