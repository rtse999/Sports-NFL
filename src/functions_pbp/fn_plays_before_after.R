# ------------------------------------------------------------------------
# For a game_id, play_id - return the play immediately before, that play
#   and the play after it from NFL play by play data
#   https://github.com/ryurko/nflscrapR-data/tree/master/play_by_play_data
#
# Location: /Users/raymondtse/Dropbox/Analysis/Sports/fn_plays_before_after.r
# First created: 15:44 - Sunday 30 December 2018
# Last modified: 20:39 - Sunday 30 December 2018
# ------------------------------------------------------------------------

plays_before_after <- function(pbp, game, play) {
  play_before <- 
    pbp %>% 
    select(game_id, play_id) %>% 
    dplyr::filter(game_id == game, play_id < play) %>% 
    select(play_id) 
  
  if(nrow(play_before) == 0) {
    play_before = play
  } else {
    play_before = max(play_before)    
  }
  
  play_after <- 
    pbp %>% 
    select(game_id, play_id) %>% 
    dplyr::filter(game_id == game, play_id > play) %>% 
    select(play_id) 
  
  if(nrow(play_after) == 0) {
    play_after = play
  } else {
    play_after = min(play_after)    
  }
  
  plays_before_after <-
    pbp %>% 
    dplyr::filter(game_id == game, play_id %in% (play_before:play_after))
}
