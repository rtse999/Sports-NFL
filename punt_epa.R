# ------------------------------------------------------------------------
# Highest EPA punts in the 2018/19 regular season
#
# Link: see https://twitter.com/ThePuntRunts/status/1083121505964310529
#
# Location: /Users/raymondtse/Dropbox/Analysis/github packages/nflscrapr/punt_epa.r
# First created: 22:23 - Thursday 10 January 2019
# Last modified: 22:23 - Thursday 10 January 2019
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# System time
# ------------------------------------------------------------------------
format(Sys.time(), "%a %b %d %H:%M:%S %Y")

# ------------------------------------------------------------------------
# Install Packages
# ------------------------------------------------------------------------
library(conflicted)
library(skimr)
library(stringr)
library(tidyverse)

# ------------------------------------------------------------------------
# Load functions
# ------------------------------------------------------------------------
source(file = "fn_read_pbp_data.r")

# ------------------------------------------------------------------------
# Read data
# ------------------------------------------------------------------------
reg_pbp_2018 <- read_pbp_data("Data/PBP/reg_pbp_2018.csv")

# ------------------------------------------------------------------------
# Find punt referenced in tweet (see link above)
# ------------------------------------------------------------------------
tweet_punt <- 
  reg_pbp_2018 %>% 
  dplyr::filter(game_id == 2018112900, punt_attempt == 1, posteam == "NO", 
                play_id == 650)

tweet_punt$epa # Matches the EPA of ~1.67 in tweet

# ------------------------------------------------------------------------
# Find highest EPA punts in 2018 regular season
# ------------------------------------------------------------------------
high_epa_punts <- 
  reg_pbp_2018 %>% 
  dplyr::filter(punt_attempt == 1, penalty != 1, epa > 1.5) %>% 
  arrange(desc(epa)) %>% 
  select(play_id:posteam, game_date, game_seconds_remaining, desc, 
         total_home_score, total_away_score, kick_distance, epa)

# write_csv(high_epa_punts, "high_epa_punts.csv")


