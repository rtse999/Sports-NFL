# ------------------------------------------------------------------------
# Test of nflscrapR library
#
# Link: https://github.com/maksimhorowitz/nflscrapR
# Link: https://tlfvincent.github.io//2017/10/08/nlf-running-back-deep-dive/
#
# Location: /Users/raymondtse/Dropbox/Analysis/Sports/test.r
# First created: 16:20 - Thursday 27 December 2018
# Last modified: 23:58 - Saturday 29 December 2018
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# System time
# ------------------------------------------------------------------------
format(Sys.time(), "%a %b %d %H:%M:%S %Y")

# ------------------------------------------------------------------------
# Install Packages
# ------------------------------------------------------------------------
library(conflicted)
library(devtools)
library(nflscrapR)
library(pander)
library(readr)
library(skimr)
library(tidyverse)

# ------------------------------------------------------------------------
# Load functions
# ------------------------------------------------------------------------
source(file = "fn_read_pbp_data.r")
source(file = "fn_timeout_126sec_4qtr.r")
source(file = "fn_plays_before_after.r")

# ------------------------------------------------------------------------
# Download data
# ------------------------------------------------------------------------
format(Sys.time(), "%a %b %d %H:%M:%S %Y")

# 2018 Week 1 games
s2018_week_1_games <- scrape_game_ids(2018, weeks = 1)

s2018_week_1_games %>% 
  pander()

s2018_week_1_games %>% 
  skim_to_wide()

format(Sys.time(), "%a %b %d %H:%M:%S %Y")

# Collect data for 2018 NFL season - week 1
wk1_pbp_2018 <- 
  s2018_week_1_games %>% 
  pull(game_id) %>% 
  scrape_json_play_by_play()

format(Sys.time(), "%a %b %d %H:%M:%S %Y")


format(Sys.time(), "%a %b %d %H:%M:%S %Y")

reg_pbp_17 <- scrape_season_play_by_play(2017, type = "reg")

format(Sys.time(), "%a %b %d %H:%M:%S %Y")



week_2_games <- scrape_game_ids(2018, weeks = 2)
week_2_games %>%
  pander::pander()

# Now generate the play-by-play dataset for the game:
format(Sys.time(), "%a %b %d %H:%M:%S %Y")

kc_vs_pit_pbp <- week_2_games %>%
  dplyr::filter(home_team == "PIT") %>%
  pull(game_id) %>%
  scrape_json_play_by_play()

format(Sys.time(), "%a %b %d %H:%M:%S %Y")

kc_vs_pit_pbp %>% 
  pander::pander()

data_structure <-
  as.data.frame(colnames(kc_vs_pit_pbp))



---

reg_pbp_2017 <- read_pbp_data("Data/PBP/reg_pbp_2017.csv")
timeout_reg_2017 <- timeout_126sec_4qtr(reg_pbp_2017)

post_pbp_2017 <- read_pbp_data("Data/PBP/post_pbp_2017.csv")
timeout_post_2017 <- timeout_126sec_4qtr(post_pbp_2017)

reg_pbp_2018 <- read_pbp_data("Data/PBP/reg_pbp_2018.csv")
timeout_reg_2018 <- timeout_126sec_4qtr(reg_pbp_2018)

plays <- plays_before_after(reg_pbp_2018, game = 2018090600, play = 37)

