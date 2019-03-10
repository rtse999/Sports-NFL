# ------------------------------------------------------------------------
# Analysis of NFL QBs using Fast Nearest Neighbor Search Algorithms
#
# Link: 
#
# Location: /Users/raymondtse/Dropbox/Analysis/Sports/NFL/analysis/NFLcom_QB_FNN
# First created: 21:54 - Sunday 10 March 2019
# Last modified: 21:54 - Sunday 10 March 2019
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# System time
# ------------------------------------------------------------------------
format(Sys.time(), "%a %b %d %H:%M:%S %Y")

# ------------------------------------------------------------------------
# Install Packages
# ------------------------------------------------------------------------
library(conflicted)
library(FNN)
library(skimr)
library(tidyverse)

# ------------------------------------------------------------------------
# Load functions
# ------------------------------------------------------------------------
source(file = "src/functions_NFLcom_QBs/fn_load_QB_data.r")

# ------------------------------------------------------------------------
# Read Data 
# ------------------------------------------------------------------------
afc_regseason_qbs <- load_QB_data("data/NFLcom_QB/AFC_RegSeason_QBs.csv")

# ------------------------------------------------------------------------
# Create new features
# ------------------------------------------------------------------------
afc_regseason_qbs <- 
  afc_regseason_qbs %>% 
  mutate(
    games_played = ifelse(`Att/G` > 0, round(Att / `Att/G`, 0), 0),
    td_game = ifelse(games_played > 0, TD / games_played, 0),
    int_game = ifelse(games_played > 0, Int / games_played, 0),
    first_down_game = ifelse(games_played > 0, `1st` / games_played, 0),
    twenty_yds_game = ifelse(games_played > 0, `20+` / games_played, 0),
    forty_yds_game = ifelse(games_played > 0, `40+` / games_played, 0),
    sacks_game = ifelse(games_played > 0, Sck / games_played, 0)
  )

# ------------------------------------------------------------------------
# Prep FNN data
# ------------------------------------------------------------------------
afc_qb_data <- 
  afc_regseason_qbs %>% 
  select(Comp:`1st%`, `20+`:sacks_game)

# Replace all NAs with zeroes
afc_qb_data[is.na(afc_qb_data)] <- 0

# Scale values
afc_qb_data <- scale(afc_qb_data)

# ------------------------------------------------------------------------
# FNN
# ------------------------------------------------------------------------
get.knn(afc_qb_data, k=3)

# ------------------------------------------------------------------------
# Samples
# ------------------------------------------------------------------------
pmahomes2018 <-
  afc_regseason_qbs %>% 
  slice(2, 1, 323, 219)

pbrady2018 <-
  afc_regseason_qbs %>% 
  slice(7, 361, 401, 897)

pmahomes2018_scaled <-
  afc_regseason_qbs %>% 
  slice(2, 478, 591, 323)

pbrady2018_scaled <-
  afc_regseason_qbs %>% 
  slice(7, 627, 225, 480)

