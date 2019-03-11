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
library(kableExtra)
library(skimr)
library(tidyverse)

# ------------------------------------------------------------------------
# Load functions
# ------------------------------------------------------------------------
source(file = "src/functions_NFLcom_QBs/fn_load_QB_data.r")

format_comparison <- function(df) {
  df %>% 
    select(Player:Season, Team:`1st%`, `20+`:sacks_game) %>% 
    t() %>% 
    as.data.frame() %>% 
    knitr::kable(format = "html") %>% 
    kable_styling("striped", full_width = F)
}

# ------------------------------------------------------------------------
# Read Data 
# ------------------------------------------------------------------------
afc_regseason_qbs <- load_QB_data("data/NFLcom_QB/AFC_RegSeason_QBs.csv")
nfc_regseason_qbs <- load_QB_data("data/NFLcom_QB/NFC_1999-2018_RegSeason_QBs.csv")

regseason_qbs <- bind_rows(afc_regseason_qbs, nfc_regseason_qbs)

regseason_qbs <- 
  regseason_qbs %>% 
  dplyr::filter(Season >= 1999)

# ------------------------------------------------------------------------
# Create new features
# ------------------------------------------------------------------------
regseason_qbs <- 
  regseason_qbs %>% 
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
qb_data <- 
  regseason_qbs %>% 
  select(Comp:`1st%`, `20+`:sacks_game)

# Replace all NAs with zeroes
qb_data[is.na(qb_data)] <- 0

# Scale values
qb_data <- scale(qb_data)

# ------------------------------------------------------------------------
# FNN
# ------------------------------------------------------------------------
get.knn(qb_data, k=3)
nearest <- get.knn(qb_data, k=4)$nn.index


# ------------------------------------------------------------------------
# Samples
# ------------------------------------------------------------------------
pmahomes2018_scaled <-
  regseason_qbs %>% 
  slice(2, 478, 323, 591)

pbrady2018_scaled <-
  regseason_qbs %>% 
  slice(7, 925, 1462, 627)

rwilson2015_scaled <-
  regseason_qbs %>% 
  slice(927, 1023, 1056, 928)

ckeenum2017_scaled <-
  regseason_qbs %>% 
  slice(862, 442, 1280, 899)

pmanning2015_scaled <-
  regseason_qbs %>% 
  slice(191, 1471, 525, 1512)

pmanning2013_scaled <-
  regseason_qbs %>% 
  slice(256, 1052, 478, 2)

format_comparison(pmanning2013_scaled)

