# ------------------------------------------------------------------------
# Analysis of NFL QBs using Fast Nearest Neighbor Search Algorithms
#
# Link: 
#
# Location: /Users/raymondtse/Dropbox/Analysis/Sports/NFL/analysis/NFLcom_QB_FNN
# First created: 21:54 - Sunday 10 March 2019
# Last modified: 10:19 - Tuesday 12 March 2019
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
library(scales)
library(skimr)
library(tidyverse)

# ------------------------------------------------------------------------
# Load functions
# ------------------------------------------------------------------------
source(file = "src/functions_NFLcom_QBs/fn_load_QB_data.r")

format_comparison <- function(df) {
  df %>% 
    select(Player, Season, Team : Pos, games_played, Att,
           Comp, Pct : TD, td_game, Int, int_game, `1st`, 
           first_down_game, `20+`, twenty_yds_game, `40+`,
           forty_yds_game, Sck, sacks_game, Rate) %>% 
    rename("Position" = "Pos", "Games" = "games_played",
           "Attempts" = "Att", "Completions" = "Comp",
           "Completion %" = "Pct", "Attempts/Game" = "Att/G",
           "Total Yards" = "Yds", "Avg Yards" = "Avg", 
           "Yards/Game" = "Yds/G", "Touchdowns" = "TD",
           "TD/Game" = "td_game", "Interceptions" = "Int",
           "Int/Game" = "int_game", "1st Downs" = "1st",
           "1st Downs/Game" = "first_down_game",
           "20+ Yard Plays" = "20+",
           "20+ Yd Plays/Game" = "twenty_yds_game",
           "40+ Yard Plays" = "40+",
           "40+ Yd Plays/Game" = "forty_yds_game",
           "Sacks" = "Sck", "Sacks/Game" = "sacks_game",
           "QBR" = "Rate") %>% 
    mutate(`Attempts` = comma(`Attempts`),
           `Completions` = comma(`Completions`),
           `Total Yards` = comma(`Total Yards`), 
           `TD/Game` = round(`TD/Game`, 1),
           `Int/Game` = round(`Int/Game`, 2),
           `1st Downs/Game` = round(`1st Downs/Game`, 1),
           `20+ Yd Plays/Game` = round(`20+ Yd Plays/Game`, 1),
           `40+ Yd Plays/Game` = round(`40+ Yd Plays/Game`, 1),
           `Sacks/Game` = round(`Sacks/Game`, 1)) %>% 
    t() %>% 
    as.data.frame() %>% 
    set_names(~ str_to_lower(.) %>% 
                str_replace_all("v", "Player ")) %>% 
    knitr::kable(format = "html") %>% 
    kable_styling("striped", full_width = F)
}

# How do you centre justify the value columns ?

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

# Add an aveage QB record (all zeroes)
new <- as.data.frame(t(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)))
colnames(new) <- colnames(qb_data)
qb_data <- as.data.frame(qb_data)
qb_data <- bind_rows(qb_data, new)

# ------------------------------------------------------------------------
# FNN
# ------------------------------------------------------------------------
get.knn(qb_data, k=4)
nearest <- get.knn(qb_data, k=4)$nn.index


# ------------------------------------------------------------------------
# Samples
# ------------------------------------------------------------------------
pmahomes2018_scaled <-
  regseason_qbs %>% 
  slice(2, 478, 323, 591)

tbrady2018_scaled <-
  regseason_qbs %>% 
  slice(7, 925, 1462, 627)

format_comparison(tbrady2018_scaled)

rwilson2015_scaled <-
  regseason_qbs %>% 
  slice(927, 1023, 1056, 928)

ckeenum2017_scaled <-
  regseason_qbs %>% 
  slice(862, 442, 1280, 899)

format_comparison(ckeenum2017_scaled)

pmanning2015_scaled <-
  regseason_qbs %>% 
  slice(191, 1471, 525, 1512)

format_comparison(pmanning2015_scaled)

pmanning2013_scaled <-
  regseason_qbs %>% 
  slice(256, 1052, 478, 2)

format_comparison(pmanning2013_scaled)

sdarnold2018_scaled <-
  regseason_qbs %>% 
  slice(23, 1283, 787, 1396)

format_comparison(sdarnold2018_scaled)

msanchez2010_scaled <-
  regseason_qbs %>% 
  slice(367, 330, 560, 1431)

format_comparison(msanchez2010_scaled)

msanchez2011_scaled <-
  regseason_qbs %>% 
  slice(329, 631, 713, 866)

format_comparison(msanchez2011_scaled)

average_scaled <-
  regseason_qbs %>% 
  slice(1540, 1180, 196, 493)

format_comparison(average_scaled)
