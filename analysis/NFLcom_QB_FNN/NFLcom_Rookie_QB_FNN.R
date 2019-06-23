
# Analysis of Rookie NFL QBs using Fast Nearest Neighbor Search Al --------
#
# Rookies defined as first season with 10+ games 
#
# Link: 
#
# Location: /Users/raymondtse/Dropbox/Analysis/Sports/NFL/analysis/NFLcom_Rookie_QB_FNN
# First created: 16:03 - Saturday 22 June 2019
# Last modified: 16:03 - Saturday 22 June 2019


# System time -------------------------------------------------------------
format(Sys.time(), "%a %b %d %H:%M:%S %Y")


# Install Packages --------------------------------------------------------
library(conflicted)
library(FNN)
library(kableExtra)
library(scales)
library(skimr)
library(tidyverse)


# Load functions ----------------------------------------------------------
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


# Read data ---------------------------------------------------------------
afc_regseason_qbs <- load_QB_data("data/NFLcom_QB/AFC_RegSeason_QBs.csv")
nfc_regseason_qbs <- load_QB_data("data/NFLcom_QB/NFC_1999-2018_RegSeason_QBs.csv")

regseason_qbs <- bind_rows(afc_regseason_qbs, nfc_regseason_qbs)

regseason_qbs <- 
  regseason_qbs %>% 
  dplyr::filter(Season >= 1980)


# Create new features -----------------------------------------------------
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


# Select rookie seasons ---------------------------------------------------
rookie_seasons <- 
  regseason_qbs %>%
  dplyr::filter(games_played >= 8) %>%
  group_by(Player) %>%
  arrange(Season) %>%
  dplyr::filter(row_number() == 1) %>%
  ungroup()


# Prep FNN data -----------------------------------------------------------
qb_data <- 
  rookie_seasons %>% 
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


# FNN ---------------------------------------------------------------------
get.knn(qb_data, k=4)
nearest <- get.knn(qb_data, k=4)$nn.index


# Samples -----------------------------------------------------------------
msanchez_scaled <-
  rookie_seasons %>% 
  slice(262, 153, 207, 188)

format_comparison(msanchez_scaled)

sdarnold_scaled <-
  rookie_seasons %>% 
  slice(322, 158, 177, 176)

format_comparison(sdarnold_scaled)



