# ------------------------------------------------------------------------
# Calculate the EPA of 4th down and NOT punt/FG compared to punt/FG
#
# Link: https://twitter.com/InsideTheBlock/status/1084582407204933633
#
# It's a small sample size, but would be interesting to run "EPA on 
# non-punting 4th downs" through @nflscrapR. Would essentially be a coach's
# EPA. Has anyone done this @friscojosh @Moo12152
#
# Location: /Users/raymondtse/Dropbox/Analysis/Sports/NFL/4thdown_coaches_EPA.r
# First created: 17:44 - Wednesday 16 January 2019
# Last modified: 17:44 - Wednesday 16 January 2019
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
source(file = "/Users/raymondtse/Dropbox/Analysis/Sports/NFL/src/functions_pbp/fn_read_pbp_data.r")

# ------------------------------------------------------------------------
# Read data
# ------------------------------------------------------------------------
# reg_pbp_2017 <- read_pbp_data("Data/PBP/reg_pbp_2017.csv")
# post_pbp_2017 <- read_pbp_data("Data/PBP/post_pbp_2017.csv")
reg_pbp_2018 <- read_pbp_data("Data/PBP/reg_pbp_2018.csv")

# ------------------------------------------------------------------------
# Create data set
# ------------------------------------------------------------------------
fourth_down_plays <- 
  reg_pbp_2018 %>% 
  dplyr::filter(down == 4, penalty == 0)

skim(fourth_down_plays$play_type)

# ------------------------------------------------------------------------
# Summarise data
# ------------------------------------------------------------------------
fourth_down_epa <- 
  fourth_down_plays %>% 
  group_by(ydstogo, play_type) %>% 
  summarise(
    n_play = n(),
    epa = mean(epa, na.rm = TRUE)
  ) %>% 
  dplyr::filter(!is.na(play_type)) %>% 
  ungroup()

fourth_down_n_matrix <-
  fourth_down_epa %>% 
  select(-epa) %>% 
  spread(play_type, n_play)

fourth_down_epa_matrix <-
  fourth_down_epa %>% 
  select(-n_play) %>% 
  spread(play_type, epa)

# ------------------------------------------------------------------------
# 4th & 1 plays
# ------------------------------------------------------------------------
fourth_down_plays %>% 
  dplyr::filter(ydstogo == 1, play_type %in% c("pass", "run", "punt",
                                               "field_goal")) %>% 
  ggplot() +
  geom_vline(xintercept = 0, color = "white", size = 1) +
  geom_freqpoly(aes(x = epa), binwidth = 0.5) +
  ggtitle("EPA on 4th&1 plays - 2018 regular season") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~ play_type)

# ------------------------------------------------------------------------
# What are the plays with a NA epa ?
# ------------------------------------------------------------------------
na.epa <- 
  fourth_down_plays %>% 
  dplyr::filter(is.na(epa))

(na.epa$desc)

# ------------------------------------------------------------------------
# What are the plays with a "NA epa ?"no_play" play type ?
# ------------------------------------------------------------------------
no_play <-
  fourth_down_plays %>% 
  dplyr::filter(play_type == "no_play")

(no_play$desc)

# ------------------------------------------------------------------------
# What are the plays with a "NA epa ?"no_play" play type ?
# ------------------------------------------------------------------------

