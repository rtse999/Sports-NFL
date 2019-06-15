# ------------------------------------------------------------------------
# Expected EPA by down, yards to go and play type
#
# Link: 
#
# Location: /Users/raymondtse/Dropbox/Analysis/Sports/NFL/analysis/epa_playcall_vs_execution/epa_playcall_vs_execution.r
# First created: 13:03 - Sunday 5 May 2019
# Last modified: 13:03 - Sunday 5 May 2019
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
library(tidyverse)

# ------------------------------------------------------------------------
# Load functions
# ------------------------------------------------------------------------
source(file = "/Users/raymondtse/Dropbox/Analysis/Sports/NFL/src/functions_pbp/fn_read_pbp_data.r")

# ------------------------------------------------------------------------
# Read data
# ------------------------------------------------------------------------
reg_pbp_2018 <- read_pbp_data("Data/PBP/reg_pbp_2018.csv")

# ------------------------------------------------------------------------
# Create data set
# ------------------------------------------------------------------------
# Refactor to function
non_penalty_plays <- 
  reg_pbp_2018 %>% 
  dplyr::filter(penalty == 0) %>% 
  dplyr::filter(!is.na(play_type))

levels(non_penalty_plays$play_type)

# Filter out kickoffs, no_plays, qb_kneels, extra_points, qb_spikes
plays <-
  non_penalty_plays %>%
  dplyr::filter(play_type != "kickoff") %>%
  dplyr::filter(play_type != "no_play") %>%
  dplyr::filter(play_type != "qb_kneel") %>%
  dplyr::filter(play_type != "extra_point") %>% 
  dplyr::filter(play_type != "qb_spike")
  
# Calculate average EPA by down, yards to go and play type
# Add field position
plays_epa <- 
  plays %>% 
  group_by(down, ydstogo, play_type) %>% 
  summarise(
    n_play = n(),
    epa = mean(epa, na.rm = TRUE)
  ) %>% 
  ungroup()

# Reconcile with previous 4th & 1 analysis
plays %>% 
  dplyr::filter(down == 4, ydstogo == 1, play_type %in% c("pass", "run", "punt",
                                               "field_goal")) %>% 
  ggplot() +
  geom_vline(xintercept = 0, color = "white", size = 1) +
  geom_freqpoly(aes(x = epa), binwidth = 0.5) +
  ggtitle("EPA on 4th&1 plays - 2018 regular season") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~ play_type)  
  

