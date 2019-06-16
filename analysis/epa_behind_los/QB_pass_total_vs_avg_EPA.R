# ------------------------------------------------------------------------
# Average vs Total EPA for passing plays 2018
#
# Link: https://twitter.com/JayCromwell12/status/1139294494036242432
# Link: https://twitter.com/rtse999/status/1140074416094642178
#
# Location: /Users/raymondtse/Dropbox/Analysis/Sports/NFL/analysis/epa_nehind_los/QB_pass_total_vs_avg_EPA.r
# First created: 16:24 - Saturday 15 June 2019
# Last modified: 16:24 - Saturday 15 June 2019
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# System time
# ------------------------------------------------------------------------
format(Sys.time(), "%a %b %d %H:%M:%S %Y")

# ------------------------------------------------------------------------
# Install Packages
# ------------------------------------------------------------------------
library(conflicted)
library(ggrepel)
library(skimr)
library(tidyverse)

# ------------------------------------------------------------------------
# Load functions
# ------------------------------------------------------------------------
source(file = "/Users/raymondtse/Dropbox/Analysis/Sports/NFL/src/functions_pbp/fn_read_pbp_data.r")

# ------------------------------------------------------------------------
# Read data
# ------------------------------------------------------------------------
reg_pbp_2018 <- read_pbp_data("/Users/raymondtse/Dropbox/Analysis/github packages/nflscrapR-data/play_by_play_data/regular_season/reg_pbp_2018.csv")

# ------------------------------------------------------------------------
# Create data set
# ------------------------------------------------------------------------
# Refactor to function
non_penalty_plays <- 
  reg_pbp_2018 %>% 
  dplyr::filter(penalty == 0) %>% 
  dplyr::filter(!is.na(play_type))

levels(non_penalty_plays$play_type)

# Select only passes
passes <-
  non_penalty_plays %>%
  dplyr::filter(play_type == "pass")
  
# Calculate total and average EPA by QB
qb_epa <- 
  passes %>% 
  group_by(passer_player_name) %>% 
  summarise(
    n_play = n(),
    total_epa = sum(epa, na.rm = TRUE),
    avg_epa = mean(epa, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  arrange(-total_epa) %>% 
  dplyr::filter(n_play > 50) 

# Calculate total and average EPA by QB for completed passes only
qb_completions_epa <- 
  passes %>% 
  dplyr::filter(complete_pass == 1) %>% 
  group_by(passer_player_name) %>% 
  summarise(
    n_play = n(),
    total_epa = sum(epa, na.rm = TRUE),
    avg_epa = mean(epa, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  arrange(-total_epa) %>% 
  dplyr::filter(n_play > 50) 

qb_completions_epa %>% 
  ggplot() +
  geom_point(aes(x = avg_epa, y = total_epa))

# Calculate total and average EPA by QB for passes up to line of scrimmage
qb_los_epa <- 
  passes %>% 
  dplyr::filter(air_yards <= 0) %>% 
  group_by(passer_player_name) %>% 
  summarise(
    n_play = n(),
    completions = sum(complete_pass),
    completion_pct = completions / n_play,
    total_epa = sum(epa, na.rm = TRUE),
    avg_epa = mean(epa, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  arrange(-total_epa) %>% 
  dplyr::filter(n_play > 50)

qb_los_epa_labels <-
  qb_los_epa %>% 
  slice(1:5, (nrow(qb_los_epa)-3):nrow(qb_los_epa))

qb_los_epa %>% 
  ggplot() +
  geom_vline(xintercept = 0, colour = "white", size = 3) +
  geom_hline(yintercept = 0, colour = "white", size = 3) +
  geom_point(aes(x = avg_epa, y = total_epa)) +
  geom_text_repel(aes(label = passer_player_name, x = avg_epa, 
                                y = total_epa),
            data = qb_los_epa_labels,
            nudge_y = 3, alpha = 0.5) +
  labs(
    title = paste("Attempted passes at or behind LOS by QB - 2018"),
    x = "Average EPA",
    y = "Total EPA"
  )

# Total attempts behind LOS 
sum(qb_los_epa$n_play)

# Total completions behind LOS 
sum(qb_los_epa$completions)

# Completion rate behind LOS 
sum(qb_los_epa$completions) / sum(qb_los_epa$n_play)


# Calculate total and average EPA by Andrew Luck --------------------------
andrew_luck_los_epa <- 
  passes %>% 
  dplyr::filter(air_yards <= 0, passer_player_name == "A.Luck")

andrew_luck_los_epa %>% 
  ggplot() +
  geom_histogram(aes(x = epa), bins = 10) +
  geom_vline(xintercept = 0, colour = "white", size = 1) +
  labs(
    title = paste("Attempted passes at or behind LOS by A.Luck - 2018"),
    x = "EPA",
    y = "Attempts"
  )

andrew_luck_los_epa %>% 
  ggplot() +
  geom_histogram(aes(x = yards_gained), bins = 10) +
  geom_vline(xintercept = 0, colour = "white", size = 1) +
  labs(
    title = paste("Yards gained on passes at or behind LOS by A.Luck - 2018"),
    x = "Yards Gained",
    y = "Attempts"
  )

# Average EPA for Andrew Luck on passes behind the LOS is ...
mean(andrew_luck_los_epa$epa, na.rm = TRUE)

# Why does play_id == 4247 have epa == NA when it shouldn't ?
err <- dplyr::filter(andrew_luck_los_epa, is.na(epa))



# Calculate total and average EPA by Patrick Mahomes --------------------------
patrick_mahomes_los_epa <- 
  passes %>% 
  dplyr::filter(air_yards <= 0, passer_player_name == "P.Mahomes")

patrick_mahomes_los_epa %>% 
  ggplot() +
  geom_histogram(aes(x = epa), bins = 10) +
  geom_vline(xintercept = 0, colour = "white", size = 1) +
  labs(
    title = paste("Attempted passes at or behind LOS by P.Mahomes - 2018"),
    x = "EPA",
    y = "Attempts"
  )

patrick_mahomes_los_epa %>% 
  ggplot() +
  geom_histogram(aes(x = yards_gained), bins = 10) +
  geom_vline(xintercept = 0, colour = "white", size = 1) +
  labs(
    title = paste("Yards gained on passes at or behind LOS by P.Mahomes - 2018"),
    x = "Yards Gained",
    y = "Attempts"
  )


# Calculate total and average EPA by QB for completed passes up to --------
qb_completions_los_epa <- 
  passes %>% 
  dplyr::filter(complete_pass == 1) %>% 
  dplyr::filter(air_yards <= 0) %>% 
  group_by(passer_player_name) %>% 
  summarise(
    n_play = n(),
    total_epa = sum(epa, na.rm = TRUE),
    avg_epa = mean(epa, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  arrange(-total_epa) %>% 
  dplyr::filter(n_play > 50) 

qb_completions_los_epa %>% 
  ggplot() +
  geom_point(aes(x = avg_epa, y = total_epa))
