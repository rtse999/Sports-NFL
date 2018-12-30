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
source(file = "read_pbp_data.r")
source(file = "timeout_126sec_4qtr.r")

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

read_pbp_data <- function(filename) {
                      read_csv(filename,
                         col_types = 
                           cols(
                             .default = col_character(),
                             play_id = col_double(),
                             game_id = col_factor(NULL),
                             home_team = col_factor(NULL),
                             away_team = col_factor(NULL),
                             posteam = col_factor(NULL),
                             posteam_type = col_factor(NULL),
                             defteam = col_factor(NULL),
                             side_of_field = col_factor(NULL),
                             yardline_100 = col_double(),
                             game_date = col_date(format = ""),
                             quarter_seconds_remaining = col_double(),
                             half_seconds_remaining = col_double(),
                             game_seconds_remaining = col_double(),
                             game_half = col_factor(NULL),
                             quarter_end = col_double(),
                             drive = col_double(),
                             sp = col_double(),
                             qtr = col_double(),
                             down = col_double(),
                             goal_to_go = col_double(),
                             time = col_time(format = ""),
                             yrdln = col_character(),
                             ydstogo = col_double(),
                             ydsnet = col_double(),
                             desc = col_character(),
                             play_type = col_factor(NULL),
                             yards_gained = col_double(),
                             shotgun = col_double(),
                             no_huddle = col_double(),
                             qb_dropback = col_double(),
                             qb_kneel = col_double(),
                             qb_spike = col_double(),
                             qb_scramble = col_double(),
                             pass_length = col_factor(NULL),
                             pass_location = col_factor(NULL),
                             air_yards = col_double(),
                             yards_after_catch = col_double(),
                             run_location = col_factor(NULL),
                             run_gap = col_factor(NULL),
                             field_goal_result = col_factor(NULL),
                             kick_distance = col_double(),
                             extra_point_result = col_factor(NULL),
                             two_point_conv_result = col_factor(NULL),
                             home_timeouts_remaining = col_double(),
                             away_timeouts_remaining = col_double(),
                             timeout = col_double(),
                             timeout_team = col_factor(NULL),
                             td_team = col_factor(NULL),
                             posteam_timeouts_remaining = col_double(),
                             defteam_timeouts_remaining = col_double(),
                             total_home_score = col_double(),
                             total_away_score = col_double(),
                             posteam_score = col_double(),
                             defteam_score = col_double(),
                             score_differential = col_double(),
                             posteam_score_post = col_double(),
                             defteam_score_post = col_double(),
                             score_differential_post = col_double(),
                             no_score_prob = col_double(),
                             opp_fg_prob = col_double(),
                             opp_safety_prob = col_double(),
                             opp_td_prob = col_double(),
                             fg_prob = col_double(),
                             safety_prob = col_double(),
                             td_prob = col_double(),
                             extra_point_prob = col_double(),
                             two_point_conversion_prob = col_double(),
                             ep = col_double(),
                             epa = col_double(),
                             total_home_epa = col_double(),
                             total_away_epa = col_double(),
                             total_home_rush_epa = col_double(),
                             total_away_rush_epa = col_double(),
                             total_home_pass_epa = col_double(),
                             total_away_pass_epa = col_double(),
                             air_epa = col_double(),
                             yac_epa = col_double(),
                             comp_air_epa = col_double(),
                             comp_yac_epa = col_double(),
                             total_home_comp_air_epa = col_double(),
                             total_away_comp_air_epa = col_double(),
                             total_home_comp_yac_epa = col_double(),
                             total_away_comp_yac_epa = col_double(),
                             total_home_raw_air_epa = col_double(),
                             total_away_raw_air_epa = col_double(),
                             total_home_raw_yac_epa = col_double(),
                             total_away_raw_yac_epa = col_double(),
                             wp = col_double(),
                             def_wp = col_double(),
                             home_wp = col_double(),
                             away_wp = col_double(),
                             wpa = col_double(),
                             home_wp_post = col_double(),
                             away_wp_post = col_double(),
                             total_home_rush_wpa = col_double(),
                             total_away_rush_wpa = col_double(),
                             total_home_pass_wpa = col_double(),
                             total_away_pass_wpa = col_double(),
                             air_wpa = col_double(),
                             yac_wpa = col_double(),
                             comp_air_wpa = col_double(),
                             comp_yac_wpa = col_double(),
                             total_home_comp_air_wpa = col_double(),
                             total_away_comp_air_wpa = col_double(),
                             total_home_comp_yac_wpa = col_double(),
                             total_away_comp_yac_wpa = col_double(),
                             total_home_raw_air_wpa = col_double(),
                             total_away_raw_air_wpa = col_double(),
                             total_home_raw_yac_wpa = col_double(),
                             total_away_raw_yac_wpa = col_double(),
                             punt_blocked = col_double(),
                             first_down_rush = col_double(),
                             first_down_pass = col_double(),
                             first_down_penalty = col_double(),
                             third_down_converted = col_double(),
                             third_down_failed = col_double(),
                             fourth_down_converted = col_double(),
                             fourth_down_failed = col_double(),
                             incomplete_pass = col_double(),
                             touchback = col_double(),
                             interception = col_double(),
                             punt_inside_twenty = col_double(),
                             punt_in_endzone = col_double(),
                             punt_out_of_bounds = col_double(),
                             punt_downed = col_double(),
                             punt_fair_catch = col_double(),
                             kickoff_inside_twenty = col_double(),
                             kickoff_in_endzone = col_double(),
                             kickoff_out_of_bounds = col_double(),
                             kickoff_downed = col_double(),
                             kickoff_fair_catch = col_double(),
                             fumble_forced = col_double(),
                             fumble_not_forced = col_double(),
                             fumble_out_of_bounds = col_double(),
                             solo_tackle = col_double(),
                             safety = col_double(),
                             penalty = col_double(),
                             tackled_for_loss = col_double(),
                             fumble_lost = col_double(),
                             own_kickoff_recovery = col_double(),
                             own_kickoff_recovery_td = col_double(),
                             qb_hit = col_double(),
                             rush_attempt = col_double(),
                             pass_attempt = col_double(),
                             sack = col_double(),
                             touchdown = col_double(),
                             pass_touchdown = col_double(),
                             rush_touchdown = col_double(),
                             return_touchdown = col_double(),
                             extra_point_attempt = col_double(),
                             two_point_attempt = col_double(),
                             field_goal_attempt = col_double(),
                             kickoff_attempt = col_double(),
                             punt_attempt = col_double(),
                             fumble = col_double(),
                             complete_pass = col_double(),
                             assist_tackle = col_double(),
                             lateral_reception = col_double(),
                             lateral_rush = col_double(),
                             lateral_return = col_double(),
                             lateral_recovery = col_double(),
                             passer_player_id = col_character(),
                             passer_player_name = col_character(),
                             receiver_player_id = col_character(),
                             receiver_player_name = col_character(),
                             rusher_player_id = col_character(),
                             rusher_player_name = col_character(),
                             lateral_receiver_player_id = col_character(),
                             lateral_receiver_player_name = col_character(),
                             lateral_rusher_player_id = col_character(),
                             lateral_rusher_player_name = col_character(),
                             lateral_sack_player_id = col_character(),
                             lateral_sack_player_name = col_character(),
                             interception_player_id = col_character(),
                             interception_player_name = col_character(),
                             lateral_interception_player_id = col_character(),
                             lateral_interception_player_name = col_character(),
                             punt_returner_player_id = col_character(),
                             punt_returner_player_name = col_character(),
                             lateral_punt_returner_player_id = col_character(),
                             lateral_punt_returner_player_name = col_character(),
                             kickoff_returner_player_name = col_character(),
                             kickoff_returner_player_id = col_character(),
                             lateral_kickoff_returner_player_id = col_character(),
                             lateral_kickoff_returner_player_name = col_character(),
                             punter_player_id = col_character(),
                             punter_player_name = col_character(),
                             kicker_player_name = col_character(),
                             kicker_player_id = col_character(),
                             own_kickoff_recovery_player_id = col_character(),
                             own_kickoff_recovery_player_name = col_character(),
                             blocked_player_id = col_character(),
                             blocked_player_name = col_character(),
                             tackle_for_loss_1_player_id = col_character(),
                             tackle_for_loss_1_player_name = col_character(),
                             tackle_for_loss_2_player_id = col_character(),
                             tackle_for_loss_2_player_name = col_character(),
                             qb_hit_1_player_id = col_character(),
                             qb_hit_1_player_name = col_character(),
                             qb_hit_2_player_id = col_character(),
                             qb_hit_2_player_name = col_character(),
                             forced_fumble_player_1_team = col_character(),
                             forced_fumble_player_1_player_id = col_character(),
                             forced_fumble_player_1_player_name = col_character(),
                             forced_fumble_player_2_team = col_character(),
                             forced_fumble_player_2_player_id = col_character(),
                             forced_fumble_player_2_player_name = col_character(),
                             solo_tackle_1_team = col_character(),
                             solo_tackle_2_team = col_character(),
                             solo_tackle_1_player_id = col_character(),
                             solo_tackle_2_player_id = col_character(),
                             solo_tackle_1_player_name = col_character(),
                             solo_tackle_2_player_name = col_character(),
                             assist_tackle_1_player_id = col_character(),
                             assist_tackle_1_player_name = col_character(),
                             assist_tackle_1_team = col_character(),
                             assist_tackle_2_player_id = col_character(),
                             assist_tackle_2_player_name = col_character(),
                             assist_tackle_2_team = col_character(),
                             assist_tackle_3_player_id = col_character(),
                             assist_tackle_3_player_name = col_character(),
                             assist_tackle_3_team = col_character(),
                             assist_tackle_4_player_id = col_character(),
                             assist_tackle_4_player_name = col_character(),
                             assist_tackle_4_team = col_character(),
                             pass_defense_1_player_id = col_character(),
                             pass_defense_1_player_name = col_character(),
                             pass_defense_2_player_id = col_character(),
                             pass_defense_2_player_name = col_character(),
                             fumbled_1_team = col_character(),
                             fumbled_1_player_id = col_character(),
                             fumbled_1_player_name = col_character(),
                             fumbled_2_player_id = col_character(),
                             fumbled_2_player_name = col_character(),
                             fumbled_2_team = col_character(),
                             fumble_recovery_1_team = col_character(),
                             fumble_recovery_1_yards = col_double(),
                             fumble_recovery_1_player_id = col_character(),
                             fumble_recovery_1_player_name = col_character(),
                             fumble_recovery_2_team = col_character(),
                             fumble_recovery_2_yards = col_double(),
                             fumble_recovery_2_player_id = col_character(),
                             fumble_recovery_2_player_name = col_character(),
                             return_team = col_character(),
                             return_yards = col_double(),
                             penalty_team = col_character(),
                             penalty_player_id = col_character(),
                             penalty_player_name = col_character(),
                             penalty_yards = col_double(),
                             replay_or_challenge = col_double(),
                             replay_or_challenge_result = col_character(),
                             penalty_type = col_character(),
                             defensive_two_point_attempt = col_double(),
                             defensive_two_point_conv = col_double(),
                             defensive_extra_point_attempt = col_double(),
                             defensive_extra_point_conv = col_double()
                           )
                        )
}

timeout_126sec_4qtr <- function (pbp_data) {
  pbp_data %>% 
    dplyr::filter(timeout == 1) %>% 
    dplyr::filter(game_seconds_remaining <= 126 & game_seconds_remaining > 120) %>% 
    dplyr::filter(timeout_team == defteam)
}

reg_pbp_2017 <- read_pbp_data("Data/PBP/reg_pbp_2017.csv")
timeout_reg_2017 <- timeout_126sec_4qtr(reg_pbp_2017)

post_pbp_2017 <- read_pbp_data("Data/PBP/post_pbp_2017.csv")
timeout_post_2017 <- timeout_126sec_4qtr(post_pbp_2017)

reg_pbp_2018 <- read_pbp_data("Data/PBP/reg_pbp_2018.csv")
timeout_reg_2018 <- timeout_126sec_4qtr(reg_pbp_2018)
