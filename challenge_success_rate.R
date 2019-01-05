# ------------------------------------------------------------------------
# Calculate the success rate of NFL coaches challenges
#
# Link: https://en.wikipedia.org/wiki/Replay_review_in_gridiron_football
# From Wikipedia: During a scoring play (starting in 2011), turnovers 
#  (starting in 2012), or after the two-minute warning of each half, and 
#  in overtime, reviews can only take place if the replay assistant, who
#  sits in the press box and monitors the network broadcast of the game,
#  determines that a play needs review; coaches may not challenge during
#  these times. In those cases, the replay assistant will contact the 
#  referee by a specialized electronic pager with a vibrating alert. 
#  If a review takes place during that time while the clock is running, 
#  the clock will stop for the review, and then it will start running 
#  once the ball is set and ready for play. Starting with the 2010 season,
#  any reviews with the clock running inside one minute will have a 10-second
#  rundown, which can be voided if either team uses a timeout.
#
# Location:
# First created: 
# Last modified: 13:25 - Saturday 5 January 2019
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
source(file = "fn_read_pbp_data.r")

# ------------------------------------------------------------------------
# Read data
# ------------------------------------------------------------------------
reg_pbp_2017 <- read_pbp_data("Data/PBP/reg_pbp_2017.csv")
post_pbp_2017 <- read_pbp_data("Data/PBP/post_pbp_2017.csv")
reg_pbp_2018 <- read_pbp_data("Data/PBP/reg_pbp_2018.csv")

# ------------------------------------------------------------------------
# Filter challenge plays
#  - NOT a scoring play
#  - NOT a turnover
#  - NOT after 2 min warning of each half
#  - NOT over-time
# ------------------------------------------------------------------------
coaches_challenge <- 
  post_pbp_2017 %>% 
  dplyr::filter(
    replay_or_challenge != 0
                )

successful_coaches_challenge <- 
  post_pbp_2017 %>% 
  dplyr::filter(
    replay_or_challenge_result != "upheld"
  )


# 249 reg season 2017
# 22 post season 2017
# Total 271 total season 2017

#half_seconds_remaining > 120,
#game_half != "Overtime",
#grepl("TOUCHDOWN", desc) == 0

reg_pbp_2017 %>% 
  dplyr::filter(grepl("Replay Official", desc))

nrow(coaches_challenge)
sum(coaches_challenge$replay_or_challenge_result == "upheld")

# ------------------------------------------------------------------------
# Are coaches challenges in the pbp data set ?
#
# Link: 2x Ravens challenges early in 21/10 Saints @ Ravens game
#         by Ravens
#
# ------------------------------------------------------------------------

ravens_saints_20181021 <-
  reg_pbp_2018 %>% 
  dplyr::filter(game_date == "2018-10-21" & home_team == "BAL")

replays_challenges <- 
  ravens_saints_20181021 %>% 
  dplyr::filter(replay_or_challenge == 1)

# Data set only records one challenge - Play 957
#
# [1] "(13:42) (Shotgun) D.Brees sacked at NO 37 for -9 yards (Z.Smith). 
# FUMBLES (Z.Smith) [Z.Smith], RECOVERED by BAL-A.Levine Sr. at BAL 49. 
# A.Levine Sr. pushed ob at NO 15 for 36 yards (R.Ramczyk). The Replay 
# Official reviewed the fumble ruling, and the play was REVERSED. 
# (Shotgun) D.Brees pass incomplete short right to T.Smith [Z.Smith]."
#
# This was a scoring play which is automatically reviewed
#

ravens_coaches_challenges <- 
  ravens_saints_20181021 %>% 
  dplyr::filter(grepl("Baltimore challenged", desc) == 1)

#
# Play 201: Challenge by Baltimore at 11:32 remaining in first quarter 
#
# [1] "(11:32) (Shotgun) D.Brees pass short middle to B.Watson to NO 49 for
# 7 yards (B.Carr). Baltimore challenged the first down ruling, and the play 
# was REVERSED. (Shotgun) D.Brees pass short middle to B.Watson to NO 47 for 
# 5 yards (B.Carr)."
#
# pbp data records neither a timeout (as the challenge was upheld) or a challenge 
#

#
# Play 475: Challenge by Baltimore at 7:01 remaining in first quarter 
#
# [2] "(7:01) A.Kamara left guard to BAL 13 for 8 yards (E.Weddle, C.Mosley).
# Baltimore challenged the runner was down by contact ruling, and the play was
# Upheld. The ruling on the field stands. (Timeout #1.)"
#
# pbp data records it as a Baltimore timeout but not a challenge as the 
#  challenge wasn't upheld
#




