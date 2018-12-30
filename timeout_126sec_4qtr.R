# ------------------------------------------------------------------------
# Extract all plays where defensive team calls a timeout with 6 or less 
# seconds until the 4th quarter 2 minute warning
#
# Based on WHEN THEY CALL TIME WITH 2:05 ON THE CLOCK
#  Lombardi, Michael. Gridiron Genius (p. 204). Crown/Archetype. 
#  Kindle Edition. 
#
# Link: https://github.com/maksimhorowitz/nflscrapR
# Link: https://tlfvincent.github.io//2017/10/08/nlf-running-back-deep-dive/
#
# Location: /Users/raymondtse/Dropbox/Analysis/Sports/timeout_126sec_4qtr.r
# First created: 16:20 - Thursday 27 December 2018
# Last modified: 10:42 - Monday 31 December 2018
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
library(readr)
library(tidyverse)

# ------------------------------------------------------------------------
# Load functions
# ------------------------------------------------------------------------
source(file = "fn_read_pbp_data.r")
source(file = "fn_timeout_126sec_4qtr.r")
source(file = "fn_plays_before_after.r")

# ------------------------------------------------------------------------
# Read data
# ------------------------------------------------------------------------
reg_pbp_2017 <- read_pbp_data("Data/PBP/reg_pbp_2017.csv")
post_pbp_2017 <- read_pbp_data("Data/PBP/post_pbp_2017.csv")
reg_pbp_2018 <- read_pbp_data("Data/PBP/reg_pbp_2018.csv")

# ------------------------------------------------------------------------
# Create list of plays that meet criteria
# ------------------------------------------------------------------------
timeout_reg_2017 <- timeout_126sec_4qtr(reg_pbp_2017)
timeout_post_2017 <- timeout_126sec_4qtr(post_pbp_2017)
timeout_reg_2018 <- timeout_126sec_4qtr(reg_pbp_2018)

# ------------------------------------------------------------------------
# Create list of play and include play before and after time out
# ------------------------------------------------------------------------
plays <- plays_before_after(reg_pbp_2017, game = 2017120302, play = 4294)

