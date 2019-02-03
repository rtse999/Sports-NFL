# ------------------------------------------------------------------------
# Load and stack 2014 - 2018 regular season data
#
# Link: https://github.com/maksimhorowitz/nflscrapR
# Link: https://github.com/ryurko/nflscrapR-data
#
# Location: /Users/raymondtse/Dropbox/Analysis/Sports/load_pbp_data.r
# First created: 14:27 - Sunday 3 February 2019
# Last modified: 14:27 - Sunday 3 February 2019
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# System time
# ------------------------------------------------------------------------
format(Sys.time(), "%a %b %d %H:%M:%S %Y")

# ------------------------------------------------------------------------
# Install Packages
# ------------------------------------------------------------------------
library(conflicted)
library(tidyverse)

# ------------------------------------------------------------------------
# Read data
# ------------------------------------------------------------------------
format(Sys.time(), "%a %b %d %H:%M:%S %Y")

reg_pbp_2014 <- read_pbp_data("Data/PBP/reg_pbp_2014.csv")
reg_pbp_2015 <- read_pbp_data("Data/PBP/reg_pbp_2015.csv")
reg_pbp_2016 <- read_pbp_data("Data/PBP/reg_pbp_2016.csv")
reg_pbp_2017 <- read_pbp_data("Data/PBP/reg_pbp_2017.csv")
reg_pbp_2018 <- read_pbp_data("Data/PBP/reg_pbp_2018.csv")

format(Sys.time(), "%a %b %d %H:%M:%S %Y")

reg_pbp_data <- bind_rows(reg_pbp_2014, reg_pbp_2015, reg_pbp_2016, reg_pbp_2017, reg_pbp_2018)

