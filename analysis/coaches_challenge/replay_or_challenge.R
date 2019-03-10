# ------------------------------------------------------------------------
# Review the code for the replay_or_challenge variable in nflscrapr
#
# Link: https://github.com/maksimhorowitz/nflscrapR
# Link: https://github.com/rtse999/nflscrapR
#
# Location: /Users/raymondtse/Dropbox/Analysis/github packages/nflscrapr/replay_or_challenge.r
# First created: 14:09 - Sunday 6 January 2019
# Last modified: 21:19 - Sunday 6 January 2019
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
source(file = "fn_read_pbp_data.r")

# ------------------------------------------------------------------------
# Read data
# ------------------------------------------------------------------------
reg_pbp_2017 <- read_pbp_data("Data/PBP/reg_pbp_2017.csv")

# ------------------------------------------------------------------------
# scrape_play_by_play code for replay_or_challenge
# ------------------------------------------------------------------------
# Replicate current logic
stringr::str_detect(reg_pbp_2017$desc, 
                    "(Replay Official reviewed)|( challenge )") %>%
  as.numeric() %>% 
  sum()

# Revised logic
#
# Pull request submitted: 21:11 - Sunday 6 January 2019
# https://github.com/maksimhorowitz/nflscrapR/pull/108
#
stringr::str_detect(reg_pbp_2017$desc, 
                    "(Replay Official reviewed)|( challenged )") %>%
  as.numeric() %>% 
  sum()

# ------------------------------------------------------------------------
# Test cases
# ------------------------------------------------------------------------
tmp <- stringr::str_detect(reg_pbp_2017$desc, 
                           "(Replay Official reviewed)|( challenged )") %>%
  as.numeric()

tmp <- as.data.frame(tmp)

summary(!is.na(stringr::str_extract(tolower(reg_pbp_2017$desc), 
                                    "( upheld)|( reversed)|( confirmed)")))

summary(!is.na(stringr::str_extract(tolower(reg_pbp_2017$desc), 
                                    "( reversed)")))
