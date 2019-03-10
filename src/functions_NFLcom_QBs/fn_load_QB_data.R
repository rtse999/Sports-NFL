# ------------------------------------------------------------------------
# Load QB Data sourced from NFL.com
#
# Link: http://www.nfl.com/stats/categorystats?tabSeq=1&season=2017&seasonType=REG&experience=&Submit=Go&archive=true&conference=null&d-447263-p=1&statisticPositionCategory=QUARTERBACK&qualified=true
# Data Definitions: http://www.espn.com.au/nfl/news/story?id=2128923
#
# Location: /Users/raymondtse/Dropbox/Analysis/Sports/NFL/src/functions_NFLcom_QBs
# First created: 21:45 - Sunday 10 March 2019
# Last modified: 21:45 - Sunday 10 March 2019
# ------------------------------------------------------------------------

load_QB_data <- function(filename) {
  read_csv(filename,
           col_types = 
             cols(
               .default = col_double(),
               Player = col_character(),
               Team = col_character(),
               Yds = col_number(),
               Pos = col_character(),
               Lng = col_character()
             )
  )
}