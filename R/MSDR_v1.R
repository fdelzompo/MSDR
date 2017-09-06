require(tidyverse)
require(lubridate)
require(stringr)

# After downloading and reading in the dataset, the overall task for this module is to write a function
# named eq_clean_data()that takes raw NOAA data frame and returns a clean data frame.
# The clean data frame should have the following:
#
#  - A date column created by uniting the year, month, day and converting it to the Date class
#  - LATITUDE and LONGITUDE columns converted to numeric class

raw_data <- read_tsv('results')

eq_clean_data <- function(raw_data){
  raw_data <- raw_data %>%
    mutate(
      YEAR = as.numeric(YEAR),
      MONTH = ifelse(is.na(MONTH) == TRUE, 01,MONTH),
      DAY = ifelse(is.na(DAY) == TRUE, 01,DAY),
      date = if_else(raw_data$YEAR > 0,
                     as.Date(ISOdate(abs(YEAR),MONTH,DAY)),
                     as.Date(as.numeric(ISOdate(0,1,1)-ISOdate(abs(YEAR),MONTH,DAY)), origin = '0000-01-01'))
    )
  return(raw_data)
}

#  - In addition, write a function eq_location_clean() that cleans the LOCATION_NAME column by stripping out
#    the country name (including the colon) and converts names to title case (as opposed to all caps).
#    This will be needed later for annotating visualizations. This function should be applied to the raw data to produce a
#    cleaned up version of the LOCATION_NAME column.
eq_location_clean <- function(raw_data){
  raw_data$LOCATION_NAME <- raw_data$LOCATION_NAME %>% str_extract('(?<=:\\s).*') %>% str_to_title()
  return(raw_data)
}

