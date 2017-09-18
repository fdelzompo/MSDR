require(testthat)
require(readr)
library(MSDR)

data <- readr::read_delim("earthquakes.tsv.gz", delim = "\t")


testthat::test_that("eq_clean_data returns a data frame", {
  testthat::expect_is(eq_clean_data(data), "data.frame")
})


testthat::test_that("eq_location_clean returns character", {
  testthat::expect_is(eq_location_clean(data)$LOCATION_NAME, "character")
})


testthat::test_that("geom_timeline returns ggplot object", {
  testthat::expect_is(data %>% eq_clean_data() %>%
              filter(COUNTRY %in% c("USA", "CHINA")) %>%
              ggplot(aes(x = DATE, y = COUNTRY, alpha = TOTAL_DEATHS, size = EQ_PRIMARY))+
              geom_timeline(xmindate = "2008/01/01", xmaxdate = "2015/12/31"),
            "ggplot")
})

testthat::test_that("geom_timeline_label returns ggplot object", {
  testthat::expect_is(data %>% eq_clean_data() %>%
              filter(COUNTRY %in% c("USA", "CHINA")) %>%
              ggplot(aes(x = DATE, y = COUNTRY, alpha = TOTAL_DEATHS, size = EQ_PRIMARY))+
              geom_timeline()+
              geom_timeline_label(aes(label = LOCATION_NAME),n_max = 3),
            "ggplot")
})

testthat::test_that("eq_map returns leaflet object", {
  testthat::expect_is(data %>% eq_clean_data() %>%
              dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
              eq_map(annot_col = "DATE"),
            "leaflet")
})


testthat::test_that("eq_create_label returns character", {
  testthat::expect_is(eq_create_label(data), "character")
})
