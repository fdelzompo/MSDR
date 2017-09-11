require(tidyverse)
require(lubridate)
require(stringr)

# After downloading and reading in the dataset, the overall task for this module is to write a function
# named eq_clean_data()that takes raw NOAA data frame and returns a clean data frame.
# The clean data frame should have the following:
#
#  - A date column created by uniting the year, month, day and converting it to the Date class
#  - LATITUDE and LONGITUDE columns converted to numeric class

eq_clean_data <- function(raw_data){
  raw_data <- raw_data %>%
    dplyr::mutate(
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
  raw_data$LOCATION_NAME <- raw_data$LOCATION_NAME %>% stringr::str_extract('(?<=:\\s).*') %>% stringr::str_to_title()
  return(raw_data)
}

# test-zone
raw_data <- readr::read_tsv('results')
data <- eq_clean_data(raw_data)

# Build a geom for ggplot2 called geom_timeline() for plotting a time line of earthquakes ranging from xmin to
# xmaxdates with a point for each earthquake. Optional aesthetics include color, size, and alpha (for transparency).
# The xaesthetic is a date and an optional y aesthetic is a factor indicating some stratification in which case
# multiple time lines will be plotted for each level of the factor (e.g. country).

geomtimeline <- ggplot2::ggproto("Geomtimeline", Geom,
                                 required_aes = c("x"),
                                 default_aes = ggplot2::aes(y =.2, shape = 19, alpha = 0.3, fill = "blue", color = "blue", size = 0.5,
                                                            xmindate = NA, xmaxdate = NA),
                                 draw_key = draw_key_polygon,

                                 setup_data = function(data, params) {

                                   if(!is.null(data$xmaxdate[1])){
                                     maxdate <- as.numeric(lubridate::as_date(data$xmaxdate[1]))
                                     data <- data %>% dplyr::filter(x <= maxdate)
                                     # panel_scales$x.range[2] = maxdate
                                   }

                                   if(!is.null(data$xmindate[1])){
                                     mindate <- as.numeric(lubridate::as_date(data$xmindate[1]))
                                     data <- data %>% dplyr::filter(x >= mindate)
                                   }
                                   data
                                 },

                                 draw_group = function(data, panel_scales, coord){

                                   coords <- coord$transform(data, panel_scales)

                                   coords$size = coords$size/80

                                   points <- grid::pointsGrob(
                                     x = unit(coords$x, units = 'npc'),
                                     y = unit(coords$y,units = 'npc'),
                                     pch = coords$shape,
                                     size = unit(coords$size, units = 'npc'),
                                     default.units = "npc",
                                     gp = grid::gpar(alpha = coords$alpha, fill = coords$fill, col = coords$colour)
                                   )


                                   lines <- grid::polylineGrob(
                                     x = unit(c(0.01,0.99), units = "npc"),
                                     y = unit(c(min(coords$y),max(coords$y)),units = 'npc'),
                                     gp = grid::gpar(lwd = 1, alpha = 0.8, col = "black")
                                   )

                                   grid::gTree(children = grid::gList(points, lines))
                                 }
                          )


geom_timeline <- function(
                  mapping = NULL, data = NULL, stat = "identity",
                  position = "identity", na.rm = FALSE,
                  show.legend = NA, inherit.aes = TRUE, ...) {
                    ggplot2::layer(
                              geom = geomtimeline, mapping = mapping,
                              data = data, stat = stat, position = position,
                              show.legend = show.legend, inherit.aes = inherit.aes,
                              params = list(na.rm = na.rm, ...)
                )
}

# Build a geom called geom_timeline_label() for adding annotations to the earthquake data. This geom adds a vertical line to each data
# point with a text annotation (e.g. the location of the earthquake) attached to each line. There should be an option to subset to n_max
# number of earthquakes, where we take the n_max largest (by magnitude) earthquakes. Aesthetics are x, which is the date of the
# earthquake and label which takes the column name from which annotations will be obtained.
geomtimeline_label <- ggplot2::ggproto("Geomtimeline_label", Geom,
                                 required_aes = c("x","label"),
                                 default_aes = ggplot2::aes(y =.2, shape = 19, alpha = 0.3, fill = "blue", color = "blue", size = 0.5,n_max = 5),
                                 draw_key = draw_key_polygon,
                                 # setup_data = function(data, params) {
                                 #   str(params)
                                 #  data <- data %>%dplyr::group_by(group)%>%dplyr::top_n(n_max,size)%>%dplyr::ungroup()
                                 #   data
                                 #   },
                                 draw_group = function(data, panel_scales, coord){

                                   nmax <- data$n_max[1]
                                   data <- data %>%dplyr::group_by(group)%>%dplyr::top_n(nmax,size)%>%dplyr::ungroup()
                                   str(data)
                                   coords <- coord$transform(data, panel_scales)

                                   # coords <- coords%>%group_by(id)%>%top_n(n_max[1],size)%>%ungroup()

                                   labels_name <- grid::textGrob(
                                     label = coords$label,
                                     x = unit(coords$x, "npc"),
                                     y = unit(coords$y + 0.15, "npc"),
                                     just = c("left", "bottom"),
                                     rot = 45
                                   )

                                   lines <- grid::polylineGrob(
                                     id = rep(1:dim(coords)[1], 2),
                                     x = unit(c(coords$x, coords$x), units = "npc"),
                                     y = unit(c(coords$y, coords$y+0.15),units = 'npc')
                                   )

                                   grid::gTree(children = grid::gList(lines, labels_name))
                                 }
)




geom_timeline_label <- function(
  mapping = NULL, stat = "identity",
  position = "identity", na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE, ...) {

  ggplot2::layer(
    geom = geomtimeline_label,
    mapping = mapping,
    data = NULL,
    stat = stat,
    position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


# test-zone
data %>%
  filter(COUNTRY %in% c("USA", "CHINA"), YEAR > 2000) %>%
  eq_location_clean()%>%
  ggplot(aes(x = date, y = COUNTRY, fill = TOTAL_DEATHS, size = EQ_PRIMARY, label = LOCATION_NAME))+
  geom_timeline()+
  geom_timeline_label()+
  theme_bw()
