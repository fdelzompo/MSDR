#'@name eq_clean_data
#'
#'@description Clean up the format of certain columns in a NOAA dataframe
#'
#'@details This function takes raw NOAA data frame (you can download a sample from \href{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1}{here})
#'and returns a clean data frame with:
#'- A DATE column created by uniting the year, month, day and converting it to the Date class
#'- LATITUDE and LONGITUDE columns converted to numeric class
#'- LOCATION_NAME is modified removing the country name from it and changed to title case (see \code{\link{eq_location_clean}})
#'
#'@param data A data frame with raw data from NOAA (see details)
#'
#'@return data A data frame with cleaned columns
#'
#'@importFrom dplyr mutate %>%
#'
#'@examples
#' \dontrun{
#' data <- readr::read_delim("earthquakes.tsv.gz", delim = "\t")
#' clean_data <- eq_clean_data(data)
#' }
#'
#'@export
eq_clean_data <- function(data){
  data <- data %>%
    dplyr::mutate(
      EQ_PRIMARY =  as.numeric(EQ_PRIMARY),
      LATITUDE =  as.numeric(LATITUDE),
      LONGITUDE= as.numeric(LONGITUDE),
      TOTAL_DEATHS = as.numeric(TOTAL_DEATHS),
      YEAR = as.numeric(YEAR),
      MONTH = ifelse(is.na(MONTH) == TRUE, 01,MONTH),
      DAY = ifelse(is.na(DAY) == TRUE, 01,DAY),
      DATE = if_else(data$YEAR > 0,
                     as.Date(ISOdate(abs(YEAR),MONTH,DAY)),
                     as.Date(as.numeric(ISOdate(0,1,1)-ISOdate(abs(YEAR),MONTH,DAY)), origin = '0000-01-01'))
    )

  data <- eq_location_clean(data)

  return(data)
}


#'@name eq_location_clean
#'
#'@description Clean up the format of LOCATION_NAME column in a NOAA dataframe (see \code{\link{eq_clean_data}} for more details)
#'
#'@details cleans the LOCATION_NAME column by stripping out the country name (including the colon) and converts names
#' to title case (as opposed to all caps). This function is only used inside \code{\link{eq_clean_data}}, not exported.
#'
#'@param data A data frame with raw data from NOAA (see details)
#'
#'@return data A data frame with cleaned LOCATION_NAME format
#'
#'@importFrom dplyr %>%
#'@importFrom stringr str_extract str_trim str_to_title
#'
#'@examples
#' \dontrun{
#' data <- readr::read_delim("earthquakes.tsv.gz", delim = "\t")
#' clean_data <- eq_location_clean(data)
#' }
#'
#'
eq_location_clean <- function(data){
  data$LOCATION_NAME <- data$LOCATION_NAME %>% stringr::str_extract("(?![A-Z]*:).+") %>%
                                  stringr::str_trim("left") %>%
                                  stringr::str_to_title()

  data
}

#'@name geom_timeline
#'
#'@description Creates a timeline chart specific for a NOAA dataframe (see \code{\link{eq_clean_data}} for more details)
#'
#'@details plotting a time line of earthquakes ranging from xmin to xmaxdates with a point for each earthquake.
#'Optional aesthetics include color, size, and alpha (for transparency). The x aesthetic is a date and an optional y aesthetic
#'is a factor indicating some stratification in which case multiple time lines will be plotted for each level of the factor (e.g. country).
#'
#'@param data a dataframe containing NOAA details
#'@param xmaxdate date or charachter formatted in year, month, day order
#'@param xmindate date or charachter formatted in year, month, day order
#'
#'@inheritParams ggplot2::geom_point
#'
#'@importFrom ggplot2 layer
#'
#'@examples
#' \dontrun{
#' data %>%
#'  filter(COUNTRY %in% c("USA", "CHINA")) %>%
#'  eq_clean_data()%>%
#'  ggplot(aes(x = DATE, y = COUNTRY, alpha = TOTAL_DEATHS, size = EQ_PRIMARY))+
#'  geom_timeline(xmindate = "2008/01/01", xmaxdate = "2015/12/31")
#' }
#'
#'@export
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

#'@name geomtimeline
#'
#'@description Creates the grob for \code{\link{geom_timeline}}
#'
#'@importFrom ggplot2 ggproto aes draw_key_point Geom
#'@importFrom grid gpar pointsGrob polylineGrob gList gTree
#'@importFrom lubridate as_date
#'@importFrom dplyr filter %>%
geomtimeline <- ggplot2::ggproto("Geomtimeline", ggplot2::Geom,
                                 required_aes = c("x"),
                                 default_aes = ggplot2::aes(y =.2, shape = 19, alpha = 0.3, fill = "blue", color = "blue", size = 0.5, stroke = 0.5,
                                                            xmindate = NULL, xmaxdate = NULL),
                                 draw_key = ggplot2::draw_key_point,

                                 setup_data = function(data, params) {

                                   if(!is.null(params$xmaxdate)){
                                     maxdate <- as.numeric(lubridate::as_date(params$xmaxdate))
                                     data <- data %>% dplyr::filter(x <= maxdate)
                                   }

                                   if(!is.null(params$xmindate)){
                                     mindate <- as.numeric(lubridate::as_date(params$xmindate))
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


#'@name geom_timeline_label
#'
#'@description Creates additional notation for the timeline chart (see \code{\link{geom_timeline}} for more details)
#'
#'@details This geom adds a vertical line to each data point with a text annotation (e.g. the location of the earthquake) attached to each line.
#'There is an option to subset to n_max number of earthquakes, where we take the n_max largest (by magnitude) earthquakes.
#'
#'
#'@param data a dataframe containing NOAA details
#'@param n_max numeric, it requires a value defined for the aestetic size, if omitted is by default = 5
#'
#'@inheritParams ggplot2::geom_text
#'
#'@importFrom ggplot2 layer
#'
#'@examples
#' \dontrun{
#' data %>%
#'  filter(COUNTRY %in% c("USA", "CHINA")) %>%
#'  eq_clean_data()%>%
#'  ggplot(aes(x = DATE, y = COUNTRY, alpha = TOTAL_DEATHS, size = EQ_PRIMARY))+
#'  geom_timeline()+
#'  geom_timeline_lable(aes(label = LOCATION_NAME),n_max = 3)
#' }
#'
#'@export
geom_timeline_label <- function(
  mapping = NULL, stat = "identity",
  data = NULL,
  position = "identity", na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE, ...) {

  ggplot2::layer(
    geom = geomtimeline_label,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#'@name geomtimeline_label
#'
#'@description Creates the grob for \code{\link{geom_timeline_label}}
#'
#'@importFrom ggplot2 ggproto aes draw_key_polygon
#'@importFrom grid gpar pointsGrob polylineGrob gList gTree
#'@importFrom lubridate as_date
#'@importFrom dplyr filter %>%
geomtimeline_label <- ggplot2::ggproto("Geomtimeline_label", ggplot2::Geom,
                                 required_aes = c("x","label"),
                                 default_aes = ggplot2::aes(y =.2, shape = 19, alpha = 0.3, fill = "blue", color = "blue", size = 0.5,
                                                            n_max = NA,
                                                            xmindate = NULL, xmaxdate = NULL),
                                 draw_key = draw_key_blank,

                                 setup_data = function(self, data, params){

                                   nmax <- ifelse(!is.null(params$n_max),params$n_max,5)

                                   data <- data %>%dplyr::group_by(group)%>%dplyr::top_n(nmax,size)%>%dplyr::ungroup()

                                   data

                                 },

                                 draw_group = function(data, panel_scales, coord){

                                     coords <- coord$transform(data, panel_scales)

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


#'@name eq_map
#'
#'@description Creates a html map using leaflet to show NOAA data (see \code{\link{eq_clean_data}} for more details)
#'
#'@details The function maps the epicenters (LATITUDE/LONGITUDE) and annotates each point with in pop up window containing annotation data
#'stored in a column of the data frame. The user should be able to choose which column is used for the annotation in the pop-up with
#'a function argument named annot_col. Each earthquake should be shown with a circle, and the radius of the circle should be proportional
#'to the earthquake's magnitude (EQ_PRIMARY).
#'
#'@param annot_col any column of the dataframe, will be converted to character and used as label
#'@param data a dataframe containing NOAA details
#'
#'@importFrom leaflet leaflet addTiles addCircleMarkers
#'
#'@return a leaflet map
#'
#'@examples
#' \dontrun{
#' readr::read_delim("earthquakes.tsv.gz", delim = "\t") %>%
#' eq_clean_data() %>%
#' dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#' eq_map(annot_col = "DATE")
#' }
#'
#'@export
eq_map <- function(data, annot_col = NULL){
    data%>%
    leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(lng =~LONGITUDE, lat =~LATITUDE,
                              radius =~EQ_PRIMARY,
                              weight = 2,
                              color = "red",
                              popup =as.character(data[[annot_col]])
                              )


}


#'@name eq_create_label
#'
#'@description Creates a html string for a more representative label.
#'
#'@details put together a character string for each earthquake that shows the cleaned location (as cleaned by the \code{\link{eq_location_clean}}),
#'the magnitude (EQ_PRIMARY), and the total number of deaths (TOTAL_DEATHS), with boldface labels for each ("Location","Total deaths",
#'and "Magnitude"). If an earthquake is missing values for any of these, both the label and the value are skipped for that element of the tag.
#'
#'@param data a dataframe containing NOAA details
#'
#'@return character string
#'
#'@examples
#' \dontrun{
#' readr::read_delim("earthquakes.tsv.gz", delim = "\t") %>%
#' eq_clean_data() %>%
#' dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#' dplyr::mutate(popup_text = eq_create_label(.)) %>%
#' eq_map(annot_col = "popup_text")
#' }
#'
#'@export
eq_create_label <- function(data) {
  loc <- ifelse(is.na(data$LOCATION_NAME),"",paste("<strong>Location:</strong>",data$LOCATION_NAME))
  eq <- ifelse(is.na(data$EQ_PRIMARY),"",paste("<br><strong>Magnitude:</strong>",data$EQ_PRIMARY))
  death <- ifelse(is.na(data$TOTAL_DEATHS),"",paste("<br><strong>Total deaths:</strong>",data$TOTAL_DEATHS))
  paste0(loc,eq,death)
}


