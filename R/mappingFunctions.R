# Mapping Functions. These functions are commonly used commands that allow me to
# diminish the amount of time I need to spend on mapping projects, like monthly
# maps.

#' @importFrom dplyr arrange filter group_by left_join mutate select summarise
#' @importFrom janitor clean_names
#' @importFrom magrittr %>%
#' @importFrom sf st_bbox st_intersection
#' @importFrom tidyr gather pivot_wider
#' @importFrom tmaptools bb_poly
NULL

#' Creates a CRS string based on a polygon that corrects the projection of the
#' polygon
#'
#' This function intakes a polygon and returns a CRS string to best fit that
#' polygon
#'
#' @param sfDF An SF object with at least 2 points in it
#' @return A string that can be used to set the CRS of an SF object
#' @export

correctProjection <- function(sfDF) {

  # Get the bounding box to determine the center
  bbox <- st_bbox(sfDF)
  
  # Calculate the center of the bounding box
  centerLon <- (bbox["xmin"] + bbox["xmax"]) / 2
  centerLat <- (bbox["ymin"] + bbox["ymax"]) / 2
  
  # Define the Albers Equal Area projection in meters using sprintf
  albersProj <- sprintf("+proj=aea +lon_0=%.6f +lat_1=%.6f +lat_2=%.6f +x_0=0 +y_0=0 +datum=WGS84 +units=m", 
                        centerLon, centerLat, centerLat)
  
  return(albersProj)

}

#' This function will take a polygon and cut it off at a specific set of
#' coordinates. To accomplish this, it uses a dataframe containing columns
#' titled value (which contains coordinates like xmin, xmax, ymin, and ymax),
#' and a second column containing the value to cut at.
#'
#' This function cuts a polygon in the specific lat/long value. It does this
#' using a data frame containing information on where to cut. NOTE: Only works
#' in lat/long coordinate systems
#'
#' @param polygon a polygon to clip
#' @param overrideDF a dataframe containing information on where to clip
#' @return the polygon, clipped at the coordinates
#' @export

polygonClipper <- function(polygon,overrideDF) {

  bboxCutter <- sf::st_bbox(polygon)

  for(coord in names(bboxCutter)) {
    if(coord %in% overrideDF$value) {
      bboxCutter[coord] <- overrideDF$coordinate[overrideDF$value==coord]
    }
  }

  cutter <- tmaptools::bb_poly(bboxCutter)

  polygon <- polygon %>% sf::st_intersection(cutter)

  return(polygon)

}

#' Uses 2 input data frames to create specific subway and train travel lines
#' that I have traversed in a given timeframe. One data frame contains dates and
#' stops, another contains subway route data, broken up by traversals between
#' two stops
#'
#' This function intakes two data frames and builds out a geographic dataframe
#' containing all of the routes that were developed between two routes.
#'
#' @param startStopDF a dataframe containing the starting trainstop and ending
#'   trainstop for a given train ride
#' @param subwaySFDF a geographic dataframe containing all of the subway lines
#'   in a given geographic area, where the sf data is broken down based on legs
#'   between stops
#' @return A dataframe containing all of the train routes between two stops
#' @export

subwayRoutes <- function(startStopDF,subwaySFDF) {

  # Create rows to merge
  mergeRange <- subwaySFDF %>%
    as.data.frame() %>%
    dplyr::select(id,line,station_1,station_2,spur) %>%
    tidyr::gather(station_num,station,station_1:station_2)

  # Train data set

  trainRows <- startStopDF %>%
    as.data.frame() %>%
    janitor::clean_names(case = 'lower_camel') %>%
    dplyr::mutate(
      date = as.Date(description, format = '%m/%d/%Y'),
      transitID = ceiling(row_number()/2)+2000,
      phase = ifelse(
        row_number()%%2==0,'end','start'
      )
    ) %>%
    dplyr::select(transitID,name,date,phase) %>%
    tidyr::pivot_wider(
      values_from = name,
      names_from = phase
    ) %>%
    tidyr::gather(position,station,start:end) %>%
    dplyr::left_join(
      mergeRange,
      by = 'station'
    ) %>%
    dplyr::group_by(transitID,line,position) %>%
    dplyr::mutate(rowPortion = 1/n()) %>%
    dplyr::group_by(transitID, line) %>%
    dplyr::mutate(maxRow = sum(rowPortion)) %>%
    dplyr::group_by(transitID) %>%
    dplyr::filter(maxRow == max(maxRow)) %>%
    as.data.frame()

  trains <- data.frame()

  for(tid in unique(trainRows$transitID)) {

    miniSet <- trainRows %>%
      dplyr::filter(transitID == tid)

    rteLine <- unique(miniSet$line)
    description <- unique(miniSet$Description)
    name <- unique(miniSet$Name)
    date <- unique(miniSet$date)

    spurS <- unique(miniSet$spur[miniSet$position == 'start'])
    spurE <- unique(miniSet$spur[miniSet$position == 'end'])

    spurVal <- unique(c(spurS,spurE))
    if(is_empty(spurVal)) {
      spurVal <- NA
    }

    set1 <- miniSet$id[miniSet$position == 'start']
    set2 <- miniSet$id[miniSet$position == 'end']

    # all combinations
    allCombos <- expand.grid(set1,set2) %>%
      dplyr::mutate(difference = abs(Var1-Var2))

    keeper <- allCombos %>%
      dplyr::filter(difference == min(difference))

    rteValues = c(keeper$Var1,keeper$Var2)

    rteSegment <- subwaySFDF %>%

      dplyr::filter(
        line == rteLine,
        id >= min(rteValues),
        id <= max(rteValues),
        spur %in% spurVal | is.na(spur)
      ) %>%

      dplyr::mutate(transitID = tid) %>%

      dplyr::arrange(id) %>%

      dplyr::group_by(transitID) %>%

      dplyr::summarise() %>%

      dplyr::mutate(
        Description = description,
        Name = name,
        line = rteLine,
        date = date
      )

    # Add the geometry to the list
    trains <- trains %>% rbind(rteSegment)
  }

  finalDF <- trains %>% dplyr::select(line, date, geometry)

  return(finalDF)

}
