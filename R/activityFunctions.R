# To Do Functions. These are funcitons specifically geared towards making
# working with my elaborate to do data set easier.

#' @importFrom dplyr enquo if_else mutate row_number
#' @importFrom hms as_hms
#' @importFrom janitor clean_names
#' @importFrom lubridate date minutes month seconds
#' @importFrom magrittr %>%
#' @importFrom stringr str_to_lower
#' @importFrom tidyr uncount
NULL

#' Cleans up the activity data by converting all datetime values to the
#' appropriate timezone (currently, probably permanently, Eastern) and then
#' splitting lines where activities cross days
#'
#' This function converts timezones on the activities data using a separate data
#' set and then splits lines that cross days into 2 separate line items
#'
#' @param df The data frame containing my activities data
#' @param tzDF The timezone conversion data frame
#' @param startVar The column containing the starting datetime for each activity
#' @param endVar The column containing the ending datetime for each activity
#' @param shiftVar The column in the timezone conversion data frame containing
#'   the number of hours to shift the time by
#' @param tzVar The column in the activities data frame containing the text
#'   timezone value.
#' @return Activities data with timezones shifted to the appropriate times
#' @export

actDataCleaner <- function(df,tzDF,startVar,endVar,shiftVar,tzVar) {

  startVar <- dplyr::enquo(startVar)
  endVar <- dplyr::enquo(endVar)
  shiftVar <- dplyr::enquo(shiftVar)
  tzVar <- dplyr::enquo(tzVar)

  newDF <- df %>%
    janitor::clean_names(case = 'lower_camel') %>%
    dplyr::mutate(
      !!tzVar := stringr::str_to_lower(!!tzVar),
      id = dplyr::row_number()
    ) %>%
    actDataTZCorrection(tzDF,!!startVar,!!endVar,!!shiftVar,!!tzVar) %>%
    actDataDateSplitter(!!startVar,!!endVar,id)

}

#' After a timezone conversion, the activities data may have certain activities
#' that start and end on different days. This is not how the activity data is
#' intended to be designed - this function splits activities that start and end
#' on separate days into multiple line items
#'
#' This function splits activities that start and end on different days into 2
#' separate line items
#'
#' @param df The data frame containing my activities data
#' @param startVar The column containing the starting datetime for each activity
#' @param endVar The column containing the ending datetime for each activity
#' @param idVar The column containing a unique identifier for each original line
#'   item
#' @return Activities data cross-day line items split into multiple lines
#' @export

actDataDateSplitter <- function(df, startVar, endVar, idVar) {

  newDF <- df %>%
    dplyr::mutate(
      expansionValue = dplyr::if_else(
        lubridate::date({{ startVar }}) != lubridate::date({{ endVar }}),
        2, 1
      )
    ) %>%

    tidyr::uncount(expansionValue) %>%

    dplyr::group_by({{ idVar }}) %>%

    dplyr::mutate(
      idRowNum = dplyr::row_number(),
      modRow = ifelse(
        n() > 1, 1, 0
      )
    ) %>%

    dplyr::ungroup() %>%

    dplyr::mutate(

      # The next two lines convert the combo lines so that each one sits on a single date.
      # The script converts the endPoint of the first line to be the end of day
      # and the StartPoint of the second line to be the start of the day.

      # The first expansion line will be set to date of the start time to 11:59 PM
      {{ endVar }} := dplyr::if_else(
        modRow == 1 & idRowNum == 1,
        as.POSIXct(
          paste(
            lubridate::date({{ startVar }}),
            hms::as_hms('23:59:59')
          ),
          format="%Y-%m-%d %H:%M:%S",
          tz = 'UTC'
        ),
        {{ endVar }}
      ),

      # Second expansion line will be 0:00 on the date of the end point
      {{ startVar }} := dplyr::if_else(
        modRow == 1 & idRowNum == 2,
        as.POSIXct(
          paste(
            lubridate::date({{ endVar }}),
            hms::as_hms('00:00:00')
          ),
          format="%Y-%m-%d %H:%M:%S",
          tz = 'UTC'
        ),
        {{ startVar }}
      ),

      # Datetime values
      # Quarter measurement
      quarter = ceiling(lubridate::month({{ startVar }})/3),
      date = lubridate::date({{ startVar }}),
      startTime = hms::as_hms({{ startVar }}),
      endTime = hms::as_hms({{ endVar }})
    )

  return(newDF)

}

#' This function corrects the timezones in the activities data set using a
#' separate, outside table. It takes the outside table and adds (or subtracts)
#' hours from the start and end time of an activity based on the shiftVar, which
#' should be a numeric value indicating the total hours that the data needs to
#' move from one timezone to another. Til present, the shift to column has been
#' eastern time.
#'
#' This function corrects the start and end variables in the activity data set
#' to a new timezone.
#'
#' @param df The data frame containing my activities data
#' @param tzDF The timezone conversion data frame
#' @param startVar The column containing the starting datetime for each activity
#' @param endVar The column containing the ending datetime for each activity
#' @param shiftVar The column in the timezone conversion data frame containing
#'   the number of hours to shift the time by
#' @param tzVar The column in the activities data frame containing the text
#'   timezone value.
#' @param mergeList The variables on which to join the timezone shift table to
#'   the activities data. Typically a timezone variable and two indicator
#'   variables that indicate whether or not the secondary timezeon is on
#'   daylight savings
#' @return Activities data with start and end values corrected
#' @export

actDataTZCorrection <- function(df, tzDF, startVar, endVar, shiftVar, tzVar, mergeList = c('timezone', 'amDst', 'locDst')) {

  startVar <- dplyr::enquo(startVar)
  endVar <- dplyr::enquo(endVar)
  shiftVar <- dplyr::enquo(shiftVar)
  tzVar <- dplyr::enquo(tzVar)

  newDF <- df %>%
    dplyr::mutate(
      !!endVar := if_else(
        hms::as_hms(!!endVar) == hms::as_hms('23:59:00'),
        !!endVar + lubridate::minutes(1), !!endVar
      )
    ) %>%
    tzCorrecter(tzDF, !!tzVar, !!startVar, !!shiftVar, mergeList) %>%
    tzCorrecter(tzDF, !!tzVar, !!endVar, !!shiftVar, mergeList) %>%
    dplyr::mutate(
      !!endVar := if_else(
        hms::as_hms(!!endVar) == hms::as_hms('00:00:00'),
        !!endVar - lubridate::seconds(1), !!endVar
      )
    )

  return(newDF)
}

#' A function to shift the timezone of a variable based on an outside table. It
#' takes the outside table and adds (or subtracts) hours from chosen variable
#' based on the shiftVar, which should be a numeric value indicating the total
#' hours that the data needs to move from one timezone to another. Til present,
#' the shift to column has been eastern time.
#'
#' Shifts a single variable to a new timezone based on a separate data table
#'
#' @param df The data frame containing my activities data
#' @param tzDF The timezone conversion data frame
#' @param tzVar The column in the activities data frame containing the text
#'   timezone value.
#' @param timeVar The column containing datetime to shift
#' @param shiftVar The column in the timezone conversion data frame containing
#'   the number of hours to shift the time by
#' @param mergeList The variables on which to join the timezone shift table to
#'   the activities data. Typically a timezone variable and two indicator
#'   variables that indicate whether or not the secondary timezone is on
#'   daylight savings
#' @return Activities data with start and end values corrected
#' @export

tzCorrecter <- function(df, tzDF, tzVar, timeVar, shiftVar, mergeList) {

  tzVar <- dplyr::enquo(tzVar)
  timeVar <- dplyr::enquo(timeVar)
  shiftVar <- dplyr::enquo(shiftVar)

  newDF <- df %>%
    dplyr::mutate(!!tzVar := stringr::str_to_lower(!!tzVar)) %>%
    dplyr::left_join(tzDF, by = mergeList) %>%
    dplyr::mutate(!!timeVar := !!timeVar + lubridate::hours(!!shiftVar)) %>%
    dplyr::select(-!!shiftVar)

  return(newDF)
}

