# Mapping Functions. These functions are commonly used commands that allow me to
# diminish the amount of time I need to spend on mapping projects, like monthly
# maps.

#' @import tidyverse sf
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

  coordinates <- sfDF %>%

    st_bbox() %>%
    {
      c(x = mean(.$xmin, .$xmax),
        y = mean(.$ymin, .$ymax))
    }

  returnString <- sprintf(
    "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=%s +lon_0=%s +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs",
    coordinates[2],
    coordinates[1]
  )

  return(returnString)

}
