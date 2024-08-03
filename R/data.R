#' Data on mountains above 2,000 meters from all around the world by elevation
#'
#' Scraped from Wikipedia.
#'
#' @format ## `mountains`
#' A data frame with 1,256 rows and 5 columns:
#' \describe{
#'   \item{id}{Ascending row number}
#'   \item{mountain}{Name of the mountain}
#'   \item{meters, feet}{Height of the mountain in meters & feet}
#'   \item{country}{Countries in which the mountain is located}
#'   ...
#' }
#' @source <https://en.wikipedia.org/wiki/List_of_mountains_by_elevation>
"mountains"

#' Coordinates to the 'mountains' data frame
#'
#' Geocoded by name and country. There may be erros. I did not check all coordinates.
#' The two data frames can be merged by the 'id' column.
#'
#' @format ## `coordinates`
#' A data frame with 1,256 rows and 3 columns:
#' \describe{
#'   \item{id}{Ascending row number}
#'   \item{lat, long}{Latitude & longitude of the mountain}
#'   ...
#' }
#' @source Geocoded using the R package 'tidygeocoder' with the geocoding service ArcGIS.
"coordinates"
