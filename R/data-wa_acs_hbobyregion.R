#' Estimates of Washington race (hispanic, black, or other) region joint distribution.
#'
#' These estimates are based on ACS 2015 population estimates.
#'
#' We are currently working to update documentation on the ACS data.  While
#' much of the data was taken from the ACS website American Fact Finder, this
#' system has been eliminated from the census's webpage, and it is still not clear
#' where to recover this data.
#'
#' @docType data
#' @keywords datasets
#' @format A dataset with each row being a region and race
#' (hispanic, black, or other), and the columns are
#' the region name, race, and number of males 15-65 in the specified race/region
#' group.  The corresponding percent is given in the last column.
#' @name wa_acs_hbobyregion
"wa_acs_hbobyregion"
