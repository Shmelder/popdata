#' Crude Mortality Rates for WA Men.
#'
#' Mortality rates for men by 5 year age group and race, 2016.  Downloaded from
#' NCHS via Wonder.  Original data in \code{data-raw} folder.
#' Metadata included on bottom of first page.
#'
#' @format A data frame with ten variables: 
#' \describe{
#' \item{\code{State}}{chr, "Washington"} 
#' \item{\code{agegrp}}{chr, "15-19" "20-24" "25-34" "35-44" "45-54" "55-64" "65-74" "75-84" "85+"}
#' \item{\code{Race}}{chr, "Black or African American" "White" "Asian or Pacific Islander" "American Indian or Alaska Native"} 
#' \item{\code{Race Code}}{chr, "2054-5" "2106-3" "A-PI"   "1002-5"}
#' \item{\code{hispanic}}{chr, "Not Hispanic or Latino" "Hispanic or Latino"} 
#' \item{\code{Hispanic Origin Code}}{"2186-2" "2135-2"}
#' \item{\code{Deaths}}{numeric, Number of deaths in this group}
#' \item{\code{Population}}{numeric, Population size of this group} 
#' \item{\code{CrudeRate}}{numeric, Death rate per 100,000 population} 
#' \item{\code{rateNotes}}{chr, Reliability of estimate, (Unreliable) or NA}
#' }
#' 
#' @source \url{http://wonder.cdc.gov/wonder/help/cmf.html}
#' @name mortality
"mortality"