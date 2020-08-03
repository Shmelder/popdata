#' Washington Region Population of males 15-65 from ACS data
#'
#' This data is used in the process of reweighting our sample to be
#' representative of washington MSM.  Note these values are multiplied
#' by the proportion of individuals in each region who are MSM to obtain
#' the number of MSM in each region.  The number of MSM in each region
#' is what is used to reweight the sample, **not the number of males
#' 15-65 in each region**
#'
#'#' @docType data
#' @keywords datasets
#' @format A dataset with each row being a region, and the columns are the region
#' name, total number of males aged 15-65 and the corresponding percent.
#' @name wa_acs_totalbyregion
"wa_acs_totalbyregion"
