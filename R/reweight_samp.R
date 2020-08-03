#' This function allows for the reweighting of any dataset
#' with information on age (in 10 year increments from 15 to 65),
#' race (black, hispanic, or other),
#' and region of Washington to match the ACS based joint age / race
#' distribution and proportion of MSM in each region.
#'
#' @param obs_data The observed data for which weights are wanted.
#' @param age_c_name The name of the age column in the data frame.
#' @param race_c_name The name of the race column in the data frame.
#' @param reg_c_name The name of the region column in the data frame.
#' @param ego_id_name The name of the column with unique identifiers for
#' each ego.
#' @return A vector of weights such that the observations with
#' these weights match the Washington Age / Race distribution and
#' regional MSM distribution.
#'
#' @export

reweight_data <- function(obs_data, age_c_name = "age.grp",
                          race_c_name = "race",
                          reg_c_name = "region",
                          ego_id_name = "ego.id") {
  ## TODO: Remove these lines when transferring to
  ## Egonet (or wherever)
  if (file.exists("data/wa_joint_age_race_comp.rda")) {
    load("data/wa_joint_age_race_comp.rda")
    load("data/MSMbyregion.rda")
    rwt_df <- wa_joint_age_race_comp
  }else{
    rwt_df <- WHAMPData::wa_joint_age_race_comp
    MSMbyregion <- WHAMPData::MSMbyregion
  }

  reg_distr <- data.frame("region" = factor(1:3),
                          "Freq" = round(MSMbyregion$numMSM))
  obs_data$age <- obs_data[, age_c_name]
  obs_data$race <- obs_data[, race_c_name]
  obs_data$ego.id <- obs_data[, ego_id_name]
  if(!is.null(reg_c_name)){
    obs_data$region <- obs_data[, reg_c_name]
  }else{
    obs_data$region <- NA
  }

  ego_dems <- obs_data %>%
    select(ego.id, "race" = race,
           "age.grp" = age, region)
  ego_dems$race <- c("Black", "Hispanic", "Other")[ego_dems$race]
  ego_dems$age.grp <- c("15-24", "25-34", "35-44",
                        "45-54", "55-65", "55-65")[ego_dems$age.grp]
  ego_dems$agerace <- paste0(ego_dems$age.grp, "_", ego_dems$race)
  ego_dems$agerace <- factor(ego_dems$agerace, levels = unique(ego_dems$agerace),
                             labels = unique(ego_dems$agerace))
  ego_dems$region <- factor(ego_dems$region, levels = unique(ego_dems$region),
                            labels = unique(ego_dems$region))
  colnames(ego_dems) <- c("ego.id", "age_cat_rwt", "hbo", "region", "agerace")

  if (is.null(reg_c_name)) {
    w_wts <- complete.cases(ego_dems %>% dplyr::select(-region))
  }else{
    w_wts <- complete.cases(ego_dems)
  }

  rwt_df <- rwt_df[
    rwt_df$agerace %in% unique(ego_dems$agerace), ]

  egos_svy <- suppressWarnings(
    survey::svydesign(id=~1, data = ego_dems[w_wts, ]) )

  if (!is.null(reg_c_name)) {
    egos_raked <-  survey::rake(egos_svy,
                                list(~agerace, ~region),
                                list(rwt_df[, c("Freq", "agerace")],
                                     reg_distr))
  }else{
    egos_raked <-  survey::rake(egos_svy,
                                list(~agerace),
                                list(rwt_df[, c("Freq", "agerace")]))
  }

  wts <- cbind.data.frame("ego.id" = egos_raked$variables$ego.id,
                          "weight" = (1/egos_raked$prob),
                          stringsAsFactors = FALSE)

  wts <- wts[match(unique(wts$ego.id), wts$ego.id), ]
  wts$weight <- nrow(obs_data) * wts$weight / sum(wts$weight)


  fin_tab <- left_join(obs_data, wts, by = "ego.id")
  return(fin_tab$weight)
}
