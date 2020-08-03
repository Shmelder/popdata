library(tidyverse)
library(data.table)

wa_acs <- read.csv("../data/acs_15_spt_b01001_with_ann.csv",
                   skip = 1, header = TRUE, sep = ",")
## give info that file is static/where to find new versions. place .csv inside of package itself.

wa_acs %>%
  filter(Population.Group != "Total population")

## Reformat data and define age groups ----------------------------------------------

# Select the columns with ages 15-65 for males and the total for males
wa_acs_male15to65 <- wa_acs %>%
  dplyr::select(Geography, Population.Group, Estimate..Male.,
                Estimate..Male....15.to.17.years, Estimate..Male....18.and.19.years,
                Estimate..Male....20.years, Estimate..Male....21.years,
                Estimate..Male....22.to.24.years, Estimate..Male....25.to.29.years,
                Estimate..Male....30.to.34.years, Estimate..Male....35.to.39.years,
                Estimate..Male....40.to.44.years, Estimate..Male....45.to.49.years,
                Estimate..Male....50.to.54.years, Estimate..Male....55.to.59.years,
                Estimate..Male....60.and.61.years, Estimate..Male....62.to.64.years,
                Estimate..Male....65.and.66.years)

# Rename columns
setnames(wa_acs_male15to65,
         old=c("Geography", "Estimate..Male.", "Estimate..Male....15.to.17.years",
               "Estimate..Male....18.and.19.years", "Estimate..Male....20.years",
               "Estimate..Male....21.years", "Estimate..Male....22.to.24.years",
               "Estimate..Male....25.to.29.years", "Estimate..Male....30.to.34.years",
               "Estimate..Male....35.to.39.years", "Estimate..Male....40.to.44.years",
               "Estimate..Male....45.to.49.years", "Estimate..Male....50.to.54.years",
               "Estimate..Male....55.to.59.years", "Estimate..Male....60.and.61.years",
               "Estimate..Male....62.to.64.years", "Estimate..Male....65.and.66.years"),
         new=c("county", "tot.male", "males.15to17", "males.18to19", "males.20", "males.21",
               "males.22to24", "males.25to29", "males.30to34", "males.35to39", "males.40to44",
               "males.45to49", "males.50to54", "males.55to59", "males.60to61", "males.62to64",
               "males.65to66"))

# Divide column "males.65to66" in half to get column for males.65 (assuming 65-year-olds are half of this group)
wa_acs_male15to65$males.65 <- round(wa_acs_male15to65$males.65to66 / 2)
wa_acs_male15to65$males.65to66 <- NULL

# Age
## Define a variable for Total males in age range
wa_acs_male15to65$males.total <-  (wa_acs_male15to65$males.15to17 + wa_acs_male15to65$males.18to19 + wa_acs_male15to65$males.20 +  wa_acs_male15to65$males.21 + wa_acs_male15to65$males.22to24 + wa_acs_male15to65$males.25to29 + wa_acs_male15to65$males.30to34 + wa_acs_male15to65$males.35to39 + wa_acs_male15to65$males.40to44 + wa_acs_male15to65$males.45to49 + wa_acs_male15to65$males.50to54 + wa_acs_male15to65$males.55to59 + wa_acs_male15to65$males.60to61 + wa_acs_male15to65$males.62to64 + wa_acs_male15to65$males.65)

## Re-group ages 18-24 into bin 18-24
d <- data(package = "plyr")
## names of data sets in the package
d$results[, "Item"]

## Re-group ages 60-65 into bin 60-65
wa_acs_male15to65$males.60to65 <- (wa_acs_male15to65$males.60to61 + wa_acs_male15to65$males.62to64 +  wa_acs_male15to65$males.65)

## Select only age groups of interest
wa_acs_male15to65 <- wa_acs_male15to65 %>% dplyr::select(county, Population.Group, males.15to17, males.18to24, males.25to29, males.30to34, males.35to39, males.40to44, males.45to49, males.50to54, males.55to59, males.60to65, males.total)

#Reshape the data so race/ethnicity groups are columns
#Melt the data
wa_acs_male15to65_melt <- reshape2::melt(wa_acs_male15to65, id=c("county", "Population.Group"))
wa_acs_male15to65_cast <- reshape2::dcast(wa_acs_male15to65_melt, county + variable ~ Population.Group)

#replace NA to 0 - race/ethnic groups with no values are likely b/c 0 men in that group were counted
wa_acs_male15to65_cast[is.na(wa_acs_male15to65_cast)] <- 0

#Rename columns
setnames(wa_acs_male15to65_cast,
         old=c("variable", "American Indian and Alaska Native alone, not Hispanic or Latino",
               "Asian alone, not Hispanic or Latino",
               "Black or African American alone or in combination with one or more other races, Hispanic or Latino",
               "Black or African American alone or in combination with one or more other races, not Hispanic or Latino",
               "Black or African American alone, Hispanic or Latino",
               "Black or African American alone, not Hispanic or Latino",
               "Hispanic or Latino (of any race) (200-299)",
               "Native Hawaiian and Other Pacific Islander alone, not Hispanic or Latino",
               "Some other race alone, not Hispanic or Latino",
               "Total population", "Two or more races, not Hispanic or Latino",
               "White alone, not Hispanic or Latino"),
         new=c("Age_group", "AIAN", "Asian", "Black_any_Hispanic", "Black_any_nonHispanic",
               "Black_alone_Hispanic", "Black_alone_nonHispanic", "Hispanic",
               "NHOPI", "Other", "Total", "Multiple", "White"))

#Replace original dataset with the reshaped one
wa_acs_male15to65_orig <- wa_acs_male15to65
wa_acs_male15to65 <- wa_acs_male15to65_cast

# Region
wa_acs_male15to65$region <- ifelse(wa_acs_male15to65$county=="King County, Washington", "King County",
                            ifelse(wa_acs_male15to65$county %in%
                                     c("Clallam County, Washington", "Clark County, Washington",
                                       "Cowlitz County, Washington", "Grays Harbor County, Washington",
                                       "Island County, Washington", "Jefferson County, Washington",
                                       "Kitsap County, Washington", "Lewis County, Washington",
                                       "Mason County, Washington", "Pacific County, Washington",
                                       "Pierce County, Washington", "San Juan County, Washington",
                                       "Skagit County, Washington", "Skamania County, Washington",
                                       "Snohomish County, Washington", "Thurston County, Washington",
                                       "Wahkiakum County, Washington", "Whatcom County, Washington"),
                                   "Other Western", "Eastern"))

# Race
## Look at how many Hispanic men identify as black
# wa_acs_male15to65 %>% group_by(region) %>% filter(Age_group %in% "males.total") %>% summarise(total.black.Hisp = sum(Black_any_Hispanic), total.Hisp = sum(Hispanic)) %>% mutate(percent.hispblack = total.black.Hisp / total.Hisp)
#Ranges from 0.4 to 3%. Keep these men as Hispanic

## Look at how many men who listed two or more races men identify as black
# black.multi <- wa_acs_male15to65 %>% group_by(region) %>% filter(Age_group %in% "males.total") %>% summarise(total.black.alone = sum(Black_alone_nonHispanic), total.black.any = sum(Black_any_nonHispanic), total.multiple = sum(Multiple)) %>% mutate(total.black.multi = (total.black.any - total.black.alone), percent.blackmulti = total.black.multi / total.multiple)
#Ranges from 23% to 30%. Code these as black


## Calculate desired population totals and percents----------------------------------------------------------

#Total males by region
wa_acs_totalbyregion <- wa_acs_male15to65 %>%
  filter(Age_group %in% "males.total") %>%
  dplyr::select(region, Age_group, Total) %>%
  group_by(region) %>% summarise(total = sum(Total)) %>%
  mutate(Percent = total / sum(total))

usethis::use_data(wa_acs_totalbyregion, overwrite = TRUE)

#Age by region
wa_acs_agebyregion <- wa_acs_male15to65 %>%
  dplyr::select(region, Age_group, Total) %>%
  filter(!(Age_group %in% "males.total")) %>%
  group_by(region, Age_group) %>%
  summarise(Frequency = sum(Total)) %>%
  group_by(region) %>% mutate(Percent = Frequency / sum(Frequency))

usethis::use_data(wa_acs_agebyregion, overwrite = TRUE)

#[] Race by region
## Define race groups - note: this is not perfect because the numbers for the individual race/eth groups (Hispanic, NH white, NH black alone, NH Asian, NH Native Hawaiian/Other Pacific Islander, NH American Indian/Alaska Native, NH other, and NH two or more) do not sum to the total in each county. This is likely due to variability in the estimates for each group. Could keep the columns with the margin of error for the original data frame and work with those (details on how they are calculated here: https://www.census.gov/content/dam/Census/programs-surveys/acs/guidance/training-presentations/2016_MOE_Transcript_01.pdf.)

### Define race groups with the "other" category not collapsed  (for reasons specified above, this is some error in these totals)
wa_acs_male15to65_fullracedist <- wa_acs_male15to65 %>%
  mutate(blackmulti = Black_any_nonHispanic - Black_alone_nonHispanic) %>%
  mutate(nonblackmulti = Multiple - blackmulti) %>%
  dplyr::select(county, region, Age_group, Hispanic,
                Black_any_nonHispanic, White, AIAN, Asian,
                NHOPI, Other, nonblackmulti, Total)

wa_acs_fullracebyregion <- as.data.frame(
  wa_acs_male15to65_fullracedist %>%
    filter(Age_group %in% "males.total") %>%
    group_by(region) %>%
    summarise_at(vars(Hispanic:nonblackmulti), sum))

wa_acs_fullracebyregion <- tidyr::pivot_longer(wa_acs_fullracebyregion, -region)

wa_acs_fullracebyregion <- wa_acs_fullracebyregion %>%
  group_by(region) %>% mutate(percent = value / sum(value))

wa_acs_fullracebyregion_percent <- wa_acs_fullracebyregion %>%
  dplyr::select(-value) %>%
  tidyr::pivot_wider(values_from = percent)

usethis::use_data(wa_acs_fullracebyregion, overwrite = TRUE)

### Define HBO race groups
## (for reasons specified above, this is some error in these totals. But for now use use the point estimates and define other group as total - Hispanic - black alone or in combination)
wa_acs_male15to65_hbo <- wa_acs_male15to65 %>%
  dplyr::select(county, region, Age_group,
                Black_any_nonHispanic,
                Hispanic, Total) %>%
  mutate(Other = Total - Black_any_nonHispanic - Hispanic)

wa_acs_male15to65_hbo %>% group_by(Age_group) %>%
  summarise("Black" = sum(Black_any_nonHispanic),
            "Hispanic" = sum(Hispanic),
            "Other" = sum(Other))

wa_acs_hbobyregion <- wa_acs_male15to65_hbo %>%
  filter(Age_group %in% "males.total") %>%
  group_by(region) %>%
  summarise_at(vars(Black_any_nonHispanic:Other), sum) %>%
  mutate(Percent_Hispanic = (Hispanic / Total),
         Percent_black = (Black_any_nonHispanic / Total),
         Percent_other = (Other / Total)) %>%
  dplyr::select(region, Percent_Hispanic, Percent_black, Percent_other)

usethis::use_data(wa_acs_hbobyregion, overwrite = TRUE)

#remove datasets don't need
rm(wa_acs_male15to65_cast)
rm(wa_acs_male15to65_melt)
rm(wa_acs_male15to65_orig)
rm(wa_acs)

us_msm_est2013 <-
  read.csv("../orig_data/Emory_CAMP_US_MSM_Estimates_Data_2013.csv",
                           header=TRUE, sep=",")

#Filter to Washington State and limit to variables of interest
wa_msm_est2013 <- us_msm_est2013 %>%
  dplyr::select(STATEFP, COUNTYFP,
                ADULTMEN, MSM12MTH, MSM5YEAR) %>%
  filter(STATEFP %in% 53)

#Define variable for region (couty codes here: Parameter estimation/Data/Emory CAMP_US MSM Estimates Data Dictionary 2013.doc)
wa_msm_est2013$region <- ifelse(wa_msm_est2013$COUNTYFP %in% 033, "King County",
                                ifelse(wa_msm_est2013$COUNTYFP %in%
                                         c(009, 011, 015, 027, 029, 031, 035,
                                           041, 045, 049, 053, 055, 057, 059,
                                           061, 067, 069, 073),
                                       "Other Western WA",
                                ifelse(wa_msm_est2013$COUNTYFP %in%
                                         c(001, 003, 005, 007, 013,
                                           017, 019, 021, 023, 025,
                                           037, 039, 043, 047, 051,
                                           063, 065, 071, 075, 077),
                                       "Eastern WA", NA)))

#Calculate the proportion of males who are MSM in each region  (based on past 5-year behavior)
propMSMbyregion <- wa_msm_est2013 %>% group_by(region) %>%
  summarise(propmsm = sum(MSM5YEAR) / sum(ADULTMEN))

# #Look at distribution of the MSM population based on the CAMP dataset estimates of population size (note this is men 18+)
# wa_msm_est2013 %>% group_by(region) %>% summarise(totMSM = sum(MSM5YEAR)) %>% mutate(distMSM = totMSM / sum(totMSM))


#----------------------------------------------------------------------------------
#Calculate number of MSM by region using 2011-2015 ACS data
#----------------------------------------------------------------------------------

MSMbyregion <- cbind.data.frame(
  region = c("Eastern WA", "King County", "Other Western WA"),
  numMSM = wa_acs_totalbyregion$total*propMSMbyregion$propmsm
  )

MSMbyregion$percentMSM <- MSMbyregion$numMSM / sum(MSMbyregion$numMSM)
#Reorder rows
MSMbyregion$region <- factor(MSMbyregion$region,
                             levels(factor(MSMbyregion$region))[c(2,3,1)])
MSMbyregion <- MSMbyregion %>% arrange(region)

usethis::use_data(MSMbyregion, overwrite = TRUE)

# Estimate MSM population totals by age, race/ethnicity, and region
#-----------------------------------------------------------------------------------
#' Define "population" totals from census data and Jeremy Grey's analysis
#' Population totals are defined for age and race by taking the distribution among all
#' males by region from the census, and multiplying it by the number of MSM in each region
#-----------------------------------------------------------------------------------

# REGION----

pop.region <- MSMbyregion %>% dplyr::select(region, numMSM) %>%
  mutate(total = round(numMSM)) %>% dplyr::select(-numMSM)
levels(pop.region$region)[levels(pop.region$region)=="Other Western WA"] <- "Western WA"

# RACE/ETHNICITY BY REGION ----
# Full race groups by region
# Weighted by MSM population in each region
pop.fullracebyregion <- rbind.data.frame(
  c(region = "Eastern WA", wa_acs_fullracebyregion_percent[1,2:9] *
      pop.region$total[pop.region$region %in% "Eastern WA"]),
  c(region = "King County", wa_acs_fullracebyregion_percent[2,2:9] *
      pop.region$total[pop.region$region %in% "King County"]),
  c(region = "Western WA", wa_acs_fullracebyregion_percent[3,2:9] *
      pop.region$total[pop.region$region %in% "Western WA"]))

# HBO by region
# Weighted by MSM population in each region
pop.hbobyregion <- cbind.data.frame(
  region=c(rep("King County", 3), rep("Western WA", 3), rep("Eastern WA", 3)),
  hbo=c(rep(c("Hispanic", "Black", "Other"), 3)),
  Freq=c(
    wa_acs_hbobyregion$Percent_Hispanic[wa_acs_hbobyregion$region %in% "King County"] *
      pop.region$total[pop.region$region %in% "King County"],
    wa_acs_hbobyregion$Percent_black[wa_acs_hbobyregion$region %in% "King County"] *
      pop.region$total[pop.region$region %in% "King County"],
    wa_acs_hbobyregion$Percent_other[wa_acs_hbobyregion$region %in% "King County"] *
      pop.region$total[pop.region$region %in% "King County"],
    wa_acs_hbobyregion$Percent_Hispanic[wa_acs_hbobyregion$region %in% "Other Western"] *
      pop.region$total[pop.region$region %in% "Western WA"],
    wa_acs_hbobyregion$Percent_black[wa_acs_hbobyregion$region %in% "Other Western"] *
      pop.region$total[pop.region$region %in% "Western WA"],
    wa_acs_hbobyregion$Percent_other[wa_acs_hbobyregion$region %in% "Other Western"] *
      pop.region$total[pop.region$region %in% "Western WA"],
    wa_acs_hbobyregion$Percent_Hispanic[wa_acs_hbobyregion$region %in% "Eastern"] *
      pop.region$total[pop.region$region %in% "Eastern WA"],
    wa_acs_hbobyregion$Percent_black[wa_acs_hbobyregion$region %in% "Eastern"] *
      pop.region$total[pop.region$region %in% "Eastern WA"],
    wa_acs_hbobyregion$Percent_other[wa_acs_hbobyregion$region %in% "Eastern"] *
      pop.region$total[pop.region$region %in% "Eastern WA"]))

#Specify order of factors

pop.hbobyregion$hbo <- factor(pop.hbobyregion$hbo, levels = c("Hispanic", "Black", "Other"))
pop.hbobyregion$region <- factor(pop.hbobyregion$region, levels = c("King County", "Western WA", "Eastern WA"))
pop.hbobyregion <- pop.hbobyregion %>% arrange(region, hbo)


#  RACE/ETHNICITY ----
# Race (Full race groups)
pop.fullrace <- reshape2::melt(pop.fullracebyregion, id="region")
pop.fullrace <- pop.fullrace %>% group_by(variable) %>%
  summarise(total = sum(value)) %>% mutate(percent = total/sum(total))

# Race (Hispanic, black, other)
pop.hbo <- pop.hbobyregion %>% group_by(hbo) %>% summarise(Freq=sum(Freq))

# AGE ----
# Define age groups to correspond to the age bins defined for the "age_cat_rwt" variable with the sample data

## For re-weighting, combine 15-17 and 18-24
wa_acs_agebyregion$age_cat_rwt <- ifelse(wa_acs_agebyregion$Age_group %in%
                                           c("males.15to17", "males.18to24"), "15-24",
                                  ifelse(wa_acs_agebyregion$Age_group %in%
                                           "males.25to29", "25-29",
                                  ifelse(wa_acs_agebyregion$Age_group %in%
                                           "males.30to34", "30-34",
                                  ifelse(wa_acs_agebyregion$Age_group %in%
                                           "males.35to39", "35-39",
                                  ifelse(wa_acs_agebyregion$Age_group %in%
                                           "males.40to44", "40-44",
                                  ifelse(wa_acs_agebyregion$Age_group %in%
                                           "males.45to49", "45-49",
                                  ifelse(wa_acs_agebyregion$Age_group %in%
                                           "males.50to54", "50-54",
                                  ifelse(wa_acs_agebyregion$Age_group %in%
                                           "males.55to59", "55-59",
                                  ifelse(wa_acs_agebyregion$Age_group %in%
                                           "males.60to65", "60-65", NA)))))))))

# Weight by MSM population in each region to get age distribution among MSM
wa_acs_agebyregion$numMSM <- ifelse(
  wa_acs_agebyregion$region %in% "Eastern",
  round(wa_acs_agebyregion$Percent *
          pop.region$total[pop.region$region %in% "Eastern WA"]),
  ifelse(wa_acs_agebyregion$region %in% "King County",
         round(wa_acs_agebyregion$Percent *
                 pop.region$total[pop.region$region %in% "King County"]),
        ifelse(wa_acs_agebyregion$region %in% "Other Western",
               round(wa_acs_agebyregion$Percent *
                       pop.region$total[pop.region$region %in% "Western WA"]), NA)))

pop.age <- wa_acs_agebyregion %>% dplyr::select(-Frequency) %>%
  group_by(age_cat_rwt) %>% summarise(Freq = sum(numMSM))

#################################################
##### Creating joint age race distribution ######
#################################################
data_sets <- list()

zp_fls <- unzip("../orig_data/PEP_2018_PEPASR5HnonHispanicRaceAC.zip",
                exdir = "../intermediate_files")
df_name <- zp_fls[grep("with_ann", zp_fls)]
data_sets[[1]] <- read.csv(df_name, stringsAsFactors = FALSE)
unlink(zp_fls)

zp_fls <- unzip("../orig_data/PEP_2018_PEPASR6HHispanic.zip",
                exdir = "../intermediate_files")
df_name <- zp_fls[grep("with_ann", zp_fls)]
data_sets[[2]] <- read.csv(df_name, stringsAsFactors = FALSE)
unlink(zp_fls)

alone_comb_f <- function(x){
  length(grep("ac", colnames(data_sets[[x]]))) > 0
}
rac_df_idx <- vapply(1:length(data_sets), alone_comb_f,
                     FUN.VALUE = TRUE)
rac_df <- data_sets[[which(rac_df_idx)]]
ra_df  <- data_sets[[min(which(!rac_df_idx))]]

b_max_age <- 84
lower_bounds <- seq(from = 0, to = b_max_age - 4, by = 5)
upper_bounds <- seq(from = 0 + 4, to = b_max_age, by = 5)
age_ranges <- paste0("age", lower_bounds, "to", upper_bounds)
age_ranges <- c(age_ranges, "age85plus")

### Obtaining information on Hispanic and non-Hispanic populations

max_year_ra <- surv_year <- 2018

ra_df_mr <- ra_df[c(1, grep(max_year_ra,ra_df$year.id)),
                  c(1:4, grep("totpop", colnames(ra_df))), ]
ra_df_sub <- ra_df_mr[
  c(1, which(ra_df_mr$hisp.id %in% c("hisp", "nhisp"))),
  c(1:3, grep("sex1", colnames(ra_df_mr)))
]
ra_colnames <- paste0("totpop_sex1_", age_ranges)
h_nh_t <- cbind(ra_df_sub[, 1:4],
                ra_df_sub[, ra_colnames])

#### Obtaining information on Black non-Hispanic populations
if(is.null(surv_year)){
  max_year_rac <- suppressWarnings(
    max(as.numeric(gsub("est.", "",
                        rac_df$year.id)),
        na.rm = TRUE)
  )
}else{
  max_year_rac <- surv_year
}

rac_df_mr <- rac_df[c(1, grep(max_year_rac,rac_df$year.id)),
                    c(1:4, grep("bac", colnames(rac_df))), ]
rac_df_sub <- rac_df_mr[
  c(1, which(rac_df_mr$hisp.id %in% c("nhisp"))),
  c(1:3, grep("sex1", colnames(rac_df_mr)))
]
rac_colnames <- paste0("bac_sex1_", age_ranges)
bac_t <- cbind(rac_df_sub[, 1:4],
               rac_df_sub[, rac_colnames], stringsasFactors = FALSE)
all_data <- data.frame("Black" = as.numeric(t(bac_t[bac_t$hisp.id == "nhisp",
                                                    rac_colnames])),
                       "Hispanic" = as.numeric(t(h_nh_t[h_nh_t$hisp.id == "hisp",
                                                        ra_colnames])),
                       "Non_Hispanic" = as.numeric(t(h_nh_t[h_nh_t$hisp.id == "nhisp",
                                                            ra_colnames])),
                       stringsAsFactors = FALSE)
colnames(all_data) <- c("Black", "Hispanic", "Non_Hispanic")
all_data$Other <- all_data$Non_Hispanic - all_data$Black
all_data$Non_Hispanic <- NULL
all_data$Age_grp <- age_ranges
rownames(all_data) <- NULL
wa_joint_age_race <- all_data

usethis::use_data(wa_joint_age_race, overwrite = TRUE)

compressed <-
  rbind(apply(wa_joint_age_race[wa_joint_age_race$Age_grp %in%
                          c("age15to19", "age20to24"), 1:3], 2, sum),
        apply(wa_joint_age_race[wa_joint_age_race$Age_grp %in%
                          c("age25to29", "age30to34"), 1:3], 2, sum),
        apply(wa_joint_age_race[wa_joint_age_race$Age_grp %in%
                          c("age35to39", "age40to44"), 1:3], 2, sum),
        apply(wa_joint_age_race[wa_joint_age_race$Age_grp %in%
                          c("age45to49", "age50to54"), 1:3], 2, sum),
        apply(wa_joint_age_race[wa_joint_age_race$Age_grp %in%
                          c("age55to59", "age60to64"), 1:3], 2, sum))
fin_tab <- data.frame(compressed, "age_grp" = c("15-24", "25-34", "35-44", "45-54", "55-65"))
jnt_distr <- fin_tab %>% gather("race", "Freq", -age_grp)
jnt_distr$agerace <- paste0(jnt_distr$age_grp, "_", jnt_distr$race)
jnt_distr$agerace <- factor(jnt_distr$agerace,
                            levels = unique(jnt_distr$agerace),
                            labels = unique(jnt_distr$agerace))

wa_joint_age_race_comp <- jnt_distr
usethis::use_data(wa_joint_age_race_comp, overwrite = TRUE)





