###YFS MARGINAL INCREMENT ANALYSIS

library(tidyverse)
library(readxl)



# READ IN RAW DATA --------------------------------------------------------

#spreadsheet with measurements and blind id (structure id)
YFS.meas <- read_excel("./Sample selection and initial modeling/Data/YFS_MIA_measurements.xlsx",
                       sheet = "measurements")

#spreadsheet with all specimen information
YFS.specimen <- read.csv("./Sample selection and initial modeling/Data/YFS_MIA_subsample.csv")



# REFINE LIFE STAGE INFO BASED ON LENGTH AND SEX --------------------------

#When first selecting specimens for this project, life stage was assigned based on fish age 
    #(mistakenly thought that age at maturity was 10 yr for both males and females)
#Here, reassigning life stage based on both fish length (to avoid the use of age which will be unknown in the future) and sex

#Female length at maturity = 29.57 cm (TenBrink & Wilderbuer 2015), corresponds to ~age 10
#Male length at maturity = 21.65 cm (Nichol et al. 2019), corresponds to ~age 7, 21.65=34.03*(1-exp(-0.161*(6.795-0.515))) SAFE von B

YFS.specimen <- YFS.specimen %>% 
  mutate(Stage.ls = ifelse(sex==1 & length < 220, "Juvenile",
                           ifelse(sex==1 & length >= 220, "Adult",
                                  ifelse(sex==2 & length < 300, "Juvenile",
                                         ifelse(sex==2 & length >= 300, "Adult", "NA")))),
         Stage.same = ifelse(Stage==Stage.ls, "same", "different"))

table(YFS.specimen$Stage.same)
table(YFS.specimen$Stage)
table(YFS.specimen$Stage.ls)  #new stage rules greatly reduce sample size for juveniles
#might be better to pick a percentage of Linf? The point when growth really changes from somatic to reproduction

# FILTER FOR SPECIMENS WITH MEASUREMENTS ----------------------------------

YFS.meas <- YFS.meas %>% 
  select(-Month) %>% 
  filter(meas_diff_code <=3)


# REMOVE CONFIDENTIAL FISHERIES INFO --------------------------------------

YFS.specimen <- YFS.specimen %>% 
  select(!c(haul, latitude, longitude, cruise_number, vessel_code))


# JOIN DATA ---------------------------------------------------------------

YFS.MIA <- left_join(YFS.meas, YFS.specimen, by="structure_id")



# WRITE DATA TO FILE ------------------------------------------------------

write.csv(YFS.MIA, file = "./Data/joined_ncf_yfs_data.csv", row.names = FALSE)


#when adding to GitHub, make sure to gitignore 'Sample selection and initial modeling' folder



