rm(list=ls()) # Clearing work space

path_data <- "./01__Data/01__Raw_data/"

# Reading in Data -------------------------------------------------------------
# Tracker file containing information on each participant
tracker <- haven::read_sav(file.path(path_data, "Tracker.sav"))

# HRS data (2016)
demographics <- haven::read_sav(file.path(path_data, "HRS_2016/Demographics_2016.sav"))
cognition <- haven::read_sav(file.path(path_data, "HRS_2016/Cognition_2016.sav"))
health <- haven::read_sav(file.path(path_data, "HRS_2016/Health_2016.sav"))
employement <- haven::read_sav(file.path(path_data, "HRS_2016/Employment_2016.sav"))
lbq_w2 <- haven::read_sav(file.path(path_data, "HRS_2016/LB_Question_2016.sav"))

# HRS data (2020)
mod_v <- haven::read_sav(file.path(path_data, "HRS_2020/Module_V_2020.sav")) # Experimental module with the procrastination data

# Filtration ------------------------------------------------------------------
# Out of the 15723 participants in the data set only 1368 answered the procrastination scale
mod_v <- mod_v|>
  dplyr::filter(!is.na(RV155))

# Each data file is now filtered to only focus on those participants per wave
# This is done by matching the HHID and PN numbers
tracker <- dplyr::semi_join(tracker, mod_v, by = c("HHID", "PN"))

demographics <- dplyr::semi_join(demographics, mod_v, by = c("HHID", "PN"))
health <- dplyr::semi_join(health, mod_v, by = c("HHID", "PN"))
cognition <- dplyr::semi_join(cognition, mod_v, by = c("HHID", "PN"))
employement <- dplyr::semi_join(employement, mod_v, by = c("HHID", "PN"))
lbq_w2 <- dplyr::semi_join(lbq_w2, mod_v, by = c("HHID", "PN"))

# Since the data frames do not match in dimensions we need to filter down to the 
# exact participants per wave with the smallest dimension
tracker <- dplyr::semi_join(tracker, lbq_w2, by = c("HHID", "PN"))
demographics <- dplyr::semi_join(demographics, lbq_w2, by = c("HHID", "PN"))
cognition <- dplyr::semi_join(cognition, lbq_w2, by = c("HHID", "PN"))
health <- dplyr::semi_join(health, lbq_w2, by = c("HHID", "PN"))
mod_v <- dplyr::semi_join(mod_v, lbq_w2, by = c("HHID", "PN"))

# Creating singular data set of relevant data ----------------------------------
hrs_data <- cbind(
  tracker[, c("HHID", "PN", "GENDER", "BIRTHYR", "DEGREE", "PMARST", "PAGE")],
  health[, c("PC001")],
  employement[, c("PJ005M1")],
  cognition[, c("PD110", "PD111", "PD112", "PD113", "PD114", "PD115", "PD116", "PD117")],
  lbq_w2[, c("PLB019A", "PLB019B", "PLB019C")],
  mod_v[, c(paste0("RV", 156:167))]
)

# Renaming the variables
hrs_data <- hrs_data |>
  dplyr::rename(
    HHID = "HHID",
    ID = "PN",
    Gender = "GENDER",
    Birth_year = "BIRTHYR",
    Education = "DEGREE",
    Marital_status = "PMARST",
    Age_w2 = "PAGE",
    Health_assessment = "PC001",
    Job_status = "PJ005M1",
    Depression_1 = "PD110", # Depression (2016)
    Depression_2 = "PD111",
    Depression_3 = "PD112",
    Depression_4 = "PD113",
    Depression_5 = "PD114",
    Depression_6 = "PD115",
    Depression_7 = "PD116",
    Depression_8 = "PD117",
    Loneliness_1 = "PLB019A", # Loneliness (2016)
    Loneliness_2 = "PLB019B",
    Loneliness_3 = "PLB019C",
    Procras_1 = "RV156", # Procrastination (2020)
    Procras_2 = "RV157",
    Procras_3 = "RV158",
    Procras_4 = "RV159",
    Procras_5 = "RV160",
    Procras_6 = "RV161",
    Procras_7 = "RV162",
    Procras_8 = "RV163",
    Procras_9 = "RV164",
    Procras_10 = "RV165",
    Procras_11 = "RV166",
    Procras_12 = "RV167"
  )

# Exporting -------------------------------------------------------------------
export_path <- "./01__Data/02__Processed_data/"

writexl::write_xlsx(hrs_data, path = file.path(export_path, "HRS_Data_Longitudinal.xlsx"))