# The below code reverse scores necessary data columns
# -----------------------------------------------------------------------------
rm(list=ls()) # Clearing work space

path_data <- "./01__Data/02__Processed_data/"

# Reading in data -------------------------------------------------------------
hrs_data <- readxl::read_xlsx(file.path(path_data, "HRS_Data_Longitudinal.xlsx"))

# Reverse scoring and recoding certain variables
hrs_data <- hrs_data  |>
  dplyr::mutate(Gender = ifelse(Gender == 1, 0, 1),
                Education = ifelse(Education %in% c(0, 1, 2), 0, 1),
                Health_assessment = ifelse(Health_assessment %in% c(1, 2, 3, 4), 1, 0),
                Marital_status = ifelse(Marital_status == 1, 1, 0),
                Job_status = ifelse(Job_status == 5, 1, 0),
                Depression_1 = ifelse(Depression_1 == 5, 0, 1),
                Depression_2 = ifelse(Depression_2 == 5, 0, 1),
                Depression_3 = ifelse(Depression_3 == 5, 0, 1),
                Depression_4 = ifelse(Depression_4 == 5, 1, 0),
                Depression_5 = ifelse(Depression_5 == 5, 0, 1),
                Depression_6 = ifelse(Depression_6 == 5, 1, 0),
                Depression_7 = ifelse(Depression_7 == 5, 0, 1),
                Depression_8 = ifelse(Depression_8 == 5, 0, 1),
                Loneliness_1 = dplyr::recode(Loneliness_1, '1' = 3, '2' = 2, '3' = 1),
                Loneliness_2 = dplyr::recode(Loneliness_2, '1' = 3, '2' = 2, '3' = 1),
                Loneliness_3 = dplyr::recode(Loneliness_3, '1' = 3, '2' = 2, '3' = 1),
  )

# Exporting -------------------------------------------------------------------
writexl::write_xlsx(hrs_data, path = file.path(path_data, "HRS_Data_Longitudinal.xlsx"))