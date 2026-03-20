# The below code reverse scores necessary data columns
# -----------------------------------------------------------------------------
rm(list=ls()) # Clearing work space

path_data <- "./01__Data/02__Processed_data/"

# Reading in data -------------------------------------------------------------
hrs_data <- readxl::read_xlsx(file.path(path_data, "HRS_Data_Longitudinal.xlsx"))

# Replacing preassigned missing values integers with the actual NA value ------ 
hrs_data <- hrs_data |>
  dplyr::mutate(across(c("Education"), ~ ifelse(. %in% 9, NA, .))) |>
  dplyr::mutate(across("Health_assessment", ~ ifelse(. %in% c(8, 9), NA, .))) |>
  dplyr::mutate(across("Marital_status", ~ ifelse(. %in% 5, NA, .))) |>
  dplyr::mutate(across(c("Job_status"), ~ ifelse(. %in% c(98, 99), NA, .))) |>
  dplyr::mutate(across(c(paste0("Procras_", 1:12), 
                         paste0("Depression_", 1:8)), ~ ifelse(. %in% c(-8, 8, 9), NA, .)))

# Exporting -------------------------------------------------------------------
writexl::write_xlsx(hrs_data, path = file.path(path_data, "HRS_Data_Longitudinal.xlsx"))