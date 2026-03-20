rm(list=ls()) # Clearing work space

library(dplyr)
library(ggplot2)

path_data <- "./01__Data/02__Processed_data/"

# Reading in data --------------------------------------------------------------
hrs_data <- readxl::read_xlsx(file.path(path_data, "HRS_Data_Longitudinal.xlsx"))

# Creating various summary datasets --------------------------------------------
# Depression cut off
descriptives <- hrs_data %>%
  filter(complete.cases(Marital_status, Job_status)) %>%
  select(c(starts_with("Depression"), starts_with("Procras"))) %>%
  mutate(Total_depression = rowSums(select(., starts_with("Depression")), na.rm = TRUE),
         Total_procrastination = rowSums(select(., starts_with("Procras")), na.rm = TRUE)) %>%
  mutate(across("Total_procrastination", ~ ifelse(. %in% 0, NA, .))) %>%
  mutate(Depression_binary = ifelse(Total_depression < 1, 0, 1),
         Depression_binary = factor(Depression_binary), .before = "Depression_1") %>%
  rowwise() %>%
  mutate(Mean_procrastination = round(mean(
    c_across(paste0("Procras_", 1:12)), na.rm = TRUE), 
    digits = 2))

# Performing T-Test and linear regression --------------------------------------
t.test(Total_procrastination ~ Depression_binary, var.equal = TRUE, data = descriptives)

model <- lm(Mean_procrastination ~ Depression_1 + Depression_2 + Depression_3 + 
            Depression_4 + Depression_5 + Depression_6 + Depression_7 + 
            Depression_8, data = descriptives)

model_summary <- summary(model)

# Visualizing Model
# Residuals vs Fitted Values
ggplot(data = data.frame(fitted = model$fitted.values, 
                         residuals = model$residuals), 
       aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, colour = "red") +
  labs(x = "Fitted Values", y = "Residuals", title = "Residuals vs. Fitted Values") +
  theme_classic() +
  ggeasy::easy_center_title()

# Q-Q Plot
ggplot(data = data.frame(residuals = model$residuals), mapping = aes(sample = residuals)) +
  qqplotr::stat_qq_band() +
  qqplotr::stat_qq_line() +
  qqplotr::stat_qq_point() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles", title = "Normal Q-Q Plot") +
  theme_classic() +
  ggeasy::easy_center_title()

# Scale-Location Plot
ggplot(data.frame(fitted = model$fitted.values, sqrt_resid = sqrt(abs(model$residuals))), aes(x = fitted, y = sqrt_resid)) +
  geom_point() +
  labs(x = "Fitted values", y = "√|Standardized Residuals|", title = "Scale-Location Plot") +
  theme_classic() +
  ggeasy::easy_center_title()

# Cook's Distance Plot
cooksd <- cooks.distance(model)
ggplot(data.frame(observation = seq_along(cooksd), cooksd), aes(x = observation, y = cooksd)) +
  geom_point() +
  geom_hline(yintercept = 4/length(model$residuals), color = "red", linetype = "dashed") +
  labs(x = "Observation", y = "Cook's Distance", title = "Cook's Distance Plot") +
  theme_classic() +
  ggeasy::easy_center_title()

# Creating a summary dataset for plotting --------------------------------------
summary_data <- descriptives %>%
  # Pivoting to long format
  select(starts_with("Depression"), Total_procrastination, -Depression_binary) %>%
  tidyr::pivot_longer(cols = starts_with("Depression"), 
                      names_to = "Symptom", 
                      values_to = "Present") %>%
  # Changing depression names to actual symptom names
  mutate(Symptom = factor(case_when(
    Symptom == "Depression_1" ~ "Depression",
    Symptom == "Depression_2" ~ "Fatigue",
    Symptom == "Depression_3" ~ "Restlessness",
    Symptom == "Depression_4" ~ "Lack of Happiness",
    Symptom == "Depression_5" ~ "Loneliness",
    Symptom == "Depression_6" ~ "Lack of Enjoyment",
    Symptom == "Depression_7" ~ "Sadness",
    Symptom == "Depression_8" ~ "Lack of Motivation"
  ), levels = c("Depression", "Fatigue", "Restlessness", "Lack of Happiness",
                "Loneliness", "Lack of Enjoyment", "Sadness", "Lack of Motivation"))) %>%
  # Grouping
  filter(complete.cases(Present)) %>%
  group_by(Symptom, Present) %>%
  summarize(mean_procrastination = round(mean(
    Total_procrastination, na.rm = TRUE), 
    digits = 2)) %>%
  mutate(Present = ifelse(Present == 1, "Present", "Not Present"),
         Present = factor(Present))

# Mean Procrastination Scores Between Depressed/Not Depressed
depression_plot <- descriptives %>%
  select(Depression_binary, Total_procrastination) %>%
  filter(complete.cases(Depression_binary)) %>%
  group_by(Depression_binary) %>%
  summarise(Procrastination = mean(Total_procrastination, na.rm = TRUE)) %>%
  mutate(Depression_binary = ifelse(Depression_binary == 0, "Not Depressed", "Depressed"),
         Depression_binary = factor(Depression_binary)) %>%
  ggplot(aes(x = Depression_binary, y = Procrastination, fill = Depression_binary)) +
  geom_bar(stat = "identity", width = .50) +
  scale_fill_manual(values = c("Depressed" = "#c1272D", "Not Depressed" = "#0000a7")) +
  theme_minimal(base_size = 14, base_family = "Arial") +
  labs(x = "", y = "Mean Procrastination", 
       title = "Mean Procrastination Scores per Group") +
  ggeasy::easy_center_title() +
  ggeasy::easy_remove_legend()

# Mean Procrastination Scores Per Symptom
symptom_plot <- ggplot(summary_data, aes(x = Symptom, y = mean_procrastination, fill = Present)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Present" = "#c1272D", "Not Present" = "#0000a7")) +
  theme_minimal(base_size = 14, base_family = "Arial") +
  # Add data labels
  geom_text(aes(label = round(mean_procrastination, 2)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 4) +
  # Scaling y-axis so labels fit
  ylim(0, max(summary_data$mean_procrastination) * 1.1) +
  labs(x = "", y = "Mean Procrastination Scores", 
       title = "Mean Procrastination Scores by Symptom Presence") +
  ggeasy::easy_center_title() +
  ggeasy::easy_add_legend_title("") +
  ggeasy::easy_move_legend(to = c("bottom"))

# We would like to bold certain labels on this graph
text_format <- c("plain", "bold", "plain", "plain", 
                 "bold", "plain", "plain", "bold")

symptom_plot <- symptom_plot +
  theme(axis.text.x = element_text(face = text_format))

# Exporting --------------------------------------------------------------------
export_path <- "./02__Models/04__Exploratory/"

output_1 <- capture.output(results)
output_2 <- capture.output(model_summary)

writeLines(output_1, file.path(export_path, "results/02__Regression/01__t_test_results.txt"))
writeLines(output_2, file.path(export_path, "results/02__Regression/02__regression_results.txt"))

cowplot::save_plot(filename = file.path(export_path, "results/02__Regression/03__symptom_plot.png"),
                   plot = symptom_plot, base_height = 7)
cowplot::save_plot(filename = file.path(export_path, "results/02__Regression/04__depression_plot.png"),
                   plot = depression_plot)
  
