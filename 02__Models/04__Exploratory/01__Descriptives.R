rm(list=ls()) # Clearing work space

library(dplyr)
library(ggplot2)

path_data <- "./01__Data/02__Processed_data/"

# Reading in data --------------------------------------------------------------
hrs_data <- readxl::read_xlsx(file.path(path_data, "HRS_Data_Longitudinal.xlsx"))

# Creating descriptive --------------------------------------------------------
descriptives <- hrs_data %>%
  filter(complete.cases(Marital_status, Job_status)) %>%
  mutate(age_group = cut(Age_w2, breaks = c(28, 40, 50, 60, 70, 80, 92),
                         labels = c("28-40", "41-50", "51-60", "61-70", "71-80", "80+"),
                         include.lowest = TRUE)) %>%
  mutate(Depression_total = rowSums(select(., starts_with("Depression")), na.rm = TRUE),
         Loneliness_total = rowSums(select(., starts_with("Loneliness")), na.rm = TRUE),
         Procrastination_total = rowSums(select(., starts_with("Procras")), na.rm = TRUE)) %>%
  mutate(across(c("Loneliness_total", "Procrastination_total"), 
                ~ if_else(. %in% 0, NA, .))) %>%
  select(Age_w2, age_group, Depression_total, Loneliness_total, Procrastination_total)

# Creating dataset with means and errors ---------------------------------------
means <- descriptives %>%
  group_by(age_group) %>%
  summarise(mean_score = mean(Procrastination_total, na.rm = TRUE),
            error = sd(Procrastination_total, na.rm = TRUE) / sqrt(n()))

# Plotting graphs --------------------------------------------------------------
distribution_plot <- ggplot(descriptives, aes(x = Procrastination_total)) +
  geom_density(alpha = 0.7, fill = "#8F0A0A") +
  facet_wrap(~ age_group) +
  labs(title = "Distribution of Procrastination Scores Across Age",
       x = "Total Procrastination",
       y = "Density") +
  theme_bw() +
  ggeasy::easy_center_title()

line_plot_continous <- descriptives %>%
  select(Age_w2, Procrastination_total) %>%
  group_by(Age_w2) %>%
  summarise(mean = mean(Procrastination_total, na.rm = TRUE)) %>%
  ggplot(aes(x = Age_w2, y = mean)) +
  geom_line(colour = "#8F0A0A", linewidth = 0.75) +
  theme_minimal(base_size = 14, base_family = "Arial") +
  scale_x_continuous(breaks = seq(20, 100, by = 5)) +
  labs(x = "Age", y = "Mean Procrastination", 
       title = "Mean Procrastination Scores Across the Life Span") +
  ggeasy::easy_center_title()

line_plot_grouped <- means %>%
  ggplot(aes(x = age_group, y = mean_score, group = 1)) +
  geom_point(colour = "#8F0A0A", size = 1) +
  geom_line(colour = "#8F0A0A", linewidth = 0.75) +
  geom_errorbar(aes(ymin = mean_score - error, ymax = mean_score + error),
                width = 0.1, colour = "#8F0A0A") +
  labs(x = "Age Group", y = "Mean Procrastination") +
  theme_minimal()

bar_plot <- means %>%
  ggplot(aes(x = age_group, y = mean_score, fill = "#8F0A0A")) +
  geom_bar(stat = "identity", width = 0.7, alpha = 0.7) +
  geom_errorbar(aes(ymin = mean_score - error, ymax = mean_score + error),
                width = 0.1, colour = "#8F0A0A") +
  labs(title = "Mean Procrastination Scores Across Age", 
       x = "Age Group", y = "Mean Procrastination") +
  theme_bw() +
  ggeasy::easy_center_title() +
  ggeasy::easy_remove_legend()

# Exporting --------------------------------------------------------------------
export_path <- "./02__Models/04__Exploratory/"

cowplot::save_plot(filename = file.path(export_path, "results/01__Descriptives/01__distribution_plot.png"),
                   plot = distribution_plot)
cowplot::save_plot(filename = file.path(export_path, "results/01__Descriptives/02a__line_plot_continous.png"),
                   plot = line_plot_continous)
cowplot::save_plot(filename = file.path(export_path, "results/01__Descriptives/02b__line_plot_grouped.png"),
                   plot = line_plot_grouped)
cowplot::save_plot(filename = file.path(export_path, "results/01__Descriptives/03__bar_plot.png"),
                   plot = bar_plot)




  
