# The below code will create a Directed Acylic Graph (DAG) of the predictors of procrastination in older adults 
# previously created using the online DAGitty software. This is done with the help of the ggdag package, which utlises
# DAGitty to create and analyze structural causal models and plot them  using ggplot2 and ggraph in a consistent and easy manner
# -----------------------------------------------------------------------------
rm(list=ls()) # Clearing work space

library(ggplot2)
library(ggdag)
  
# Creating DAG ----------------------------------------------------------------
# Initially, the DAG was created in the online DAgitty software - the outputted code is then copied into R
# ggdag is then used to tidy up the graph and add better structure
# To better highlight the outcome variable, we use to dplyr to change the colour of the procrastination node
  
project_dag_full <- dagitty::dagitty('dag {
  bb="-3.061,-3.755,2.929,3.852"
  "HA" [pos="-2.559,2.697"]
  "LS" [pos="-0.369,3.515"]
  "Living status" [pos="2.552,2.845"]
  "MS" [pos="-0.303,-3.366"]
  "PA" [pos="-2.526,3.282"]
  "Self-Control" [pos="1.484,-2.783"]
  Age [exposure,pos="-2.578,0.000"]
  Anxiety [pos="0.736,2.525"]
  Conscientiousness [pos="-1.936,-3.046"]
  Depression [pos="0.713,1.301"]
  Education [pos="-0.640,-1.234"]
  Loneliness [pos="1.673,3.612"]
  Neuroticism [pos="-2.147,1.675"]
  Procrastination [outcome,pos="2.578,0.000"]
  Retirement [pos="2.557,1.944"]
  Stress [pos="-1.987,-0.998"]
  "HA" -> "LS"
  "LS" -> Procrastination
  "Living status" -> Loneliness
  "MS" -> "LS"
  "MS" -> Depression
  "MS" -> Loneliness [pos="2.586,-0.439"]
  "MS" -> Procrastination [pos="1.094,-2.874"]
  "PA" -> "LS"
  "Self-Control" -> "LS" [pos="0.170,0.994"]
  "Self-Control" -> Procrastination [pos="2.204,-1.444"]
  Age -> "LS" [pos="-2.593,2.639"]
  Age -> Conscientiousness [pos="-2.421,-2.748"]
  Age -> Neuroticism
  Age -> Procrastination
  Anxiety -> "LS"
  Anxiety -> Loneliness
  Anxiety -> Procrastination [pos="2.210,1.849"]
  Conscientiousness -> "LS"
  Conscientiousness -> "Self-Control" [pos="-0.741,-3.202"]
  Conscientiousness -> Education
  Conscientiousness -> Procrastination
  Conscientiousness -> Stress [pos="-2.116,-1.415"]
  Depression -> "LS"
  Depression -> "Self-Control" [pos="1.612,-0.353"]
  Depression -> Procrastination
  Education -> "LS"
  Education -> Procrastination
  Loneliness -> "LS"
  Loneliness -> Depression [pos="1.796,2.056"]
  Loneliness -> Procrastination [pos="2.518,0.626"]
  Neuroticism -> "LS" [pos="-1.719,2.617"]
  Neuroticism -> "MS" [pos="-1.222,-1.329"]
  Neuroticism -> Anxiety [pos="-0.933,2.833"]
  Neuroticism -> Depression
  Neuroticism -> Loneliness [pos="0.748,1.621"]
  Neuroticism -> Procrastination
  Neuroticism -> Stress [pos="-2.225,-0.344"]
  Retirement -> "LS" [pos="1.444,3.200"]
  Retirement -> Procrastination [pos="2.728,0.882"]
  Stress -> "LS" [pos="-0.825,2.781"]
  Stress -> "Self-Control" [pos="0.183,-2.296"]
  Stress -> Procrastination}') %>%
  tidy_dagitty() %>%
  dplyr::mutate(colour = ifelse(name == "Procrastination" | name == "Age", "Blue", "Red"))

# A shortened DAG is then created for the purpose of the parallel-process 
# mediation analysis
project_dag_short <- dagitty::dagitty('dag {
  bb="0,0,1,1"
  "Life Satisfaction" [pos="0.000,0.250"]
  Age [exposure,pos="-2.500, 0.000"]
  Conscientiousness [pos="0.000,0.500"]
  Neuroticism [pos="0.000,0.750"]
  Procrastination [outcome,pos="2.500,0.000"]
  "Life Satisfaction" -> Procrastination
  Age -> "Life Satisfaction"
  Age -> Conscientiousness
  Age -> Neuroticism
  Age -> Procrastination
  Conscientiousness -> Procrastination
  Neuroticism -> Procrastination}') %>%
  tidy_dagitty() %>%
  dplyr::mutate(colour = ifelse(name == "Procrastination" | name == "Age", "Blue", "Red"))
  
# Plotting DAG ----------------------------------------------------------------
dag_graph_full <- project_dag_full %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_point(aes(colour = colour), size = 18) +
  geom_dag_text(size = 2, col = "black") +
  geom_dag_edges() +
  theme_dag(legend.position = "none") +
  ggtitle("Predictors of Procrastination in Older Age Adults (DAG)") +
  theme(panel.background = element_rect(fill = "white"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(vjust = -10, hjust = 0.5)) +
  annotate("text", x = 0, y = -4, label = "Note: PA = Physical Activity | HA = Health Assessment | LS = Life Satisfaction | MS = Marital Status")

dag_graph_short <- project_dag_short %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_point(aes(colour = colour), size = 28) +
  geom_dag_text(size = 2, col = "black") +
  geom_dag_edges() +
  theme_dag(legend.position = "none") +
  ggtitle("Mediators of Procrastination in Older Age Adults (DAG)") +
  theme(panel.background = element_rect(fill = "white"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(vjust = -10, hjust = 0.5))
# Exporting -------------------------------------------------------------------
export_path <- "./02__Models/02__DAG/"
  
cowplot::save_plot(file.path(export_path, "results/01_DAG_full.png"), dag_graph_full, base_height = 10)
cowplot::save_plot(file.path(export_path, "results/02_DAG_short.png"), dag_graph_short, base_height = 10)

