# Library ----------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)

# Data
results_by_day <- readRDS("_PrivateFolder_datagotchi_federal_2025/data/clustering/qc2022/06_cluster_voteIntent.rds")

# Set a color for each party

# graph
ggplot(results_by_day, aes(x = time, y = proportion, color = op_intent)) +
  geom_line() +
  facet_wrap(~ cluster) +
  labs(title = "Évolution du vote intent par cluster",
       x = "Date", y = "Proportion de répondants") +
  theme_minimal()

ggsave()


