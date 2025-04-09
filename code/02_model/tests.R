library(dplyr)

df <- readRDS("_SharedFolder_datagotchi_federal_2024/data/modele/finalmodel_withRTAPredictions_2025-04-15.rds")

print(df$terms)
print(df$sym_coef)
