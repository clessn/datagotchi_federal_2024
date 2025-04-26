library(dplyr)

df <- haven::read_sav("_SharedFolder_datagotchi_federal_2024/data/panel/panel_questionnaire_April 22, 2025_12.50.sav") %>%
  select(starts_with("ses"))
