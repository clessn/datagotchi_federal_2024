# Library ----------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

# Data
app_data_cluster <- readRDS("_PrivateFolder_datagotchi_federal_2025/data/clustering/qc2022/05_app_2022_clustered.rds")

appData <- readRDS("_SharedFolder_datagotchi_federal_2024/data/clustering/qc_2022/data-hub-clean-2022-10-27_clean.rds")



# merged appDatas

dfVote <- appData |> 
  select(id, op_intent, time)

data <- app_data_cluster |> 
left_join(dfVote, by = "id") |> 
  drop_na()

# Fonction pour analyser le voteIntent par jour
fct_06_analyze_voteintent_by_day <- function(data) {
  # 1. Ajouter une colonne "day" si elle n'existe pas déjà
  if (!"day" %in% names(data)) {
    data$time <- as.Date(data$time)
    unique_dates <- sort(unique(data$time))
    date_to_day_number <- data.frame(
      time = unique_dates,
      day  = seq_along(unique_dates)
    )
    data <- data %>%
      left_join(date_to_day_number, by = "time") %>%
      arrange(time)
  }
  
  # 2. Agréger les résultats par jour et par cluster
  results_by_day <- data %>%
    group_by(time, cluster) %>%
    summarise(
      total_respondents = n(),
      mean_vote_intent = mean(op_intent, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(results_by_day)
}

# Appliquer la fonction pour obtenir les résultats par jour
voteintent_by_day <- fct_06_analyze_voteintent_by_day(data)
