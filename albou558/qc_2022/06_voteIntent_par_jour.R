# Library ----------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

# Data
app_data_cluster <- readRDS("_PrivateFolder_datagotchi_federal_2025/data/clustering/qc2022/05_app_2022_clustered.rds")

appData <- readRDS("_PrivateFolder_datagotchi_federal_2025/data/clustering/qc2022/01_app_2022.rds")

# merged appDatas

dfVote <- appData |> 
  select(id, op_intent)

data <- app_data_cluster |> 
left_join(dfVote, by = "id")

# Fonction pour analyser le voteIntent par jour
fct_06_analyze_voteintent_by_day <- function(app_data_with_clusters) {
  # 1. Ajouter une colonne "day" si elle n'existe pas déjà
  if (!"day" %in% names(app_data_with_clusters)) {
    app_data_with_clusters$date <- as.Date(app_data_with_clusters$date)
    unique_dates <- sort(unique(app_data_with_clusters$date))
    date_to_day_number <- data.frame(
      date = unique_dates,
      day  = seq_along(unique_dates)
    )
    app_data_with_clusters <- app_data_with_clusters %>%
      left_join(date_to_day_number, by = "date") %>%
      arrange(date)
  }
  
  # 2. Agréger les résultats par jour et par cluster
  results_by_day <- app_data_with_clusters %>%
    group_by(day, cluster) %>%
    summarise(
      total_respondents = n(),
      mean_vote_intent = mean(vote_intent, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(results_by_day)
}

# Appliquer la fonction pour obtenir les résultats par jour
voteintent_by_day <- fct_06_analyze_voteintent_by_day(app_data_with_clusters)