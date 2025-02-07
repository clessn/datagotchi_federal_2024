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

data <- app_data_cluster %>% 
  left_join(dfVote, by = "id") %>% 
  drop_na() %>%
  # Convertir la variable time en Date si nécessaire et filtrer
  mutate(time = as.Date(time)) %>% 
  filter(time <= as.Date("2022-10-03"))

# Fonction pour analyser le voteIntent par jour
fct_06_analyze_voteintent_by_day <- function(data) {
  # 1. S'assurer que time est bien de type Date
  data <- data %>% mutate(time = as.Date(time))
  
  # Création de la variable "day" si nécessaire
  if (!"day" %in% names(data)) {
    unique_dates <- sort(unique(data$time))
    date_to_day_number <- data.frame(
      time = unique_dates,
      day  = seq_along(unique_dates)
    )
    data <- data %>%
      left_join(date_to_day_number, by = "time") %>%
      arrange(time)
  }
  
  # 2. Agréger : compter le nombre de réponses par jour, par cluster et par parti
  results_by_day <- data %>%
    group_by(time, cluster, op_intent) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(time, cluster) %>%
    mutate(total = sum(n),
           proportion = n / total) %>%
    ungroup()
  
  return(results_by_day)
}


# Appliquer la fonction pour obtenir les résultats par jour
voteintent_by_day <- fct_06_analyze_voteintent_by_day(data)


library(ggplot2)

results_by_day <- fct_analyze_voteintent_by_day(data)

ggplot(results_by_day, aes(x = time, y = proportion, color = op_intent)) +
  geom_line() +
  facet_wrap(~ cluster) +
  labs(title = "Évolution du vote intent par cluster",
       x = "Date", y = "Proportion de répondants") +
  theme_minimal()
