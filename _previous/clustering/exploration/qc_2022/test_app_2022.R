
library(dplyr)
library(tidyr)
library(ggplot2)

# Calculer le nombre de répondants par jour et par région
obs_per_day_region <- app2022 %>%
  select(time, region_name) %>%
  group_by(time, region_name) %>%
  summarise(num_obs = n(), .groups = 'drop')

# 1. Graphique du nombre total d'observations par jour
# Étape 1 : Calculer le nombre total d'observations par jour
obs_per_day <- obs_per_day_region %>%
  group_by(time) %>%
  summarise(total_obs = sum(num_obs))

# Étape 2 : Créer le graphique
ggplot(obs_per_day, aes(x = as.Date(time), y = total_obs)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(
    title = "Nombre total d'observations par jour",
    x = "Date",
    y = "Nombre d'observations"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 2. Graphique du nombre total d'observations par région
# Étape 1 : Calculer le nombre total d'observations par région
obs_per_region <- obs_per_day_region %>%
  group_by(region_name) %>%
  summarise(total_obs = sum(num_obs), .groups = 'drop')

# Étape 2 : Créer le graphique
ggplot(obs_per_region, aes(x = reorder(region_name, -total_obs), y = total_obs)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(
    title = "Nombre total d'observations par région",
    x = "Région",
    y = "Nombre d'observations"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
