library(dplyr)
library(ggplot2)


# Par jour ---------------------------------------------------------------

# Nombre de répondants par jour
nb_par_jour <- app_data %>%
  group_by(date) %>%
  summarise(count = n(), .groups = "drop")

ggplot(nb_par_jour, aes(x = date, y = count)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Nombre de répondants par jour",
    x = "Date",
    y = "Nombre de répondants"
  ) +
  clessnize::theme_clean_light()

# PAr party --------------------------------------------------------------
# Couleurs spécifiques pour les partis
party_colors <- c(
  "CAQ" = "#05d2e0",
  "PQ" = "#0043FE",
  "PLQ" = "#FF2806",
  "QS" = "#f88808",
  "PCQ" = "#311c68",
  "Autre" = "#73F986",
  "NoVote" = "#707373"
)

rep_vote <- app_data %>%
  group_by(vote_intent) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(pct = count / sum(count) * 100)

ggplot(rep_vote, aes(x = reorder(vote_intent, -pct), y = pct, fill = vote_intent)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Répartition des répondants par intention de vote (App Datagotchi QC-2022)",
    x = "Intention de vote",
    y = "Pourcentage"
  ) +
  theme_minimal() +
  scale_fill_manual(values = party_colors) +
  clessnize::theme_clean_light() +
    theme(legend.position = "none")




