# Data -------------------------------------------------------------------
df_pilot1_2021 <- read.csv("data/intrant/pilote-1-federal-2021.csv")
df_pilot2_2021 <- read.csv("data/intrant/pilote-2-federal-2021.csv")

## Sélectionner les colonnes d'intérêt dans chaque dataset
df_pilot1_2021 <- df_pilot1_2021 %>%
  select(any_of(variables_interet))

df_pilot2_2021 <- df_pilot2_2021 %>%
  select(any_of(variables_interet))

## Ajouter une colonne d'identification pour chaque pilot
df_pilot1_2021$source <- "pilot1"
df_pilot2_2021$source <- "pilot2"

## Fusionner les deux datasets en conservant toutes les observations
df_pilot_2021_merged <- bind_rows(df_pilot1_2021, df_pilot2_2021)

# Filter the merged dataset to include only respondents from Quebec
df_pilot_2021_merged_qc <- df_pilot_2021_merged %>%
  filter(quebec == 1)

# Enregistrer le jeux de donnees merged

# Enregistrer le jeu de données fusionné sous forme de CSV
saveRDS(df_pilot_2021_merged_qc, "data/extrant/pilot2021_clean_qconly.rds")