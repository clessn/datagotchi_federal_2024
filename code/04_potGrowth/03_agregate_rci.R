## Ce script prend les données du pilote avec le RCI et les clusters et l'agrège pour chaque combinaison.
## Le output voulu est un df avec le cluster, le parti et un indicateur entre -100 et 100 qui indique le
##     potentiel de croissance ou la solidité du vote
## On refait le calcul du RCI après avoir calculé sa moyenne par cluster pour mettre le parti en premier
##     au-dessus de 0


# Packages ----------------------------------------------------------------

library(dplyr)
library(tidyr)

# Load data ---------------------------------------------------------------

df_with_rci_and_clusters <- readRDS("_SharedFolder_datagotchi_federal_2024/data/potGrowth/02_pilote_with_rci_and_clusters.rds")

# Data Wrangling ----------------------------------------------------------
# Transformer les données en format long et calculer la moyenne par cluster et parti
df_mean_rci_by_cluster <- df_with_rci_and_clusters %>%
  pivot_longer(
    cols = starts_with("rci_"),  # Sélectionne toutes les colonnes commençant par "rci_"
    names_to = "party",          # Création d'une colonne "party" à partir des noms de colonnes
    names_prefix = "rci_",       # Retire le préfixe "rci_" des noms de colonnes
    values_to = "rci_score"      # Stocke les valeurs dans la colonne "rci_score"
  ) %>%
  group_by(cluster_name, party) %>%
  summarize(mean_rci = mean(rci_score, na.rm = TRUE), .groups = "drop")  # Moyenne du RCI par cluster et parti

# Refaire le calcul du RCI sur les moyennes par clusters ------------------------------

df_rci_by_cluster <- df_mean_rci_by_cluster |> 
  group_by(cluster_name) |> 
  mutate(
    max_potgrowth = max(mean_rci),
    leader = ifelse(mean_rci == max_potgrowth, 1, 0),
    trailer = ifelse(mean_rci != max_potgrowth, 1, 0),
    n_leaders = sum(leader),
    potgrowth_trailers = ifelse(trailer == 1, mean_rci, NA),
    second_potgrowth = case_when(
      n_leaders == 1 ~ max(potgrowth_trailers, na.rm = TRUE),
      n_leaders >= 2 ~ max_potgrowth
    ),
    rci = case_when(
      leader == 1 ~ mean_rci - second_potgrowth,
      trailer == 1 ~ mean_rci - max_potgrowth
    )
  ) |>
  select(cluster_name, party, rci)

# Standardiser les RCI entre -100 et 100 ---------------------------------

### Comment mettre les limites?
#### - Complètement relatif donc que la combinaison parti-cluster avec le plus haut RCI est mise à 100 et vice-versa
#### - Complètement absolu, donc qu'on set manuellement des limites -100 et 100 et on normalise entre ça
#### Après discussion avec Yannick, on croit que absolu c'est mieux à ce stade


##### Au moment d'écrire ce code, la différence entre la plus grande moyenne
#####  et la plus petite est de 1.5 environ
max(df_mean_rci_by_cluster$mean_rci) - min(df_mean_rci_by_cluster$mean_rci) 

### Donc, on peut mettre les limites -100 et 100 à -2 et 2 (et si un cluster dépasse ces limites, on le ramène à 100)
###### On va aller plus loin que le 1.5 étant donné que ça pourrait dépasser nos limites actuelles. On espère que ça bouge pas trop.

## On peut aussi ajuster ce "coefficient", mais c'est pas mal arbitraire.
extreme_limit <- 2

df_rci_by_cluster$rci <- case_when(
  df_rci_by_cluster$rci <= -extreme_limit ~ -100,
  df_rci_by_cluster$rci >= extreme_limit  ~ 100,
  TRUE ~ df_rci_by_cluster$rci * (100 / extreme_limit)
)

# Save it ----------------------------------------------------------------

saveRDS(df_rci_by_cluster, "_SharedFolder_datagotchi_federal_2024/data/potGrowth/03_aggregated_rci.rds")

