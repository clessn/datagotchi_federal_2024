# Script complet pour calculer les prédictions politiques par RTA
# basées sur l'appartenance aux circonscriptions

# 1. Chargement des packages nécessaires
library(openxlsx)
library(dplyr)
library(sf)
library(cartessn)

# 2. Chargement des données de prédictions par circonscription
path_data <- "_SharedFolder_datagotchi_federal_2024/data/modele/"
poliwave <- openxlsx::read.xlsx(paste0(path_data, "CAtable.xlsx"))

# Afficher les premières lignes pour vérification
cat("Données poliwave chargées:\n")
print(head(poliwave))

# 3. Chargement des données spatiales et calcul des intersections
cat("\nChargement des données spatiales et calcul des intersections...\n")

# Charger les géométries des RTA et des circonscriptions
rta <- cartessn::spatial_canada_2021_rta
circo <- cartessn::spatial_canada_2022_electoral_ridings

# Vérifier que les systèmes de coordonnées correspondent
if (st_crs(rta) != st_crs(circo)) {
  cat("Transformation du système de coordonnées des RTA...\n")
  rta <- st_transform(rta, st_crs(circo))
}

# Calculer les intersections entre RTA et circonscriptions
intersections <- cartessn::intersect_spatial_objects(
  spatial_ref = rta,
  id_ref = "rta",
  spatial_target = circo,
  id_target = "id_riding",
  dTolerance = 50
)

# Extraire les noms des circonscriptions
noms_circo <- circo %>%
  st_drop_geometry() %>%
  select(id_riding, name_riding_en)

# Préparer la table d'appartenance RTA-circonscription
rta_circo_appartenance <- intersections %>%
  left_join(noms_circo, by = "id_riding") %>%
  mutate(pourcentage_appartenance = prop_of_ref_area_covered_by_target * 100) %>%
  select(
    rta = rta,
    circo_id = id_riding,
    circo_nom = name_riding_en,
    aire_intersection_m2 = area_covered_by_target_m2,
    pourcentage_appartenance
  ) %>%
  arrange(rta, desc(pourcentage_appartenance))

# Afficher les premières lignes pour vérification
cat("\nTable d'appartenance RTA-circonscription créée:\n")
print(head(rta_circo_appartenance, 5))

# Sauvegarder la table d'appartenance
write.csv(rta_circo_appartenance, 
          paste0(path_data, "rta_circonscriptions_appartenance.csv"), 
          row.names = FALSE)

# 4. Renommer la colonne ID dans poliwave pour correspondre à circo_id
poliwave_renamed <- poliwave %>%
  rename(circo_id = ID)

# 5. Joindre les données de prédiction avec les données d'appartenance
cat("\nJoindre les données et calculer les prédictions par RTA...\n")

# Convertir circo_id en numérique dans rta_circo_appartenance
rta_circo_appartenance <- rta_circo_appartenance %>%
  mutate(circo_id = as.numeric(circo_id))

# Joindre les tables
rta_predictions <- rta_circo_appartenance %>%
  left_join(poliwave_renamed, by = "circo_id")

# 6. Calculer les prédictions pondérées pour chaque parti par combinaison RTA-circonscription
rta_predictions_weighted <- rta_predictions %>%
  mutate(
    CPC_weighted = CPC * (pourcentage_appartenance / 100),
    LPC_weighted = LPC * (pourcentage_appartenance / 100),
    NDP_weighted = NDP * (pourcentage_appartenance / 100),
    GPC_weighted = GPC * (pourcentage_appartenance / 100),
    BQ_weighted = ifelse(is.na(BQ), 0, BQ) * (pourcentage_appartenance / 100)
  )

# 7. Regrouper par RTA et additionner les prédictions pondérées
rta_final_predictions <- rta_predictions_weighted %>%
  group_by(rta) %>%
  summarize(
    CPC = sum(CPC_weighted, na.rm = TRUE),
    LPC = sum(LPC_weighted, na.rm = TRUE),
    NDP = sum(NDP_weighted, na.rm = TRUE),
    GPC = sum(GPC_weighted, na.rm = TRUE),
    BQ = sum(BQ_weighted, na.rm = TRUE)
  )

# 8. Normaliser les pourcentages (somme = 1 pour chaque RTA)
rta_final_predictions <- rta_final_predictions %>%
  mutate(
    Total = CPC + LPC + NDP + GPC + BQ,
    CPC = CPC / Total,
    LPC = LPC / Total,
    NDP = NDP / Total,
    GPC = GPC / Total,
    BQ = BQ / Total
  ) %>%
  select(-Total)

# 9. Afficher les résultats
cat("\nPrédictions finales par RTA (premières lignes):\n")
print(head(rta_final_predictions))

# Vérification de la somme des pourcentages
rta_final_predictions %>%
  mutate(Total = CPC + LPC + NDP + GPC + BQ) %>%
  summarize(min_total = min(Total), max_total = max(Total)) %>%
  print()

# 10. Sauvegarder les résultats
output_file <- paste0(path_data, "rta_predictions_partis.csv")
write.csv(rta_final_predictions, output_file, row.names = FALSE)
cat("\nRésultats sauvegardés dans:", output_file, "\n")

# 11. Statistiques supplémentaires des prédictions par RTA
cat("\nStatistiques descriptives des prédictions par parti:\n")
summary_stats <- data.frame(
  Parti = c("CPC", "LPC", "NDP", "GPC", "BQ"),
  Moyenne = c(
    mean(rta_final_predictions$CPC),
    mean(rta_final_predictions$LPC),
    mean(rta_final_predictions$NDP),
    mean(rta_final_predictions$GPC),
    mean(rta_final_predictions$BQ)
  ),
  Médiane = c(
    median(rta_final_predictions$CPC),
    median(rta_final_predictions$LPC),
    median(rta_final_predictions$NDP),
    median(rta_final_predictions$GPC),
    median(rta_final_predictions$BQ)
  ),
  Min = c(
    min(rta_final_predictions$CPC),
    min(rta_final_predictions$LPC),
    min(rta_final_predictions$NDP),
    min(rta_final_predictions$GPC),
    min(rta_final_predictions$BQ)
  ),
  Max = c(
    max(rta_final_predictions$CPC),
    max(rta_final_predictions$LPC),
    max(rta_final_predictions$NDP),
    max(rta_final_predictions$GPC),
    max(rta_final_predictions$BQ)
  )
)

print(summary_stats)

cat("\nTraitement terminé avec succès!\n")
