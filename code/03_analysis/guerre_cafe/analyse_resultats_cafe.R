library(dplyr)
library(tidyr)
library(knitr)
library(readr)

# Chargement des données
data <- readRDS("_SharedFolder_datagotchi_federal_2024/data/app/dataClean/datagotchi2025_canada_appPonderee_20250323.rds")

# Renommage pour cohérence
data <- data %>%
  rename(id_riding = ses_riding_id)

# Vérifier le taux de disponibilité des IDs de circonscription
matched_count <- sum(!is.na(data$id_riding))
total_count <- nrow(data)
match_rate <- matched_count / total_count * 100
cat("====== STATISTIQUES GÉNÉRALES ======\n")
cat(paste("Nombre total de répondants:", format(total_count, big.mark = " "), "\n"))
cat(paste("Nombre de répondants avec une circonscription identifiée:", format(matched_count, big.mark = " "), "\n"))
cat(paste("Taux de disponibilité des circonscriptions:", round(match_rate, 2), "%\n\n"))

# S'assurer que la variable weight existe
if(!"weight" %in% names(data)) {
  data$weight <- 1
}

# 1. RÉSULTATS NATIONAUX

# Moyennes nationales des chaînes de café
national_averages <- data %>%
  summarize(
    sum_weight = sum(weight, na.rm = TRUE),
    tim_hortons_avg = sum((lifestyle_consCoffeeTimHortons == 1) * weight, na.rm = TRUE) / sum_weight * 100,
    mcdo_avg = sum((lifestyle_consCoffeeMcDo == 1) * weight, na.rm = TRUE) / sum_weight * 100,
    starbucks_avg = sum((lifestyle_consCoffeeStarbucks == 1) * weight, na.rm = TRUE) / sum_weight * 100,
    secondcup_avg = sum((lifestyle_consCoffeeSecondCup == 1) * weight, na.rm = TRUE) / sum_weight * 100
  )

cat("====== MOYENNES NATIONALES DE CONSOMMATION ======\n")
cat(paste("Tim Hortons:", round(national_averages$tim_hortons_avg, 1), "%\n"))
cat(paste("McDonald's:", round(national_averages$mcdo_avg, 1), "%\n"))
cat(paste("Starbucks:", round(national_averages$starbucks_avg, 1), "%\n"))
cat(paste("Second Cup:", round(national_averages$secondcup_avg, 1), "%\n\n"))

# 2. RÉPARTITION PAR CIRCONSCRIPTION

# Nombre de circonscriptions où chaque chaîne domine
coffee_battle_by_riding <- data %>%
  filter(!is.na(id_riding)) %>%
  group_by(id_riding) %>%
  summarize(
    sum_weight = sum(weight, na.rm = TRUE),
    tim_fans_pct = sum((lifestyle_consCoffeeTimHortons == 1) * weight, na.rm = TRUE) / sum_weight * 100,
    mcdo_fans_pct = sum((lifestyle_consCoffeeMcDo == 1) * weight, na.rm = TRUE) / sum_weight * 100,
    starbucks_fans_pct = sum((lifestyle_consCoffeeStarbucks == 1) * weight, na.rm = TRUE) / sum_weight * 100,
    secondcup_fans_pct = sum((lifestyle_consCoffeeSecondCup == 1) * weight, na.rm = TRUE) / sum_weight * 100,
    n_people = n()
  ) %>%
  ungroup() %>%
  mutate(
    dominant_chain = case_when(
      tim_fans_pct >= mcdo_fans_pct & tim_fans_pct >= starbucks_fans_pct & tim_fans_pct >= secondcup_fans_pct ~ "Tim Hortons",
      mcdo_fans_pct >= tim_fans_pct & mcdo_fans_pct >= starbucks_fans_pct & mcdo_fans_pct >= secondcup_fans_pct ~ "McDonald's",
      starbucks_fans_pct >= tim_fans_pct & starbucks_fans_pct >= mcdo_fans_pct & starbucks_fans_pct >= secondcup_fans_pct ~ "Starbucks",
      secondcup_fans_pct >= tim_fans_pct & secondcup_fans_pct >= mcdo_fans_pct & secondcup_fans_pct >= starbucks_fans_pct ~ "Second Cup",
      TRUE ~ "Égalité"
    )
  )

# Compter les circonscriptions par chaîne dominante
dominant_counts <- coffee_battle_by_riding %>%
  count(dominant_chain) %>%
  mutate(percentage = n / sum(n) * 100)

cat("====== RÉPARTITION DES CIRCONSCRIPTIONS PAR CHAÎNE DOMINANTE ======\n")
for (i in 1:nrow(dominant_counts)) {
  cat(paste(dominant_counts$dominant_chain[i], ":", dominant_counts$n[i], 
            "circonscriptions (", round(dominant_counts$percentage[i], 1), "%)\n"))
}
cat("\n")

# 3. ÉCARTS PAR PARTI POLITIQUE

# Calcul des écarts par parti politique
coffee_by_party <- data %>%
  filter(!is.na(dv_voteChoice)) %>%
  filter(dv_voteChoice %in% c("lpc", "cpc", "ndp", "bq", "gpc")) %>%
  group_by(dv_voteChoice) %>%
  summarize(
    sum_weight = sum(weight, na.rm = TRUE),
    tim_fans_pct = sum((lifestyle_consCoffeeTimHortons == 1) * weight, na.rm = TRUE) / sum_weight * 100,
    mcdo_fans_pct = sum((lifestyle_consCoffeeMcDo == 1) * weight, na.rm = TRUE) / sum_weight * 100,
    starbucks_fans_pct = sum((lifestyle_consCoffeeStarbucks == 1) * weight, na.rm = TRUE) / sum_weight * 100,
    n_people = n()
  ) %>%
  ungroup() %>%
  mutate(
    party_name = case_when(
      dv_voteChoice == "lpc" ~ "Parti libéral",
      dv_voteChoice == "cpc" ~ "Parti conservateur",
      dv_voteChoice == "ndp" ~ "NPD",
      dv_voteChoice == "bq" ~ "Bloc Québécois",
      dv_voteChoice == "gpc" ~ "Parti vert",
      TRUE ~ NA_character_
    ),
    tim_deviation = tim_fans_pct - national_averages$tim_hortons_avg,
    mcdo_deviation = mcdo_fans_pct - national_averages$mcdo_avg,
    starbucks_deviation = starbucks_fans_pct - national_averages$starbucks_avg
  ) %>%
  select(party_name, tim_fans_pct, mcdo_fans_pct, starbucks_fans_pct, 
         tim_deviation, mcdo_deviation, starbucks_deviation, n_people)

# Ordonner selon l'axe politique (conservateur à gauche progressive)
party_order <- c("Parti conservateur", "Parti libéral", "Bloc Québécois", "NPD", "Parti vert")
coffee_by_party <- coffee_by_party %>%
  arrange(match(party_name, party_order))

cat("====== CONSOMMATION PAR AFFILIATION POLITIQUE ======\n")
cat("--- Pourcentages absolus ---\n")
for (i in 1:nrow(coffee_by_party)) {
  cat(paste(coffee_by_party$party_name[i], ":\n"))
  cat(paste("  Tim Hortons:", round(coffee_by_party$tim_fans_pct[i], 1), "%\n"))
  cat(paste("  McDonald's:", round(coffee_by_party$mcdo_fans_pct[i], 1), "%\n"))
  cat(paste("  Starbucks:", round(coffee_by_party$starbucks_fans_pct[i], 1), "%\n"))
  cat(paste("  Nombre de répondants:", format(coffee_by_party$n_people[i], big.mark = " "), "\n"))
}
cat("\n")

cat("--- Écarts par rapport à la moyenne nationale (points de %) ---\n")
for (i in 1:nrow(coffee_by_party)) {
  cat(paste(coffee_by_party$party_name[i], ":\n"))
  cat(paste("  Tim Hortons:", sprintf("%+.1f", round(coffee_by_party$tim_deviation[i], 1)), "points\n"))
  cat(paste("  McDonald's:", sprintf("%+.1f", round(coffee_by_party$mcdo_deviation[i], 1)), "points\n"))
  cat(paste("  Starbucks:", sprintf("%+.1f", round(coffee_by_party$starbucks_deviation[i], 1)), "points\n"))
}
cat("\n")

# 4. CHAÎNES DOMINANTES PAR RÉGION URBAINE

# Pour les principales villes
main_regions <- c("montreal", "toronto", "vancouver", "quebec_city")
region_names <- c("Montréal", "Toronto", "Vancouver", "Québec")

# Créer une fonction pour obtenir les résultats par région
get_region_results <- function(region_id) {
  # Supposons que nous pouvons extraire les données par région urbaine
  # (Cette partie est simplifiée car nous n'avons pas la fonction crop_map)
  # Nous allons simplement filtrer par provinces pour des résultats approximatifs
  
  region_data <- switch(region_id,
    "montreal" = data %>% filter(ses_province == "QC" & grepl("Montreal", ses_region, ignore.case = TRUE)),
    "toronto" = data %>% filter(ses_province == "ON" & grepl("Toronto|GTA", ses_region, ignore.case = TRUE)),
    "vancouver" = data %>% filter(ses_province == "BC" & grepl("Vancouver", ses_region, ignore.case = TRUE)),
    "quebec_city" = data %>% filter(ses_province == "QC" & grepl("Quebec City|Québec", ses_region, ignore.case = TRUE)),
    data # default case, though it shouldn't happen
  )
  
  # Si nous n'avons pas assez de données pour la région, retournons NA
  if (nrow(region_data) < 30) {
    return(data.frame(
      region = NA,
      tim_fans_pct = NA,
      mcdo_fans_pct = NA,
      starbucks_fans_pct = NA,
      secondcup_fans_pct = NA,
      dominant_chain = NA
    ))
  }
  
  # Calculer les pourcentages de fans par région
  region_results <- region_data %>%
    summarize(
      sum_weight = sum(weight, na.rm = TRUE),
      tim_fans_pct = sum((lifestyle_consCoffeeTimHortons == 1) * weight, na.rm = TRUE) / sum_weight * 100,
      mcdo_fans_pct = sum((lifestyle_consCoffeeMcDo == 1) * weight, na.rm = TRUE) / sum_weight * 100,
      starbucks_fans_pct = sum((lifestyle_consCoffeeStarbucks == 1) * weight, na.rm = TRUE) / sum_weight * 100,
      secondcup_fans_pct = sum((lifestyle_consCoffeeSecondCup == 1) * weight, na.rm = TRUE) / sum_weight * 100,
      n_respondents = n()
    ) %>%
    mutate(
      dominant_chain = case_when(
        tim_fans_pct >= mcdo_fans_pct & tim_fans_pct >= starbucks_fans_pct & tim_fans_pct >= secondcup_fans_pct ~ "Tim Hortons",
        mcdo_fans_pct >= tim_fans_pct & mcdo_fans_pct >= starbucks_fans_pct & mcdo_fans_pct >= secondcup_fans_pct ~ "McDonald's",
        starbucks_fans_pct >= tim_fans_pct & starbucks_fans_pct >= mcdo_fans_pct & starbucks_fans_pct >= secondcup_fans_pct ~ "Starbucks",
        secondcup_fans_pct >= tim_fans_pct & secondcup_fans_pct >= mcdo_fans_pct & secondcup_fans_pct >= starbucks_fans_pct ~ "Second Cup",
        TRUE ~ "Égalité"
      )
    )
  
  return(region_results)
}

cat("====== RÉSULTATS PAR RÉGION URBAINE ======\n")
for (i in 1:length(main_regions)) {
  region_results <- get_region_results(main_regions[i])
  cat(paste("--- ", region_names[i], " ---\n"))
  cat(paste("Tim Hortons:", round(region_results$tim_fans_pct, 1), "%\n"))
  cat(paste("McDonald's:", round(region_results$mcdo_fans_pct, 1), "%\n"))
  cat(paste("Starbucks:", round(region_results$starbucks_fans_pct, 1), "%\n"))
  cat(paste("Second Cup:", round(region_results$secondcup_fans_pct, 1), "%\n"))
  cat(paste("Chaîne dominante:", region_results$dominant_chain, "\n"))
  cat(paste("Nombre de répondants:", format(region_results$n_respondents, big.mark = " "), "\n\n"))
}

# 5. EXPORTER LES RÉSULTATS DANS UN FICHIER CSV POUR RÉFÉRENCE FUTURE

# Préparons une synthèse des résultats nationaux
national_summary <- data.frame(
  metric = c("Taux de consommation Tim Hortons", 
             "Taux de consommation McDonald's", 
             "Taux de consommation Starbucks",
             "Taux de consommation Second Cup"),
  value = c(round(national_averages$tim_hortons_avg, 1),
            round(national_averages$mcdo_avg, 1),
            round(national_averages$starbucks_avg, 1),
            round(national_averages$secondcup_avg, 1)),
  unit = c("%", "%", "%", "%")
)

# Sauvegarder les résultats
write_csv(national_summary, "_SharedFolder_datagotchi_federal_2024/reports/resultats_nationaux_cafe.csv")
write_csv(dominant_counts, "_SharedFolder_datagotchi_federal_2024/reports/repartition_circonscriptions_cafe.csv")
write_csv(coffee_by_party, "_SharedFolder_datagotchi_federal_2024/reports/resultats_politiques_cafe.csv")

cat("Fichiers CSV des résultats créés dans le dossier '_SharedFolder_datagotchi_federal_2024/reports/'\n")