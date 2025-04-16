#' Calcul des contributions pré-calculées des RTA aux prédictions par parti et par région
#' 
#' Ce script calcule les contributions de chaque RTA aux prédictions des partis politiques
#' en tenant compte uniquement de sa région réelle. Corrigé pour détecter correctement
#' tous les formats d'interactions dans le modèle.
#'
#' Entrée :
#' - Prédictions par RTA (rta_predictions_partis.csv)
#' - Modèle final avec coefficients (finalmodel_withRTAPredictions_6Regions_2025-04-09.rds)
#'
#' Sortie :
#' - Table de contributions pré-calculées par région (rta_regional_precalculated_contributions.csv)
#'

# Charger les packages nécessaires
library(tidyverse)
library(nnet)  # Important pour le traitement des modèles multinomiaux

# Charger les prédictions RTA
rta_predictions <- read.csv("_SharedFolder_datagotchi_federal_2024/data/modele/rta_predictions_partis.csv",
                           stringsAsFactors = FALSE)

# Standardiser les RTA dans le fichier de prédictions
rta_predictions$rta <- toupper(rta_predictions$rta)

# Charger le modèle pour obtenir les coefficients
model_path <- "_SharedFolder_datagotchi_federal_2024/data/modele/finalmodel_withRTAPredictions_6Regions_2025-04-09.rds"
final_model <- readRDS(model_path)

# Vérifier si le modèle contient déjà les coefficients symétriques
if (!exists("sym_coef", where = final_model)) {
  cat("Calcul des coefficients symétriques...\n")
  
  # Extraire la matrice des coefficients originaux
  orig_coef <- coef(final_model)
  
  # Récupérer tous les niveaux de la variable réponse
  all_levels <- c(rownames(orig_coef), "bq") # Assurer que "bq" est inclus
  all_levels <- unique(all_levels)  # Éliminer les doublons potentiels
  
  # Créer une matrice complète pour les coefficients
  full_coef <- matrix(0, nrow = length(all_levels), ncol = ncol(orig_coef))
  rownames(full_coef) <- all_levels
  colnames(full_coef) <- colnames(orig_coef)
  
  # Remplir la matrice pour les niveaux non de référence
  for (lvl in rownames(orig_coef)) {
    full_coef[lvl, ] <- orig_coef[lvl, ]
  }
  
  # Pour chaque prédicteur, calculer la moyenne des coefficients sur tous les niveaux
  m <- colMeans(full_coef)
  
  # Reparamétrer de manière symétrique
  sym_coef <- full_coef - matrix(rep(m, each = length(all_levels)), nrow = length(all_levels))
  
  # Ajouter les coefficients symétriques au modèle
  final_model$sym_coef <- sym_coef
  
  # Sauvegarder le modèle modifié
  saveRDS(final_model, gsub("\\.rds$", "_with_sym_coef.rds", model_path))
  cat("Modèle sauvegardé avec coefficients symétriques\n")
} else {
  cat("Le modèle contient déjà des coefficients symétriques\n")
  sym_coef <- final_model$sym_coef
}

# Définir les régions à traiter
all_regions <- c("ontario", "quebec", "british_columbia", "prairie", "atlantic", "territories")

# Charger la configuration des régions (fusion potentielle)
region_mapping_path <- "_SharedFolder_datagotchi_federal_2024/data/modele/region_mapping_6Regions_2025-04-09.rds"
region_mapping <- NULL
if (file.exists(region_mapping_path)) {
  cat("Chargement de la configuration des régions...\n")
  region_mapping <- readRDS(region_mapping_path)
  print(region_mapping)
}

# Créer un mappage des régions originales vers les régions du modèle
region_model_mapping <- setNames(all_regions, all_regions)  # Par défaut, chaque région pointe vers elle-même

# Si le fichier de configuration existe, utiliser ce mappage
if (!is.null(region_mapping) && "model_region" %in% colnames(region_mapping)) {
  for (i in 1:nrow(region_mapping)) {
    original_region <- region_mapping$original_region[i]
    model_region <- region_mapping$model_region[i]
    region_model_mapping[original_region] <- model_region
  }
}

cat("Mappage des régions pour le modèle:\n")
for (r in names(region_model_mapping)) {
  cat("  ", r, "->", region_model_mapping[r], "\n")
}

# Obtenir les régions uniques utilisées dans le modèle
model_regions <- unique(region_model_mapping)
cat("Régions uniques du modèle:", paste(model_regions, collapse=", "), "\n")

# Récupérer les partis à utiliser - en minuscules comme dans les coefficients
parties <- c("lpc", "gpc", "cpc", "bq", "ndp")

# Extraire tous les noms de coefficients du modèle
coef_names <- colnames(sym_coef)
cat("Nombre total de coefficients:", length(coef_names), "\n")

# Modèles de variables RTA à rechercher
rta_var_patterns <- c("prediction_CPC", "prediction_LPC", "prediction_NDP", "prediction_GPC", "prediction_BQ")

# Organiser les variables d'interaction par région et type de prédiction
region_rta_interactions <- list()

for (region in model_regions) {
  region_pattern <- paste0("is_", region)
  region_interactions <- list()
  
  for (rta_pattern in rta_var_patterns) {
    # Chercher les interactions dans les deux formats possibles
    interaction_pattern1 <- paste0(region_pattern, ":", rta_pattern)
    interaction_pattern2 <- paste0(rta_pattern, ":", region_pattern)
    
    # Trouver les variables correspondantes (format 1: is_region:prediction_PARTI)
    matching_vars1 <- coef_names[grep(interaction_pattern1, coef_names, fixed = TRUE)]
    
    # Trouver les variables correspondantes (format 2: prediction_PARTI:is_region)
    matching_vars2 <- coef_names[grep(interaction_pattern2, coef_names, fixed = TRUE)]
    
    # Combiner les résultats
    all_matching_vars <- c(matching_vars1, matching_vars2)
    
    if (length(all_matching_vars) > 0) {
      # Extraire le type de prédiction (CPC, LPC, etc.) pour l'indexation
      prediction_type <- gsub("prediction_", "", rta_pattern)
      region_interactions[[prediction_type]] <- all_matching_vars[1]  # Prendre la première correspondance
      cat("Trouvé:", region, "-", prediction_type, "->", all_matching_vars[1], "\n")
    } else {
      cat("Non trouvé:", region, "-", rta_pattern, "\n")
    }
  }
  
  region_rta_interactions[[region]] <- region_interactions
}

# Vérification du résultat
cat("\nRésumé des interactions RTA par région:\n")
for (region in names(region_rta_interactions)) {
  cat("\nRégion:", region, "\n")
  interactions <- region_rta_interactions[[region]]
  if (length(interactions) == 0) {
    cat("  Aucune interaction trouvée\n")
  } else {
    for (party in names(interactions)) {
      cat("  Parti", party, "->", interactions[[party]], "\n")
    }
  }
}

# Fonction pour attribuer une région à chaque RTA basée sur le premier caractère
assign_region_to_rta <- function(rta) {
  first_letter <- substr(rta, 1, 1)
  
  if (first_letter %in% c("K", "L", "M", "N", "P")) {
    return("ontario")
  } else if (first_letter %in% c("G", "H", "J")) {
    return("quebec")
  } else if (first_letter %in% c("V")) {
    return("british_columbia") 
  } else if (first_letter %in% c("R", "S", "T")) {
    return("prairie")
  } else if (first_letter %in% c("A", "B", "C", "E")) {
    return("atlantic")
  } else if (first_letter %in% c("X", "Y")) {
    return("territories")
  } else {
    # Par défaut, retourner "ontario" si inconnu
    return("ontario")
  }
}

# Fonction pour calculer la contribution d'une RTA pour un parti dans sa région
calculate_contribution <- function(rta_row, party_name, region, sym_coef, region_rta_interactions) {
  contribution <- 0
  
  # Obtenir les interactions pour cette région
  region_interactions <- region_rta_interactions[[region]]
  
  if (length(region_interactions) > 0) {
    # Pour chaque type de prédiction RTA disponible dans cette région
    for (rta_party in names(region_interactions)) {
      # Obtenir le nom de la variable d'interaction
      interaction_var <- region_interactions[[rta_party]]
      
      # Obtenir la valeur de prédiction correspondante dans la ligne RTA
      rta_value <- rta_row[[rta_party]]
      
      if (!is.null(rta_value) && !is.na(rta_value)) {
        # Obtenir le coefficient pour ce parti et cette interaction
        coef_value <- sym_coef[party_name, interaction_var]
        
        # Calculer la contribution
        term_contribution <- rta_value * coef_value
        contribution <- contribution + term_contribution
        
        # Débogage des premiers termes
        if (contribution == term_contribution) {  # Si c'est le premier terme ajouté
          cat("DEBUG:", party_name, region, "- Variable:", interaction_var, 
              "- RTA value:", rta_value, 
              "- Coefficient:", coef_value, 
              "- Contribution:", term_contribution, "\n")
        }
      }
    }
  }
  
  return(contribution)
}

# Initialiser le dataframe pour stocker les contributions
all_contributions <- data.frame()

# Test de débogage sur les premières RTAs
cat("\nDÉBOGAGE: Test sur les 3 premières RTAs\n")
debug_sample <- head(rta_predictions, 3)

for (i in 1:nrow(debug_sample)) {
  rta_row <- debug_sample[i, ]
  rta_code <- rta_row$rta
  cat("\nRTA:", rta_code, "\n")
  
  # Déterminer la région originale de cette RTA
  orig_region <- assign_region_to_rta(rta_code)
  cat("Région originale:", orig_region, "\n")
  
  # Obtenir la région du modèle (qui peut être différente si fusion)
  model_region <- region_model_mapping[orig_region]
  cat("Région du modèle:", model_region, "\n")
  
  # Pour chaque parti
  for (party in parties) {
    if (party %in% rownames(sym_coef)) {
      # Calculer la contribution
      contribution <- calculate_contribution(
        rta_row, 
        party, 
        model_region, 
        sym_coef, 
        region_rta_interactions
      )
      
      cat("Parti", party, "- Contribution:", contribution, "\n")
    }
  }
}

# Traiter toutes les RTAs
cat("\nTraitement de toutes les RTAs...\n")

for (i in 1:nrow(rta_predictions)) {
  rta_row <- rta_predictions[i, ]
  rta_code <- rta_row$rta
  
  # Déterminer la région originale de cette RTA
  orig_region <- assign_region_to_rta(rta_code)
  
  # Obtenir la région du modèle (qui peut être différente si fusion)
  model_region <- region_model_mapping[orig_region]
  
  # Pour chaque parti
  for (party in parties) {
    if (party %in% rownames(sym_coef)) {
      contribution <- calculate_contribution(
        rta_row, 
        party, 
        model_region, 
        sym_coef, 
        region_rta_interactions
      )
      
      row <- data.frame(
        rta = rta_code,
        region = orig_region,
        model_region = model_region,
        party = party,
        contribution = contribution
      )
      
      all_contributions <- rbind(all_contributions, row)
    }
  }
  
  # Afficher la progression
  if (i %% 100 == 0 || i == nrow(rta_predictions)) {
    cat("Traitement des RTAs:", i, "/", nrow(rta_predictions), "\n")
  }
}

# Résumé des contributions par région et parti
region_summary <- all_contributions %>%
  group_by(region, party) %>%
  summarise(
    avg_contribution = mean(contribution, na.rm = TRUE),
    min_contribution = min(contribution, na.rm = TRUE),
    max_contribution = max(contribution, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(region, party)

cat("\nContributions moyennes par région et parti:\n")
print(region_summary)

# Analyse des contributions négatives pour LPC (si présentes)
if (any(region_summary$party == "lpc" & region_summary$avg_contribution < 0)) {
  cat("\nANALYSE DES CONTRIBUTIONS NÉGATIVES POUR LPC:\n")
  
  # Obtenir les régions avec contributions moyennes négatives pour LPC
  lpc_negative_regions <- region_summary %>% 
    filter(party == "lpc" & avg_contribution < 0) %>%
    pull(region)
  
  for (neg_region in lpc_negative_regions) {
    cat("\nRégion avec contribution moyenne négative pour LPC:", neg_region, "\n")
    
    # Obtenir la région du modèle correspondante
    model_reg <- region_model_mapping[neg_region]
    
    # Afficher les coefficients pertinents
    cat("Coefficients LPC pour la région", model_reg, ":\n")
    region_interactions <- region_rta_interactions[[model_reg]]
    
    for (rta_party in names(region_interactions)) {
      interaction_var <- region_interactions[[rta_party]]
      coef_value <- sym_coef["lpc", interaction_var]
      cat("  ", interaction_var, "=", coef_value, "\n")
    }
    
    # Obtenir quelques exemples de RTAs avec contributions fortement négatives
    neg_samples <- all_contributions %>%
      filter(region == neg_region, party == "lpc", contribution < 0) %>%
      arrange(contribution) %>%
      head(3)
    
    if (nrow(neg_samples) > 0) {
      cat("\nExemples de RTAs avec contributions les plus négatives:\n")
      print(neg_samples)
      
      if (nrow(neg_samples) > 0) {
        example_rta <- neg_samples$rta[1]
        cat("\nAnalyse détaillée pour RTA:", example_rta, "\n")
        
        # Retrouver les valeurs de prédiction originales
        orig_values <- rta_predictions[rta_predictions$rta == example_rta, ]
        cat("Valeurs de prédiction originales:\n")
        print(orig_values)
        
        # Montrer comment la contribution est calculée
        cat("\nCalcul détaillé de la contribution:\n")
        for (rta_party in names(region_interactions)) {
          interaction_var <- region_interactions[[rta_party]]
          rta_value <- orig_values[[rta_party]]
          coef_value <- sym_coef["lpc", interaction_var]
          term_contribution <- rta_value * coef_value
          
          cat("  ", rta_party, ": Valeur =", rta_value, 
              "× Coefficient =", coef_value, 
              "= Contribution", term_contribution, "\n")
        }
      }
    }
  }
  
  # ANALYSE APPROFONDIE: Si la signification des signes négatifs est normale ou problématique
  cat("\nINTERPRÉTATION DES CONTRIBUTIONS NÉGATIVES:\n")
  cat("Les contributions négatives pour LPC indiquent que dans certaines régions,\n")
  cat("les variables de RTA ont un effet négatif par rapport à la moyenne des effets\n")
  cat("pour tous les partis. Ceci est normal dans un modèle multinomial avec coefficients\n")
  cat("symétriques et reflète la dynamique régionale du vote.\n\n")
  
  cat("POUR LES DÉVELOPPEURS: Ces valeurs négatives doivent être conservées telles quelles\n")
  cat("car elles représentent la véritable contribution des RTA au modèle. Les transformer\n")
  cat("en valeurs positives fausserait les prédictions.\n\n")
}

# Vérifier si des régions ont toutes leurs contributions à zéro
zero_regions <- region_summary %>%
  group_by(region) %>%
  summarise(all_zero = all(abs(avg_contribution) < 1e-10), .groups = "drop") %>%
  filter(all_zero)

if (nrow(zero_regions) > 0) {
  cat("\nATTENTION: Les régions suivantes ont des contributions toutes à zéro:\n")
  print(zero_regions)
  cat("Il est possible que ces régions n'aient pas de variables d'interaction RTA dans le modèle.\n")
}

# Convertir en format large pour faciliter l'utilisation
contributions_wide <- all_contributions %>%
  select(rta, region, party, contribution) %>%
  pivot_wider(
    id_cols = c(rta, region),
    names_from = party,
    values_from = contribution
  )

# Calculer des valeurs par défaut pour chaque région
default_contributions <- data.frame()

for (region in names(region_model_mapping)) {
  # Calculer la moyenne des contributions pour cette région
  region_data <- all_contributions %>% 
    filter(region == region) %>%
    group_by(party) %>%
    summarise(avg_contribution = mean(contribution, na.rm = TRUE), .groups = "drop")
  
  default_row <- data.frame(
    rta = "DEFAULT",
    region = region
  )
  
  # Ajouter les contributions moyennes pour chaque parti
  for (party in parties) {
    party_row <- region_data %>% filter(party == !!party)
    if (nrow(party_row) > 0) {
      default_row[[party]] <- party_row$avg_contribution
    } else {
      default_row[[party]] <- 0
    }
  }
  
  default_contributions <- rbind(default_contributions, default_row)
}

# Combiner avec les contributions principales
contributions_final <- contributions_wide %>%
  bind_rows(default_contributions) %>%
  # S'assurer que toutes les colonnes des partis existent
  mutate(across(all_of(parties), ~ifelse(is.na(.), 0, .)))

# Sauvegarder le fichier final
output_file <- "_SharedFolder_datagotchi_federal_2024/data/modele/rta_regional_precalculated_contributions.csv"
write.csv(contributions_final, output_file, row.names = FALSE)

cat("\nFichier de contributions RTA sauvegardé:", output_file, "\n")
cat("Nombre total de RTAs:", nrow(rta_predictions), "\n")
cat("Nombre total de régions originales:", length(unique(contributions_final$region)), "\n")
cat("Nombre total de lignes (incluant les valeurs par défaut):", nrow(contributions_final), "\n")

# Création d'un fichier de diagnostic
diag_file <- "_SharedFolder_datagotchi_federal_2024/data/modele/rta_contributions_diagnostic.txt"
sink(diag_file)
cat("DIAGNOSTIC DES CONTRIBUTIONS RTA\n")
cat("================================\n\n")

cat("MODÈLE MULTINOMIAL AVEC INTERACTIONS RÉGIONALES\n")
cat("Ce modèle utilise uniquement des interactions entre régions et variables.\n")
cat("Les contributions RTA reflètent donc l'effet régional spécifique de chaque RTA.\n\n")

cat("Résumé des contributions par région et parti:\n")
print(region_summary)

cat("\nNombre de RTA par région:\n")
print(table(contributions_final$region[contributions_final$rta != "DEFAULT"]))

cat("\nValeurs par défaut par région:\n")
print(default_contributions)

cat("\nSIGNIFICATION DES VALEURS NÉGATIVES:\n")
cat("Les valeurs négatives sont normales et significatives dans ce modèle.\n")
cat("Elles indiquent un effet négatif relatif d'une RTA sur un parti spécifique\n")
cat("par rapport à la moyenne de tous les partis (coefficients symétriques).\n")

cat("\nRECOMMANDATION:\n")
cat("Conserver les valeurs négatives pour maintenir la cohérence du modèle.\n")
cat("Toutefois, si l'application nécessite des contributions positives,\n")
cat("il est préférable d'ajouter un décalage constant à toutes les valeurs.\n")

sink()
cat("\nFichier de diagnostic sauvegardé:", diag_file, "\n")
