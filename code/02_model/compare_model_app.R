# Analyse optimisée des corrections pour le modèle prédictif de vote
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(scales)
library(knitr)
library(grid)

# Analyse des corrections avec graphiques améliorés
analyze_corrections <- function(data) {
  # Dictionnaire des partis avec codes couleurs officiels
  party_names <- c(
    "1" = "PLC/LPC", 
    "2" = "PCC/CPC", 
    "3" = "NPD/NDP", 
    "4" = "BQ", 
    "5" = "PVC/GPC"
  )
  
  party_colors <- c(
    "1" = "#d71920", # Rouge (Libéral)
    "2" = "#0c2a51", # Bleu (Conservateur)
    "3" = "#f37021", # Orange (NPD)
    "4" = "#33b2cc", # Bleu clair (Bloc Québécois)
    "5" = "#3d9b35"  # Vert (Parti Vert)
  )
  
  # Étape 1: Identifier les corrections (où result ≠ correction et correction n'est pas 666)
  data$is_corrected <- !is.na(data$correction) & data$correction != 666 & data$result != data$correction
  
  # Étape 2: Calculer le taux de correction global
  correction_rate <- mean(data$is_corrected, na.rm = TRUE) * 100
  
  # Étape 3: Identifier les corrections avec valeur 665 à part
  corrections_with_665 <- data[!is.na(data$correction) & data$correction != 666 & data$result != data$correction, ]
  corrections_table <- table(corrections_with_665$result, corrections_with_665$correction)
  
  # Étape 4: Taux de correction par parti initial
  correction_by_party <- data %>%
    group_by(result) %>%
    summarise(
      total = n(),
      corrected = sum(is_corrected, na.rm = TRUE),
      correction_rate = (corrected / total) * 100,
      party_name = party_names[as.character(result[1])]
    )
  
  # Étape 5: Taux d'accord par parti
  # Le taux d'accord est le % de fois où l'utilisateur est d'accord avec la prédiction du modèle
  agreement_by_party <- data %>%
    group_by(result) %>%
    summarise(
      total = n(),
      agreed = sum(!is_corrected, na.rm = TRUE),
      agreement_rate = (agreed / total) * 100,
      party_name = party_names[as.character(result[1])]
    )
  
  # Étape 6: Matrice de transition pour les corrections (sans les 665)
  corrections_clean <- corrections_with_665 %>%
    filter(!is.na(correction) & correction %in% 1:5) %>%
    group_by(result, correction) %>%
    summarise(count = n(), .groups = "drop") %>%
    # Calculer pourcentage par parti initial
    group_by(result) %>%
    mutate(percentage = count / sum(count) * 100) %>%
    # Ajouter noms des partis
    mutate(
      result_name = party_names[as.character(result)],
      correction_name = party_names[as.character(correction)]
    )
  
  # Étape 7: Distribution des corrections par parti destination
  correction_destination <- corrections_with_665 %>%
    filter(!is.na(correction) & correction %in% 1:5) %>%
    group_by(correction) %>%
    summarise(
      count = n(),
      party_name = party_names[as.character(correction[1])]
    ) %>%
    mutate(percentage = count / sum(count) * 100)
  
  # Étape 8: Analyse des corrections par cause
  # Pour chaque parti initial, vers quels partis vont les corrections
  correction_flow <- corrections_with_665 %>%
    filter(!is.na(correction) & correction %in% 1:5) %>%
    group_by(result, correction) %>%
    summarise(count = n(), .groups = "drop") %>%
    mutate(
      from_party = party_names[as.character(result)],
      to_party = party_names[as.character(correction)]
    )
  
  # Étape 9: Créer visualisations améliorées
  
  # Graphique 1: Taux de correction global (pie chart)
  global_correction_data <- data.frame(
    Status = c("Corrigés", "Non corrigés"),
    Count = c(sum(data$is_corrected, na.rm = TRUE), 
              nrow(data) - sum(data$is_corrected, na.rm = TRUE)),
    stringsAsFactors = FALSE
  )
  global_correction_data$Percentage <- global_correction_data$Count / sum(global_correction_data$Count) * 100
  
  p_global_pie <- ggplot(global_correction_data, aes(x = "", y = Count, fill = Status)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    scale_fill_manual(values = c("Corrigés" = "#FF6B6B", "Non corrigés" = "#4ECDC4")) +
    geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
              position = position_stack(vjust = 0.5), color = "white", size = 5, fontface = "bold") +
    labs(
      title = "Taux global de correction des prédictions",
      fill = ""
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      legend.position = "bottom",
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
  
  # Graphique 2: Taux de correction par parti (barres horizontales)
  p_correction_by_party <- ggplot(correction_by_party, 
                               aes(x = reorder(party_name, correction_rate), 
                                   y = correction_rate, 
                                   fill = as.character(result))) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste0(round(correction_rate, 1), "%")), 
              hjust = -0.1, size = 4) +
    scale_fill_manual(values = party_colors) +
    labs(
      title = "Taux de correction par parti initialement prédit",
      subtitle = "Pourcentage des prédictions qui ont été corrigées par les utilisateurs",
      x = "",
      y = "Taux de correction (%)",
      fill = "Parti"
    ) +
    coord_flip() +
    scale_y_continuous(limits = c(0, max(correction_by_party$correction_rate) * 1.15)) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0, size = 16, face = "bold"),
      plot.subtitle = element_text(hjust = 0, size = 12),
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.text = element_text(size = 11),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
  
  # Graphique 3: Accord par parti (barres horizontales)
  p_agreement_by_party <- ggplot(agreement_by_party, 
                              aes(x = reorder(party_name, agreement_rate), 
                                  y = agreement_rate, 
                                  fill = as.character(result))) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste0(round(agreement_rate, 1), "%")), 
              hjust = -0.1, size = 4) +
    scale_fill_manual(values = party_colors) +
    labs(
      title = "Taux d'accord par parti initialement prédit",
      subtitle = "Pourcentage des prédictions acceptées par les utilisateurs",
      x = "",
      y = "Taux d'accord (%)",
      fill = "Parti"
    ) +
    coord_flip() +
    scale_y_continuous(limits = c(0, max(agreement_by_party$agreement_rate) * 1.15)) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0, size = 16, face = "bold"),
      plot.subtitle = element_text(hjust = 0, size = 12),
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.text = element_text(size = 11),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
  
  # Graphique 4: Matrice de confusion des corrections (heatmap améliorée)
  p_correction_matrix <- ggplot(corrections_clean, 
                             aes(x = correction_name, y = result_name, fill = percentage)) +
    geom_tile(color = "white", linewidth = 0.5) +  # Utiliser linewidth au lieu de size
    geom_text(aes(label = paste0(round(percentage, 1), "%")), 
              color = ifelse(corrections_clean$percentage > 30, "white", "black"), 
              size = 3.5, fontface = "bold") +
    scale_fill_gradient(low = "#E0F7FA", high = "#006064") +
    labs(
      title = "Direction des corrections par parti",
      subtitle = "De quel parti initial vers quel parti corrigé",
      x = "Parti après correction",
      y = "Parti prédit initialement",
      fill = "% des\ncorrections"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      axis.text = element_text(size = 11),
      axis.title = element_text(size = 12),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid = element_blank()
    )
  
  # Graphique 5: Distribution des prédictions (pie chart)
  result_counts <- table(data$result)
  result_distribution <- data.frame(
    Party_Code = as.integer(names(result_counts)),
    Party_Name = party_names[names(result_counts)],
    Count = as.vector(result_counts),
    stringsAsFactors = FALSE
  )
  result_distribution$Percentage <- result_distribution$Count / sum(result_distribution$Count) * 100
  
  p_result_distribution <- ggplot(result_distribution, 
                               aes(x = "", y = Count, fill = as.character(Party_Code))) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    scale_fill_manual(values = party_colors, 
                     labels = result_distribution$Party_Name) +
    geom_text(aes(label = paste0(Party_Name, "\n", round(Percentage, 1), "%")), 
              position = position_stack(vjust = 0.5), color = "white", size = 4, fontface = "bold") +
    labs(
      title = "Distribution des prédictions par parti",
      fill = "Parti"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      legend.position = "none",
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
  
  # Graphique 6: Direction des corrections (barplot)
  p_correction_flow <- ggplot(correction_flow, 
                           aes(x = from_party, y = count, fill = to_party)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = setNames(
      unlist(party_colors[as.character(1:5)]), 
      party_names[as.character(1:5)]
    )) +
    labs(
      title = "Flux des corrections par parti",
      subtitle = "Nombre de corrections selon le parti d'origine et de destination",
      x = "Parti prédit initialement",
      y = "Nombre de corrections",
      fill = "Corrigé vers"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
  
  # Étape 10: Afficher les résultats
  cat("=== ANALYSE DES CORRECTIONS DU MODÈLE PRÉDICTIF ===\n\n")
  cat(paste0("Nombre total d'observations: ", nrow(data), "\n"))
  cat(paste0("Nombre de corrections: ", sum(data$is_corrected, na.rm = TRUE), "\n"))
  cat(paste0("Taux de correction global: ", round(correction_rate, 2), "%\n\n"))
  
  cat("Distribution des prédictions par parti:\n")
  print(result_distribution %>% 
          select(Party_Code, Party_Name, Count, Percentage) %>%
          arrange(desc(Count)))
  
  cat("\nTaux de correction par parti:\n")
  print(correction_by_party %>% 
          select(party_name, total, corrected, correction_rate) %>%
          arrange(desc(correction_rate)))
  
  cat("\nDirection des corrections (nombre):\n")
  print(corrections_table)
  
  # Retourner les résultats
  return(list(
    correction_rate = correction_rate,
    correction_by_party = correction_by_party,
    agreement_by_party = agreement_by_party,
    corrections_matrix = corrections_clean,
    result_distribution = result_distribution,
    correction_flow = correction_flow,
    plots = list(
      global_pie = p_global_pie,
      correction_by_party = p_correction_by_party,
      agreement_by_party = p_agreement_by_party,
      correction_matrix = p_correction_matrix,
      result_distribution = p_result_distribution,
      correction_flow = p_correction_flow
    )
  ))
}

# Générer un rapport Markdown avec les analyses
generate_markdown_report <- function(results, output_file = "rapport_corrections.md") {
  # Créer le contenu du rapport Markdown
  md_content <- c(
    "# Rapport d'Analyse des Corrections du Modèle Prédictif",
    "",
    "## Résumé Global",
    "",
    paste0("- **Nombre total d'observations**: ", sum(results$result_distribution$Count), ""),
    paste0("- **Taux de correction global**: ", round(results$correction_rate, 1), "%"),
    paste0("- **Nombre de corrections**: ", sum(results$correction_by_party$corrected)),
    "",
    "## Distribution des Prédictions par Parti",
    "",
    "Le graphique ci-dessous montre la distribution des prédictions initiales du modèle par parti:",
    "",
    "![Distribution des prédictions](distribution_predictions.png)",
    "",
    "### Tableau des prédictions par parti",
    "",
    kable(results$result_distribution %>% 
            select(Party_Name, Count, Percentage) %>%
            arrange(desc(Count)) %>%
            rename(Parti = Party_Name, 
                  Nombre = Count, 
                  Pourcentage = Percentage) %>%
            mutate(Pourcentage = paste0(round(Pourcentage, 1), "%")), 
          format = "markdown"),
    "",
    "## Taux de Correction Global",
    "",
    "Le graphique ci-dessous montre le pourcentage global de prédictions qui ont été corrigées par les utilisateurs:",
    "",
    "![Taux de correction global](correction_global.png)",
    "",
    "## Taux de Correction par Parti",
    "",
    "Le graphique ci-dessous montre, pour chaque parti initialement prédit par le modèle, le pourcentage de prédictions qui ont été corrigées par les utilisateurs:",
    "",
    "![Taux de correction par parti](correction_par_parti.png)",
    "",
    "### Tableau des corrections par parti",
    "",
    kable(results$correction_by_party %>% 
            select(party_name, total, corrected, correction_rate) %>%
            arrange(desc(correction_rate)) %>%
            rename(Parti = party_name, 
                  `Total prédictions` = total, 
                  `Nombre corrigé` = corrected, 
                  `Taux de correction` = correction_rate) %>%
            mutate(`Taux de correction` = paste0(round(`Taux de correction`, 1), "%")), 
          format = "markdown"),
    "",
    "## Taux d'Accord par Parti",
    "",
    "Le graphique ci-dessous montre, pour chaque parti initialement prédit par le modèle, le pourcentage de prédictions qui ont été acceptées par les utilisateurs:",
    "",
    "![Taux d'accord par parti](accord_par_parti.png)",
    "",
    "### Tableau des accords par parti",
    "",
    kable(results$agreement_by_party %>% 
            select(party_name, total, agreed, agreement_rate) %>%
            arrange(desc(agreement_rate)) %>%
            rename(Parti = party_name, 
                  `Total prédictions` = total, 
                  `Nombre accepté` = agreed, 
                  `Taux d'accord` = agreement_rate) %>%
            mutate(`Taux d'accord` = paste0(round(`Taux d'accord`, 1), "%")), 
          format = "markdown"),
    "",
    "## Direction des Corrections",
    "",
    "Le graphique ci-dessous montre, pour chaque parti initialement prédit par le modèle, vers quels partis vont les corrections:",
    "",
    "![Direction des corrections](direction_corrections.png)",
    "",
    "Cette matrice de confusion des corrections permet de voir les tendances de correction. Par exemple:",
    "",
    # Générer quelques insights basés sur les données
    paste0("- Parmi les prédictions incorrectes de ", 
          results$corrections_matrix$result_name[1], 
          ", ", 
          round(results$corrections_matrix$percentage[1], 1), 
          "% ont été corrigées vers ", 
          results$corrections_matrix$correction_name[1]),
    "",
    "## Flux des Corrections entre Partis",
    "",
    "Le graphique ci-dessous montre le nombre de corrections par parti d'origine et de destination:",
    "",
    "![Flux des corrections](flux_corrections.png)",
    "",
    "## Conclusion",
    "",
    paste0("Le modèle prédictif a un taux de correction global de ", 
          round(results$correction_rate, 1), 
          "%. Les prédictions pour le parti ", 
          results$correction_by_party$party_name[which.max(results$correction_by_party$correction_rate)], 
          " sont les plus susceptibles d'être corrigées (", 
          round(max(results$correction_by_party$correction_rate), 1), 
          "%), tandis que les prédictions pour le parti ", 
          results$correction_by_party$party_name[which.min(results$correction_by_party$correction_rate)], 
          " sont les moins susceptibles d'être corrigées (", 
          round(min(results$correction_by_party$correction_rate), 1), 
          "%)."),
    "",
    "Cette analyse peut aider à améliorer le modèle prédictif en identifiant les partis pour lesquels le modèle a tendance à faire des erreurs et en comprenant comment les utilisateurs corrigent ces erreurs."
  )
  
  # Écrire le contenu dans un fichier markdown
  writeLines(md_content, output_file)
  cat("Rapport Markdown généré:", output_file, "\n")
}

# Export des résultats avec graphiques améliorés
export_enhanced_results <- function(results, output_dir = ".") {
  # Créer le répertoire si nécessaire
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Exporter les données en CSV
  write.csv(results$correction_by_party, 
            file.path(output_dir, "correction_by_party.csv"), 
            row.names = FALSE)
  
  write.csv(results$agreement_by_party, 
            file.path(output_dir, "agreement_by_party.csv"), 
            row.names = FALSE)
  
  write.csv(results$corrections_matrix, 
            file.path(output_dir, "correction_direction.csv"), 
            row.names = FALSE)
  
  write.csv(results$result_distribution, 
            file.path(output_dir, "result_distribution.csv"), 
            row.names = FALSE)
  
  # Exporter les graphiques individuels avec fond blanc
  ggsave(file.path(output_dir, "correction_global.png"), 
         results$plots$global_pie, 
         width = 10, height = 8, bg = "white")
  
  ggsave(file.path(output_dir, "correction_par_parti.png"), 
         results$plots$correction_by_party, 
         width = 10, height = 6, bg = "white")
  
  ggsave(file.path(output_dir, "accord_par_parti.png"), 
         results$plots$agreement_by_party, 
         width = 10, height = 6, bg = "white")
  
  ggsave(file.path(output_dir, "direction_corrections.png"), 
         results$plots$correction_matrix, 
         width = 10, height = 8, bg = "white")
  
  ggsave(file.path(output_dir, "distribution_predictions.png"), 
         results$plots$result_distribution, 
         width = 10, height = 8, bg = "white")
  
  ggsave(file.path(output_dir, "flux_corrections.png"), 
         results$plots$correction_flow, 
         width = 12, height = 8, bg = "white")
  
  # Générer un rapport combiné en PDF avec tous les graphiques
  pdf(file.path(output_dir, "rapport_complet_corrections.pdf"), width = 11, height = 8.5)
  
  # Page de titre
  grid.newpage()
  grid.text("ANALYSE DES CORRECTIONS DU MODÈLE PRÉDICTIF", 
           x = 0.5, y = 0.7, 
           gp = gpar(fontsize = 24, fontface = "bold"))
  grid.text(paste("Date d'analyse:", format(Sys.Date(), "%d %B %Y")), 
           x = 0.5, y = 0.6, 
           gp = gpar(fontsize = 14))
  
  # Graphiques
  grid.arrange(results$plots$global_pie, results$plots$result_distribution, ncol = 2)
  grid.arrange(results$plots$correction_by_party, ncol = 1)
  grid.arrange(results$plots$agreement_by_party, ncol = 1)
  grid.arrange(results$plots$correction_matrix, ncol = 1)
  grid.arrange(results$plots$correction_flow, ncol = 1)
  
  dev.off()
  
  # Générer un rapport Markdown
  generate_markdown_report(results, file.path(output_dir, "rapport_corrections.md"))
  
  cat("Résultats exportés dans le répertoire:", output_dir, "\n")
}

# Exemple d'utilisation:
# Charger les données
data_model <- read.csv("_SharedFolder_datagotchi_federal_2024/data/app/dataRaw/ECAN25_Data_20250305-20250318.csv")

# Exécuter l'analyse des corrections
results <- analyze_corrections(data_model)

# Exporter les résultats
# Correction de l'appel de fonction ici
export_enhanced_results(results, "_SharedFolder_datagotchi_federal_2024/reports/correction_analysis")

# Pour visualiser directement les graphiques dans R
print(results$plots$global_pie)
print(results$plots$correction_by_party)
print(results$plots$agreement_by_party)
print(results$plots$correction_matrix)
print(results$plots$result_distribution)
print(results$plots$correction_flow)