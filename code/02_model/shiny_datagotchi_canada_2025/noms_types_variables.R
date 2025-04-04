# Script pour extraire les variables et leurs valeurs d'un modèle multinomial nnet

# Charger les bibliothèques nécessaires
library(nnet)       # Pour les modèles multinomiaux
library(broom)      # Pour tidy output
library(dplyr)      # Pour la manipulation de données
library(purrr)      # Pour les opérations fonctionnelles

# Charger le modèle
modele_path <- "code/02_model/shiny_datagotchi_canada_2025/data/finalmodel_withRTAPredictions_2025-04-15.rds"
modele <- readRDS(modele_path)

# Vérifier le type de modèle
cat("Type de modèle:", class(modele), "\n\n")

# Fonction pour extraire et afficher les variables et leurs valeurs possibles
extraire_variables_modele <- function(modele) {
  # Si c'est un modèle nnet/multinom
  if ("multinom" %in% class(modele)) {
    # Obtenir les termes du modèle
    termes <- terms(modele)
    
    # Extraire les noms des variables
    variables <- attr(termes, "term.labels")
    cat("Variables utilisées dans le modèle:\n")
    print(variables)
    cat("\n")
    
    # Extraire la formule du modèle
    cat("Formule du modèle:\n")
    print(formula(modele))
    cat("\n")
    
    # Extraire les données utilisées pour ajuster le modèle
    if (!is.null(modele$model)) {
      donnees_modele <- modele$model
      cat("Structure des données du modèle:\n")
      str(donnees_modele)
      cat("\n")
      
      # Pour chaque variable, afficher ses valeurs uniques
      cat("Valeurs possibles pour chaque variable:\n")
      for (var in variables) {
        if (var %in% names(donnees_modele)) {
          # Gérer les interactions (termes avec :)
          if (grepl(":", var)) {
            cat(paste0("Variable d'interaction: ", var, "\n"))
            termes_interaction <- unlist(strsplit(var, ":"))
            for (terme in termes_interaction) {
              if (terme %in% names(donnees_modele)) {
                cat(paste0("  Composante: ", terme, "\n"))
                if (is.factor(donnees_modele[[terme]])) {
                  cat("    Valeurs (factor):", paste(levels(donnees_modele[[terme]]), collapse=", "), "\n")
                } else {
                  val_uniques <- unique(donnees_modele[[terme]])
                  if (length(val_uniques) > 10) {
                    cat("    Type:", class(donnees_modele[[terme]]), "- Trop de valeurs uniques (", length(val_uniques), ")\n")
                    cat("    Min:", min(val_uniques, na.rm=TRUE), ", Max:", max(val_uniques, na.rm=TRUE), "\n")
                  } else {
                    cat("    Valeurs:", paste(sort(val_uniques), collapse=", "), "\n")
                  }
                }
              }
            }
          } else {
            cat(paste0("Variable: ", var, "\n"))
            if (is.factor(donnees_modele[[var]])) {
              cat("  Valeurs (factor):", paste(levels(donnees_modele[[var]]), collapse=", "), "\n")
            } else {
              val_uniques <- unique(donnees_modele[[var]])
              if (length(val_uniques) > 10) {
                cat("  Type:", class(donnees_modele[[var]]), "- Trop de valeurs uniques (", length(val_uniques), ")\n")
                cat("  Min:", min(val_uniques, na.rm=TRUE), ", Max:", max(val_uniques, na.rm=TRUE), "\n")
              } else {
                cat("  Valeurs:", paste(sort(val_uniques), collapse=", "), "\n")
              }
            }
          }
        }
      }
    } else {
      cat("Les données du modèle ne sont pas stockées dans l'objet modèle.\n")
      
      # Extraire les coefficients pour voir les niveaux des facteurs
      coefs <- coef(modele)
      cat("Dimensions des coefficients:", dim(coefs), "\n")
      
      # Essayer de déduire les niveaux des facteurs à partir des noms de colonnes
      coef_names <- colnames(coefs)
      var_levels <- list()
      
      for (var in variables) {
        if (grepl(":", var)) {
          # Traiter les interactions séparément
          next
        }
        
        # Rechercher les préfixes qui pourraient indiquer les niveaux des facteurs
        pattern <- paste0("^", var)
        matching_cols <- grep(pattern, coef_names, value = TRUE)
        
        if (length(matching_cols) > 0) {
          # Extraire les niveaux potentiels
          potential_levels <- sub(pattern, "", matching_cols)
          # Filtrer les entrées vides ou commençant par des caractères non-alpha
          levels_clean <- potential_levels[nzchar(potential_levels) & grepl("^[A-Za-z]", potential_levels)]
          
          if (length(levels_clean) > 0) {
            var_levels[[var]] <- levels_clean
            cat("Variable (déduite des coefficients):", var, "\n")
            cat("  Valeurs possibles:", paste(levels_clean, collapse=", "), "\n")
          }
        }
      }
    }
    
    # Extraire les niveaux de la variable réponse (classes)
    cat("\nClasses de la variable réponse:\n")
    if (!is.null(modele$lev)) {
      print(modele$lev)
    } else {
      cat("Non disponible\n")
    }
    
    # Afficher les coefficients
    cat("\nCoefficients du modèle (premières lignes):\n")
    print(head(coef(modele)))
    
    # Statistiques du modèle
    cat("\nStatistiques du modèle:\n")
    if (!is.null(modele$deviance)) cat("Déviance:", modele$deviance, "\n")
    if (!is.null(modele$AIC)) cat("AIC:", modele$AIC, "\n")
    if (!is.null(modele$edf)) cat("Degrés de liberté effectifs:", modele$edf, "\n")
  } else {
    cat("Ce n'est pas un modèle multinomial nnet. Type détecté:", class(modele), "\n")
    
    # Tenter d'extraire les informations disponibles
    if (inherits(modele, "model")) {
      cat("Variables dans le modèle:\n")
      print(names(modele$model))
      
      cat("\nFormule:\n")
      print(formula(modele))
    }
  }
}

# Exécuter la fonction
extraire_variables_modele(modele)

# Bonus: si le modèle a des prédictions RTA comme mentionné dans le nom du fichier
if ("predictions" %in% names(modele) || "RTAPredictions" %in% names(modele)) {
  cat("\nInformations sur les prédictions RTA:\n")
  pred_name <- if ("predictions" %in% names(modele)) "predictions" else "RTAPredictions"
  str(modele[[pred_name]])
}