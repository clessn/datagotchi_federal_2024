# Fonction pour comparer deux dataframes et générer un rapport détaillé
comparer_dataframes <- function(df_pilote, df_app) {
  # Liste pour stocker les résultats
  resultats <- list()
  
  # 1. Comparer les noms des variables
  vars_pilote <- names(df_pilote)
  vars_app <- names(df_app)
  
  # Variables communes
  vars_communes <- intersect(vars_pilote, vars_app)
  # Variables dans pilote mais pas dans app
  vars_manquantes_app <- setdiff(vars_pilote, vars_app)
  # Variables dans app mais pas dans pilote
  vars_extra_app <- setdiff(vars_app, vars_pilote)
  
  resultats$nombre_vars_communes <- length(vars_communes)
  resultats$nombre_vars_manquantes_app <- length(vars_manquantes_app)
  resultats$nombre_vars_extra_app <- length(vars_extra_app)
  resultats$vars_communes <- vars_communes
  resultats$vars_manquantes_app <- vars_manquantes_app
  resultats$vars_extra_app <- vars_extra_app
  
  # 2. Pour les variables communes, comparer les valeurs
  comparaison_valeurs <- data.frame(
    variable = character(),
    type_pilote = character(),
    type_app = character(),
    memes_valeurs = logical(),
    differences = character(),
    stringsAsFactors = FALSE
  )
  
  for (var in vars_communes) {
    # Types de variables
    type_pilote <- class(df_pilote[[var]])
    type_app <- class(df_app[[var]])
    
    # Vérifier si ce sont les mêmes types
    meme_type <- identical(type_pilote, type_app)
    
    # Vérifier si les valeurs possibles sont les mêmes (pour facteurs/caractères)
    if (is.factor(df_pilote[[var]]) && is.factor(df_app[[var]])) {
      valeurs_pilote <- levels(df_pilote[[var]])
      valeurs_app <- levels(df_app[[var]])
      memes_valeurs <- identical(sort(valeurs_pilote), sort(valeurs_app))
      differences <- ifelse(memes_valeurs, "",
                           paste("Pilote:", paste(setdiff(valeurs_pilote, valeurs_app), collapse=", "),
                                 "| App:", paste(setdiff(valeurs_app, valeurs_pilote), collapse=", ")))
    } else if (is.character(df_pilote[[var]]) && is.character(df_app[[var]])) {
      valeurs_pilote <- unique(df_pilote[[var]])
      valeurs_app <- unique(df_app[[var]])
      memes_valeurs <- identical(sort(valeurs_pilote), sort(valeurs_app))
      differences <- ifelse(memes_valeurs, "",
                           paste("Pilote:", paste(setdiff(valeurs_pilote, valeurs_app), collapse=", "),
                                 "| App:", paste(setdiff(valeurs_app, valeurs_pilote), collapse=", ")))
    } else if (is.numeric(df_pilote[[var]]) && is.numeric(df_app[[var]])) {
      # Pour les variables numériques, comparer les plages de valeurs
      range_pilote <- range(df_pilote[[var]], na.rm = TRUE)
      range_app <- range(df_app[[var]], na.rm = TRUE)
      memes_valeurs <- identical(range_pilote, range_app)
      differences <- ifelse(memes_valeurs, "",
                           paste("Pilote range:", paste(range_pilote, collapse="-"),
                                 "| App range:", paste(range_app, collapse="-")))
    } else {
      # Si types différents, ne pas comparer les valeurs
      memes_valeurs <- FALSE
      differences <- paste("Types différents: Pilote =", paste(type_pilote, collapse="/"),
                          "| App =", paste(type_app, collapse="/"))
    }
    
    comparaison_valeurs <- rbind(comparaison_valeurs, data.frame(
      variable = var,
      type_pilote = paste(type_pilote, collapse="/"),
      type_app = paste(type_app, collapse="/"),
      memes_types = meme_type,
      memes_valeurs = memes_valeurs,
      differences = differences,
      stringsAsFactors = FALSE
    ))
  }
  
  resultats$comparaison_valeurs <- comparaison_valeurs
  return(resultats)
}

# Charger les fichiers
# Remplacez par vos chemins de fichiers
pilote_clean <- readRDS("_SharedFolder_datagotchi_federal_2024/data/pilote/dataClean/datagotchi2025_canada_pilot_20250310.rds")
app_clean <- readRDS("_SharedFolder_datagotchi_federal_2024/data/app/dataClean/datagotchi2025_canada_appClustering_20250412.rds")

# Exécuter la comparaison
resultats <- comparer_dataframes(pilote_clean, app_clean)

# Afficher les résultats
cat("RAPPORT DE COMPARAISON DES VARIABLES\n")
cat("===================================\n\n")

cat("RÉSUMÉ:\n")
cat("Nombre de variables communes:", resultats$nombre_vars_communes, "\n")
cat("Nombre de variables présentes dans le pilote mais pas dans l'app:", resultats$nombre_vars_manquantes_app, "\n")
cat("Nombre de variables présentes dans l'app mais pas dans le pilote:", resultats$nombre_vars_extra_app, "\n\n")

if (length(resultats$vars_manquantes_app) > 0) {
  cat("VARIABLES MANQUANTES DANS L'APP:\n")
  cat(paste("- ", resultats$vars_manquantes_app), sep="\n")
  cat("\n")
}

if (length(resultats$vars_extra_app) > 0) {
  cat("VARIABLES SUPPLÉMENTAIRES DANS L'APP:\n")
  cat(paste("- ", resultats$vars_extra_app), sep="\n")
  cat("\n")
}

cat("COMPARAISON DES VARIABLES COMMUNES:\n")
print(resultats$comparaison_valeurs)

# Exporter les résultats en CSV


# Créer un résumé des problèmes potentiels
problemes <- subset(resultats$comparaison_valeurs, !memes_types | !memes_valeurs)
if (nrow(problemes) > 0) {
  cat("\nPROBLÈMES POTENTIELS DÉTECTÉS:\n")
  print(problemes)
}
