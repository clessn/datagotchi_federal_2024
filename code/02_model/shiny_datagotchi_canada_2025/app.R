library(shiny)
library(caret)
library(tidyverse)
library(nnet)
library(rsconnect)

# ------------------------------------------------------------------------
# 1. Chargement du modèle final (chemin relatif)
# ------------------------------------------------------------------------
final_model <- readRDS("data/finalmodel_withRTAPredictions_2025-04-15.rds")

# Charger les contributions RTA pré-calculées au lieu des prédictions brutes
rta_contributions <- read.csv("data/rta_precalculated_contributions.csv", stringsAsFactors = FALSE)

# Si le fichier des contributions RTA n'existe pas, on crée une version vide pour éviter les erreurs
if (!file.exists("data/rta_precalculated_contributions.csv")) {
  # Créer un dataframe vide avec les colonnes nécessaires
  rta_contributions <- data.frame(
    rta = character(),
    bq = numeric(),
    cpc = numeric(),
    lpc = numeric(),
    ndp = numeric(),
    gpc = numeric(),
    stringsAsFactors = FALSE
  )
  # Ajouter une ligne DEFAULT avec des valeurs nulles
  rta_contributions <- rbind(rta_contributions, 
                            data.frame(rta = "DEFAULT", bq = 0, cpc = 0, lpc = 0, ndp = 0, gpc = 0))
  # Sauvegarder le fichier vide
  write.csv(rta_contributions, "data/rta_precalculated_contributions.csv", row.names = FALSE)
}

# S'assurer que les RTA sont en majuscules
rta_contributions$rta <- toupper(rta_contributions$rta)

# Obtenir les valeurs par défaut (ligne DEFAULT)
default_contributions <- rta_contributions %>%
  filter(rta == "DEFAULT") %>%
  select(-rta)

# Si pas de ligne DEFAULT, créer des valeurs par défaut (zéro)
if(nrow(default_contributions) == 0) {
  party_cols <- setdiff(names(rta_contributions), "rta")
  default_contributions <- as.data.frame(matrix(0, nrow = 1, ncol = length(party_cols)))
  names(default_contributions) <- party_cols
}

# ------------------------------------------------------------------------
# 2. Définition des variables socio-démographiques (sans variables RTA)
# ------------------------------------------------------------------------
socio_vars <- c(
  "ses_region", "ses_immigrant", "lifestyle_typeTransport",
  "lifestyle_consClothes", "lifestyle_exercise", "lifestyle_eatMeatFreq",
  "lifestyle_favAlcool", "lifestyle_consCoffee", "ses_language",
  "lifestyle_smokeFreq", "ses_age", "ses_dwelling_cat", "ses_ethnicityWhite",
  "ses_sexOrientationHetero", "ses_genderFemale", "lifestyle_clothingStyleGroups",
  "lifestyle_goHuntingFreq_numeric", "lifestyle_goFishingFreq_bin",
  "lifestyle_goMuseumsFreq_bin", "lifestyle_volunteeringFreq",
  "lifestyle_motorizedActFreq_bin", "lifestyle_hasTattoos", "ses_educ",
  "ses_income3Cat", "lifestyle_ownPet_bin"
)

# Liste des variables RTA dont nous avons besoin pour la prédiction
rta_vars <- c("prediction_CPC", "prediction_LPC", "prediction_NDP", "prediction_GPC", "prediction_BQ")

# Définition manuelle des choix possibles pour chaque variable
possible_choices <- list(
  ses_region = c("prairie", "british_columbia", "atlantic", "ontario", "quebec", "territories"),
  ses_immigrant = c(0, 1),
  lifestyle_typeTransport = c("active_transport", "car", "shared_transport"),
  lifestyle_consClothes = c("large_retailers", "small_local_store", "online_store", "other"),
  lifestyle_exercise = c("gym", "i_do_not_exercise", "other", "play_a_team_sport", "run", "swimming", "walk", "yoga"),
  lifestyle_eatMeatFreq = c(0, 0.33, 0.67, 1),
  lifestyle_favAlcool = c("beer", "cocktail", "dont_drink", "spirits", "wine"),
  lifestyle_consCoffee = c("tim_hortons", "starbucks", "second_cup", "mcdonalds", "other", "independent", "no_coffee"),
  ses_language = c("english", "french", "other"),
  lifestyle_smokeFreq = c(0, 0.167, 0.333, 0.5, 0.667, 0.833, 1),
  ses_age = 18:90,
  ses_dwelling_cat = c("stand_alone_house", "townhouse", "duplex", "apartment_complex", "high_rise_apartment", "mobile_home", "other"),
  ses_ethnicityWhite = c(0, 1),
  ses_sexOrientationHetero = c(0, 1),
  ses_genderFemale = c(0, 1),
  lifestyle_clothingStyleGroups = c("easygoing", "edgy", "formal", "other"),
  lifestyle_goHuntingFreq_numeric = c(0, 0.25, 0.5, 0.75, 1),
  lifestyle_goFishingFreq_bin = c(0, 1),
  lifestyle_goMuseumsFreq_bin = c(0, 1),
  lifestyle_volunteeringFreq = 1:5,
  lifestyle_motorizedActFreq_bin = c(0, 1),
  lifestyle_hasTattoos = c(0, 1),
  ses_educ = c("no_schooling", "elementary_school", "high_school", "technical_community_cegep", "bachelor", "masters", "doctorate"),
  ses_income3Cat = c("High", "Low", "Mid"),
  lifestyle_ownPet_bin = c(0, 1)
)

# ------------------------------------------------------------------------
# 3. Interface utilisateur
# ------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Prédiction du Choix de Vote"),
  sidebarLayout(
    sidebarPanel(
      # Nouvel input pour le code postal
      textInput("postalCode", "Code postal (pour prédictions RTA)", ""),
      # Génération dynamique des inputs à partir de possible_choices
      uiOutput("dynamic_inputs"),
      actionButton("predictBtn", "Prédire")
    ),
    mainPanel(
      h3("Résultat de la prédiction"),
      verbatimTextOutput("prediction"),
      h4("Probabilités associées"),
      tableOutput("probabilities"),
      h4("Détails RTA"),
      verbatimTextOutput("rta_details"),
      h4("Logs de débogage"),
      verbatimTextOutput("debug_log")
    )
  )
)

# ------------------------------------------------------------------------
# 4. Serveur
# ------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Variables réactives pour stocker les logs de débogage
  debug_logs <- reactiveVal("")
  
  # Fonction pour ajouter des logs de débogage
  add_log <- function(message) {
    current_logs <- debug_logs()
    new_logs <- paste(current_logs, message, sep = "\n")
    debug_logs(new_logs)
  }
  
  # Affichage des logs de débogage
  output$debug_log <- renderPrint({
    debug_logs()
  })
  
  # Création dynamique des inputs pour chaque variable
  output$dynamic_inputs <- renderUI({
    inputs <- lapply(socio_vars, function(v) {
      if (!is.numeric(possible_choices[[v]])) {
        selectInput(inputId = v, label = v,
                    choices = possible_choices[[v]],
                    selected = possible_choices[[v]][1])
      } else {
        # Pour les variables numériques, si le nombre de choix est grand, on peut utiliser un slider
        if (length(possible_choices[[v]]) > 10) {
          sliderInput(inputId = v, label = v,
                      min = min(possible_choices[[v]]),
                      max = max(possible_choices[[v]]),
                      value = median(possible_choices[[v]]))
        } else {
          selectInput(inputId = v, label = v,
                      choices = as.character(possible_choices[[v]]),
                      selected = as.character(possible_choices[[v]][1]))
        }
      }
    })
    do.call(tagList, inputs)
  })
  
  # Fonction pour extraire la RTA à partir d'un code postal
  get_rta <- function(postal_code) {
    # Standardiser le code postal (majuscules, sans espaces)
    postal_code <- toupper(gsub("[^A-Za-z0-9]", "", postal_code))
    # Extraire les 3 premiers caractères (RTA)
    if (nchar(postal_code) >= 3) {
      return(substr(postal_code, 1, 3))
    } else {
      return(NA)
    }
  }
  
  # Fonction pour obtenir les contributions RTA pré-calculées
  get_rta_contributions <- function(rta) {
    if (is.na(rta) || !rta %in% rta_contributions$rta) {
      # Si RTA non trouvée, utiliser la ligne DEFAULT
      return(default_contributions)
    } else {
      # Sinon utiliser les valeurs spécifiques à la RTA
      rta_data <- rta_contributions[rta_contributions$rta == rta, ]
      return(rta_data[, -which(names(rta_data) == "rta")])
    }
  }
  
  observeEvent(input$predictBtn, {
    # Réinitialiser les logs de débogage
    debug_logs("")
    
    output$prediction <- renderPrint({ "Calcul en cours..." })
    output$probabilities <- renderTable({ NULL })
    output$rta_details <- renderPrint({ NULL })
    
    tryCatch({
      # Extraire la RTA à partir du code postal
      rta <- get_rta(input$postalCode)
      
      # Obtenir les contributions RTA pré-calculées
      rta_contribs <- get_rta_contributions(rta)
      
      # Afficher les détails de RTA
      output$rta_details <- renderPrint({
        if (is.na(rta)) {
          return("Aucune RTA valide extraite du code postal.")
        } else {
          if (rta %in% rta_contributions$rta) {
            return(paste("RTA trouvée:", rta, "\nContributions utilisées:", 
                         paste(names(rta_contribs), "=", round(as.numeric(rta_contribs), 4), collapse=", ")))
          } else {
            return(paste("RTA non trouvée:", rta, "\nValeurs DEFAULT utilisées:",
                         paste(names(default_contributions), "=", round(as.numeric(default_contributions), 4), collapse=", ")))
          }
        }
      })
      
      # Rassembler les valeurs saisies dans un data.frame
      newdata <- data.frame(lapply(socio_vars, function(v) input[[v]]),
                          stringsAsFactors = FALSE)
      colnames(newdata) <- socio_vars
      
      # Conversion des variables numériques (si issues d'un selectInput)
      for(v in names(newdata)) {
        if (v %in% socio_vars && is.numeric(possible_choices[[v]])) {
          newdata[[v]] <- as.numeric(newdata[[v]])
        }
      }
      
      # Obtenir les noms et dimensions des coefficients du modèle pour débogage
      model_coefs <- coef(final_model)
      model_coef_names <- colnames(model_coefs)
      add_log(paste("Dimensions des coefficients du modèle:", nrow(model_coefs), "x", ncol(model_coefs)))
      add_log(paste("Classes dans le modèle:", paste(rownames(model_coefs), collapse=", ")))
      
      # Préparer les données pour la prédiction du modèle original
      # MÉTHODE ALTERNATIVE: utiliser predict.nnet directement
      
      # Obtenir la structure du modèle nnet original
      wts <- final_model$wts
      mask <- final_model$mask
      vhash <- sapply(attr(terms(final_model), "variables"), deparse)[-1]
      xlevels <- final_model$xlevels

      # Préparer manuellement les données comme dans predict.nnet
      # 1. Créer un data.frame temporaire avec toutes les variables nécessaires
      # Pour les variables RTA manquantes, on met des valeurs neutres pour l'instant
      x_temp <- newdata
      for (v in rta_vars) {
        x_temp[[v]] <- 0  # Valeur temporaire, sera remplacée plus tard
      }
      
      # Format et releveling des facteurs comme dans le modèle original
      for(v in names(x_temp)) {
        if (v %in% socio_vars && !is.numeric(possible_choices[[v]])) {
          x_temp[[v]] <- factor(x_temp[[v]], levels = possible_choices[[v]])
        }
      }
      
      # Relevel important variables
      if ("ses_region" %in% names(x_temp) && "prairie" %in% levels(x_temp$ses_region)) {
        x_temp$ses_region <- relevel(x_temp$ses_region, ref = "prairie")
      }
      if ("lifestyle_typeTransport" %in% names(x_temp) && "active_transport" %in% levels(x_temp$lifestyle_typeTransport)) {
        x_temp$lifestyle_typeTransport <- relevel(x_temp$lifestyle_typeTransport, ref = "active_transport")
      }
      if ("lifestyle_consClothes" %in% names(x_temp) && "large_retailers" %in% levels(x_temp$lifestyle_consClothes)) {
        x_temp$lifestyle_consClothes <- relevel(x_temp$lifestyle_consClothes, ref = "large_retailers")
      }
      if ("lifestyle_exercise" %in% names(x_temp) && "gym" %in% levels(x_temp$lifestyle_exercise)) {
        x_temp$lifestyle_exercise <- relevel(x_temp$lifestyle_exercise, ref = "gym")
      }
      if ("lifestyle_favAlcool" %in% names(x_temp) && "beer" %in% levels(x_temp$lifestyle_favAlcool)) {
        x_temp$lifestyle_favAlcool <- relevel(x_temp$lifestyle_favAlcool, ref = "beer")
      }
      if ("lifestyle_consCoffee" %in% names(x_temp) && "tim_hortons" %in% levels(x_temp$lifestyle_consCoffee)) {
        x_temp$lifestyle_consCoffee <- relevel(x_temp$lifestyle_consCoffee, ref = "tim_hortons")
      }
      if ("ses_language" %in% names(x_temp) && "english" %in% levels(x_temp$ses_language)) {
        x_temp$ses_language <- relevel(x_temp$ses_language, ref = "english")
      }
      if ("ses_dwelling_cat" %in% names(x_temp) && "stand_alone_house" %in% levels(x_temp$ses_dwelling_cat)) {
        x_temp$ses_dwelling_cat <- relevel(x_temp$ses_dwelling_cat, ref = "stand_alone_house")
      }
      if ("lifestyle_clothingStyleGroups" %in% names(x_temp) && "easygoing" %in% levels(x_temp$lifestyle_clothingStyleGroups)) {
        x_temp$lifestyle_clothingStyleGroups <- relevel(x_temp$lifestyle_clothingStyleGroups, ref = "easygoing")
      }
      if ("ses_educ" %in% names(x_temp) && "no_schooling" %in% levels(x_temp$ses_educ)) {
        x_temp$ses_educ <- relevel(x_temp$ses_educ, ref = "no_schooling")
      }
      if ("ses_income3Cat" %in% names(x_temp) && "High" %in% levels(x_temp$ses_income3Cat)) {
        x_temp$ses_income3Cat <- relevel(x_temp$ses_income3Cat, ref = "High")
      }
      
      # 2. Créer les variables dummy
      dv <- dummyVars(" ~ .", data = x_temp, fullRank = TRUE, sep = "_")
      x_dummy <- predict(dv, newdata = x_temp)
      
      # Vérifier si toutes les colonnes requises par le modèle sont présentes
      model_varnames <- colnames(model_coefs)
      varnames_present <- colnames(x_dummy)

      # Log pour débogage
      add_log(paste("Nombre de variables dans le modèle (avec intercept):", length(model_varnames)))
      add_log(paste("Nombre de variables dans les données:", length(varnames_present)))
      
      # Vérifier les variables manquantes
      missing_vars <- setdiff(model_varnames[-1], varnames_present)  # Ignore l'intercept
      if (length(missing_vars) > 0) {
        add_log(paste("Variables manquantes:", paste(missing_vars, collapse=", ")))
        
        # Ajouter les variables manquantes avec des zéros
        missing_matrix <- matrix(0, nrow = nrow(x_dummy), ncol = length(missing_vars))
        colnames(missing_matrix) <- missing_vars
        x_dummy <- cbind(x_dummy, missing_matrix)
      }
      
      # Réorganiser les colonnes pour correspondre exactement à l'ordre attendu par le modèle
      final_columns <- model_varnames[-1]  # Ignore l'intercept qui sera ajouté plus tard
      x_ordered <- matrix(0, nrow = nrow(x_dummy), ncol = length(final_columns))
      colnames(x_ordered) <- final_columns
      
      for (col in final_columns) {
        if (col %in% colnames(x_dummy)) {
          x_ordered[, col] <- x_dummy[, col]
        }
      }
      
      # Maintenant, à ce stade toutes les variables du modèle sont présentes dans x_ordered
      # REMPLACER les valeurs des variables de prédiction RTA par les vraies valeurs de RTA contributions
      
      # Mapper les noms de variables RTA aux noms dans rta_contribs
      rta_mapping <- list(
        "prediction_CPC" = "cpc",
        "prediction_LPC" = "lpc",
        "prediction_NDP" = "ndp",
        "prediction_GPC" = "gpc",
        "prediction_BQ" = "bq"
      )
      
      # Mettre à jour les valeurs RTA
      for (var in rta_vars) {
        if (var %in% colnames(x_ordered) && rta_mapping[[var]] %in% names(rta_contribs)) {
          x_ordered[, var] <- as.numeric(rta_contribs[[rta_mapping[[var]]]])
        }
      }
      
      # 3. Obtenir les scores bruts (coefficients linéaires) à partir du modèle
      Z <- cbind(1, x_ordered) %*% t(model_coefs)
      
      # IMPORTANT: Ajouter l'ajustement pour la classe BQ de référence
      # La contribution du BQ (classe de référence) doit être intégrée séparément
      bq_contribution <- 0  # Ligne de base (intercepte pour BQ = 0)
      if ("bq" %in% names(rta_contribs)) {
        bq_contribution <- as.numeric(rta_contribs[["bq"]])
      }
      
      # 4. Convertir les scores en probabilités (softmax) avec BQ comme référence
      max_Z <- apply(Z, 1, max)
      exp_Z <- exp(Z - max_Z)
      
      # Calculer exp(0 + contribution_bq) pour la classe de référence (BQ)
      exp_bq <- exp(bq_contribution - max_Z)
      
      # Ajouter l'exp(BQ) à la somme totale
      sum_exp_Z <- rowSums(exp_Z) + exp_bq
      
      # Calculer les probabilités pour toutes les classes sauf BQ
      pred_prob <- exp_Z / sum_exp_Z
      colnames(pred_prob) <- rownames(model_coefs)  # CPC, LPC, NDP, GPC
      
      # Calculer la probabilité pour BQ (classe de référence)
      bq_prob <- exp_bq / sum_exp_Z
      
      # Combiner toutes les probabilités, y compris BQ
      all_pred_prob <- cbind(BQ = bq_prob, pred_prob)
      
      # 5. Obtenir la classe prédite (inclut maintenant BQ comme option)
      all_parties <- c("bq", rownames(model_coefs))
      pred_probs_with_bq <- cbind(bq_prob, pred_prob)
      colnames(pred_probs_with_bq) <- all_parties
      pred_class <- all_parties[apply(pred_probs_with_bq, 1, which.max)]
      
      # Afficher les résultats
      output$prediction <- renderPrint({
        paste("Le choix de vote prédit est :", toupper(as.character(pred_class)))
      })
      
      output$probabilities <- renderTable({
        # Arrondir les probabilités à 4 décimales pour plus de lisibilité
        pred_prob_rounded <- round(all_pred_prob, 4)
        colnames(pred_prob_rounded) <- toupper(colnames(pred_prob_rounded))
        pred_prob_rounded
      }, rownames = TRUE)
      
    }, error = function(e) {
      add_log(paste("ERREUR:", e$message))
      add_log(paste("CALL:", deparse(e$call)))
      
      output$prediction <- renderPrint({
        paste("Une erreur s'est produite :", e$message)
      })
      output$probabilities <- renderTable({ NULL })
    })
  })
}

# ------------------------------------------------------------------------
# 5. Lancement de l'application Shiny
# ------------------------------------------------------------------------
shinyApp(ui, server)