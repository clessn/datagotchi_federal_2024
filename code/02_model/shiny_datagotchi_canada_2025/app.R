library(shiny)
library(caret)
library(tidyverse)
library(nnet)
library(rsconnect)

# ------------------------------------------------------------------------
# 1. Chargement du modèle final (chemin relatif)
# ------------------------------------------------------------------------
final_model <- readRDS("data/finalmodel_withRTAPredictions_2025-04-15.rds")

# Charger les données RTA pour les prédictions
rta_predictions <- read.csv("data/rta_predictions_partis.csv", stringsAsFactors = FALSE)

# Si le fichier des prédictions RTA n'existe pas, on crée une version vide pour éviter les erreurs
if (!file.exists("data/rta_predictions_partis.csv")) {
  # Créer un dataframe vide avec les colonnes nécessaires
  rta_predictions <- data.frame(
    rta = character(),
    CPC = numeric(),
    LPC = numeric(),
    NDP = numeric(),
    GPC = numeric(),
    BQ = numeric(),
    stringsAsFactors = FALSE
  )
  # Sauvegarder le fichier vide
  write.csv(rta_predictions, "data/rta_predictions_partis.csv", row.names = FALSE)
}

# Calculer les moyennes des prédictions par parti pour utiliser comme valeurs par défaut
mean_cpc <- mean(rta_predictions$CPC, na.rm = TRUE)
mean_lpc <- mean(rta_predictions$LPC, na.rm = TRUE)
mean_ndp <- mean(rta_predictions$NDP, na.rm = TRUE)
mean_gpc <- mean(rta_predictions$GPC, na.rm = TRUE)
mean_bq <- mean(rta_predictions$BQ, na.rm = TRUE)

# S'assurer que les RTA sont en majuscules
rta_predictions$rta <- toupper(rta_predictions$rta)

# ------------------------------------------------------------------------
# 2. Définition manuelle des variables utilisées dans le modèle
#    et des choix possibles (ces valeurs sont à adapter selon votre codage)
# ------------------------------------------------------------------------
final_vars <- c(
  "ses_region", "ses_immigrant", "lifestyle_typeTransport",
  "lifestyle_consClothes", "lifestyle_exercise", "lifestyle_eatMeatFreq",
  "lifestyle_favAlcool", "lifestyle_consCoffee", "ses_language",
  "lifestyle_smokeFreq", "ses_age", "ses_dwelling_cat", "ses_ethnicityWhite",
  "ses_sexOrientationHetero", "ses_genderFemale", "lifestyle_clothingStyleGroups",
  "lifestyle_goHuntingFreq_numeric", "lifestyle_goFishingFreq_bin",
  "lifestyle_goMuseumsFreq_bin", "lifestyle_volunteeringFreq",
  "lifestyle_motorizedActFreq_bin", "lifestyle_hasTattoos", "ses_educ",
  "ses_income3Cat", "lifestyle_ownPet_bin",
  "prediction_CPC", "prediction_LPC", "prediction_NDP", "prediction_GPC", "prediction_BQ"
)

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
  lifestyle_ownPet_bin = c(0, 1),
  # Variables de prédiction RTA (valeurs par défaut)
  prediction_CPC = c(mean_cpc),
  prediction_LPC = c(mean_lpc),
  prediction_NDP = c(mean_ndp),
  prediction_GPC = c(mean_gpc),
  prediction_BQ = c(mean_bq)
)

# ------------------------------------------------------------------------
# 3. Création d'un objet dummyVars pour transformer les données
#
# Puisque nous n'avons plus accès à DataModel pour construire
# dummyVars, nous créons un data.frame fictif (training_structure)
# en prenant pour chaque variable la première valeur de possible_choices.
# Cela reproduit la structure de données utilisée lors de l'entraînement.
# ------------------------------------------------------------------------
training_structure <- as.data.frame(lapply(final_vars, function(v) {
  if (is.numeric(possible_choices[[v]])) {
    as.numeric(possible_choices[[v]][1])
  } else {
    possible_choices[[v]][1]
  }
}), stringsAsFactors = FALSE)
colnames(training_structure) <- final_vars

# Pour les variables non numériques, convertir en facteur non ordonné
for(v in final_vars) {
  if (!is.numeric(possible_choices[[v]])) {
    training_structure[[v]] <- factor(training_structure[[v]],
                                      levels = possible_choices[[v]],
                                      ordered = FALSE)
  }
}

# Fixer manuellement les catégories de référence pour correspondre au modèle
if ("ses_region" %in% names(training_structure) && "prairie" %in% levels(training_structure$ses_region)) {
  training_structure$ses_region <- relevel(training_structure$ses_region, ref = "prairie")
}
if ("lifestyle_typeTransport" %in% names(training_structure) && "active_transport" %in% levels(training_structure$lifestyle_typeTransport)) {
  training_structure$lifestyle_typeTransport <- relevel(training_structure$lifestyle_typeTransport, ref = "active_transport")
}
if ("lifestyle_consClothes" %in% names(training_structure) && "large_retailers" %in% levels(training_structure$lifestyle_consClothes)) {
  training_structure$lifestyle_consClothes <- relevel(training_structure$lifestyle_consClothes, ref = "large_retailers")
}
if ("lifestyle_exercise" %in% names(training_structure) && "gym" %in% levels(training_structure$lifestyle_exercise)) {
  training_structure$lifestyle_exercise <- relevel(training_structure$lifestyle_exercise, ref = "gym")
}
if ("lifestyle_favAlcool" %in% names(training_structure) && "beer" %in% levels(training_structure$lifestyle_favAlcool)) {
  training_structure$lifestyle_favAlcool <- relevel(training_structure$lifestyle_favAlcool, ref = "beer")
}
if ("lifestyle_consCoffee" %in% names(training_structure) && "tim_hortons" %in% levels(training_structure$lifestyle_consCoffee)) {
  training_structure$lifestyle_consCoffee <- relevel(training_structure$lifestyle_consCoffee, ref = "tim_hortons")
}
if ("ses_language" %in% names(training_structure) && "english" %in% levels(training_structure$ses_language)) {
  training_structure$ses_language <- relevel(training_structure$ses_language, ref = "english")
}
if ("ses_dwelling_cat" %in% names(training_structure) && "stand_alone_house" %in% levels(training_structure$ses_dwelling_cat)) {
  training_structure$ses_dwelling_cat <- relevel(training_structure$ses_dwelling_cat, ref = "stand_alone_house")
}
if ("lifestyle_clothingStyleGroups" %in% names(training_structure) && "easygoing" %in% levels(training_structure$lifestyle_clothingStyleGroups)) {
  training_structure$lifestyle_clothingStyleGroups <- relevel(training_structure$lifestyle_clothingStyleGroups, ref = "easygoing")
}
if ("ses_educ" %in% names(training_structure) && "no_schooling" %in% levels(training_structure$ses_educ)) {
  training_structure$ses_educ <- relevel(training_structure$ses_educ, ref = "no_schooling")
}
if ("ses_income3Cat" %in% names(training_structure) && "High" %in% levels(training_structure$ses_income3Cat)) {
  training_structure$ses_income3Cat <- relevel(training_structure$ses_income3Cat, ref = "High")
}

# Création de l'objet dummyVars (la même transformation qu'à l'entraînement)
dummies_final <- dummyVars(" ~ .", data = training_structure, fullRank = TRUE, sep = "_")

# ------------------------------------------------------------------------
# 4. Interface utilisateur
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
      tableOutput("probabilities")
    )
  )
)

# ------------------------------------------------------------------------
# 5. Serveur
# ------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Création dynamique des inputs pour chaque variable
  output$dynamic_inputs <- renderUI({
    # Exclure les variables de prédiction RTA et le code postal qui sont gérés séparément
    vars_to_show <- setdiff(final_vars, c("prediction_CPC", "prediction_LPC", "prediction_NDP", "prediction_GPC", "prediction_BQ"))
    
    inputs <- lapply(vars_to_show, function(v) {
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
  
  # Fonction pour obtenir les prédictions RTA
  get_rta_predictions <- function(rta) {
    if (is.na(rta) || !rta %in% rta_predictions$rta) {
      # Si RTA non trouvée, utiliser les moyennes
      return(list(
        prediction_CPC = mean_cpc,
        prediction_LPC = mean_lpc,
        prediction_NDP = mean_ndp,
        prediction_GPC = mean_gpc,
        prediction_BQ = mean_bq
      ))
    } else {
      # Sinon utiliser les valeurs spécifiques à la RTA
      rta_data <- rta_predictions[rta_predictions$rta == rta, ]
      return(list(
        prediction_CPC = rta_data$CPC,
        prediction_LPC = rta_data$LPC,
        prediction_NDP = rta_data$NDP,
        prediction_GPC = rta_data$GPC,
        prediction_BQ = rta_data$BQ
      ))
    }
  }
  
  observeEvent(input$predictBtn, {
    output$prediction <- renderPrint({ "Calcul en cours..." })
    output$probabilities <- renderTable({ NULL })
    
    tryCatch({
      # Extraire la RTA à partir du code postal
      rta <- get_rta(input$postalCode)
      
      # Obtenir les prédictions RTA
      rta_preds <- get_rta_predictions(rta)
      
      # Rassembler les valeurs saisies dans un data.frame
      vars_to_collect <- setdiff(final_vars, c("prediction_CPC", "prediction_LPC", "prediction_NDP", "prediction_GPC", "prediction_BQ"))
      newdata <- data.frame(lapply(vars_to_collect, function(v) input[[v]]),
                          stringsAsFactors = FALSE)
      colnames(newdata) <- vars_to_collect
      
      # Ajouter les prédictions RTA
      newdata$prediction_CPC <- rta_preds$prediction_CPC
      newdata$prediction_LPC <- rta_preds$prediction_LPC
      newdata$prediction_NDP <- rta_preds$prediction_NDP
      newdata$prediction_GPC <- rta_preds$prediction_GPC
      newdata$prediction_BQ <- rta_preds$prediction_BQ
      
      # Conversion des variables numériques (si issues d'un selectInput)
      for(v in names(newdata)) {
        if (v %in% final_vars && is.numeric(possible_choices[[v]])) {
          newdata[[v]] <- as.numeric(newdata[[v]])
        }
      }
      
      # Création d'une ligne "dummy" pour forcer la présence de tous les niveaux
      dummy_row <- sapply(final_vars, function(v) {
        if (!is.numeric(possible_choices[[v]])) {
          if (length(possible_choices[[v]]) > 1)
            possible_choices[[v]][2]
          else
            possible_choices[[v]][1]
        } else {
          as.numeric(possible_choices[[v]][1])
        }
      }, simplify = FALSE)
      dummy_row <- as.data.frame(dummy_row, stringsAsFactors = FALSE)
      colnames(dummy_row) <- final_vars
      
      # Combiner la ligne dummy et la nouvelle observation (la ligne dummy en première position)
      newdata2 <- rbind(dummy_row, newdata)
      
      # Pour chaque variable catégorielle, forcer la conversion en facteur non ordonné avec tous les niveaux
      for(v in final_vars) {
        if (!is.numeric(possible_choices[[v]])) {
          newdata2[[v]] <- factor(newdata2[[v]],
                                levels = possible_choices[[v]],
                                ordered = FALSE)
        }
      }
      
      # Fixer manuellement les catégories de référence pour correspondre au modèle
      if ("ses_region" %in% names(newdata2) && "prairie" %in% levels(newdata2$ses_region)) {
        newdata2$ses_region <- relevel(newdata2$ses_region, ref = "prairie")
      }
      if ("lifestyle_typeTransport" %in% names(newdata2) && "active_transport" %in% levels(newdata2$lifestyle_typeTransport)) {
        newdata2$lifestyle_typeTransport <- relevel(newdata2$lifestyle_typeTransport, ref = "active_transport")
      }
      if ("lifestyle_consClothes" %in% names(newdata2) && "large_retailers" %in% levels(newdata2$lifestyle_consClothes)) {
        newdata2$lifestyle_consClothes <- relevel(newdata2$lifestyle_consClothes, ref = "large_retailers")
      }
      if ("lifestyle_exercise" %in% names(newdata2) && "gym" %in% levels(newdata2$lifestyle_exercise)) {
        newdata2$lifestyle_exercise <- relevel(newdata2$lifestyle_exercise, ref = "gym")
      }
      if ("lifestyle_favAlcool" %in% names(newdata2) && "beer" %in% levels(newdata2$lifestyle_favAlcool)) {
        newdata2$lifestyle_favAlcool <- relevel(newdata2$lifestyle_favAlcool, ref = "beer")
      }
      if ("lifestyle_consCoffee" %in% names(newdata2) && "tim_hortons" %in% levels(newdata2$lifestyle_consCoffee)) {
        newdata2$lifestyle_consCoffee <- relevel(newdata2$lifestyle_consCoffee, ref = "tim_hortons")
      }
      if ("ses_language" %in% names(newdata2) && "english" %in% levels(newdata2$ses_language)) {
        newdata2$ses_language <- relevel(newdata2$ses_language, ref = "english")
      }
      if ("ses_dwelling_cat" %in% names(newdata2) && "stand_alone_house" %in% levels(newdata2$ses_dwelling_cat)) {
        newdata2$ses_dwelling_cat <- relevel(newdata2$ses_dwelling_cat, ref = "stand_alone_house")
      }
      if ("lifestyle_clothingStyleGroups" %in% names(newdata2) && "easygoing" %in% levels(newdata2$lifestyle_clothingStyleGroups)) {
        newdata2$lifestyle_clothingStyleGroups <- relevel(newdata2$lifestyle_clothingStyleGroups, ref = "easygoing")
      }
      if ("ses_educ" %in% names(newdata2) && "no_schooling" %in% levels(newdata2$ses_educ)) {
        newdata2$ses_educ <- relevel(newdata2$ses_educ, ref = "no_schooling")
      }
      if ("ses_income3Cat" %in% names(newdata2) && "High" %in% levels(newdata2$ses_income3Cat)) {
        newdata2$ses_income3Cat <- relevel(newdata2$ses_income3Cat, ref = "High")
      }
      
      # Transformation dummyVars
      newdata_dummy <- predict(dummies_final, newdata = newdata2) %>% as.data.frame()
      newdata_dummy <- newdata_dummy[-1, , drop = FALSE]  # Retirer la ligne dummy
      
      # Réaliser la prédiction
      pred_class <- predict(final_model, newdata = newdata_dummy)
      pred_prob  <- predict(final_model, newdata = newdata_dummy, type = "probs")
      
      output$prediction <- renderPrint({
        paste("Le choix de vote prédit est :", as.character(pred_class))
      })
      output$probabilities <- renderTable({
        pred_prob
      }, rownames = TRUE)
      
    }, error = function(e) {
      output$prediction <- renderPrint({
        paste("Une erreur s'est produite :", e$message)
      })
      output$probabilities <- renderTable({ NULL })
    })
  })
}

# ------------------------------------------------------------------------
# 6. Lancement de l'application Shiny
# ------------------------------------------------------------------------
shinyApp(ui, server)