library(shiny)
library(caret)
library(tidyverse)
library(nnet)
library(rsconnect)

# ------------------------------------------------------------------------
# 1. Chargement du modèle final (chemin relatif)
# ------------------------------------------------------------------------
final_model <- readRDS("data/finalmodel_withOutInteractions.rds")

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
  "ses_income3Cat", "lifestyle_ownPet_bin"
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
  lifestyle_clothingStyleGroups = c("easygoing", "edgy", "formal"),
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
# 3. Création d’un objet dummyVars pour transformer les données
#
# Puisque nous n’avons plus accès à DataModel pour construire
# dummyVars, nous créons un data.frame fictif (training_structure)
# en prenant pour chaque variable la première valeur de possible_choices.
# Cela reproduit la structure de données utilisée lors de l’entraînement.
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

# Création de l’objet dummyVars (la même transformation qu’à l’entraînement)
dummies_final <- dummyVars(" ~ .", data = training_structure, fullRank = TRUE, sep = "_")

# ------------------------------------------------------------------------
# 4. Interface utilisateur
# ------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Prédiction du Choix de Vote"),
  sidebarLayout(
    sidebarPanel(
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
    inputs <- lapply(final_vars, function(v) {
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
  
  observeEvent(input$predictBtn, {
    output$prediction <- renderPrint({ "Calcul en cours..." })
    output$probabilities <- renderTable({ NULL })
    
    tryCatch({
      # Rassembler les valeurs saisies dans un data.frame
      newdata <- data.frame(lapply(final_vars, function(v) input[[v]]),
                            stringsAsFactors = FALSE)
      colnames(newdata) <- final_vars
      
      # Conversion des variables numériques (si issues d’un selectInput)
      for(v in final_vars) {
        if (is.numeric(possible_choices[[v]])) {
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
