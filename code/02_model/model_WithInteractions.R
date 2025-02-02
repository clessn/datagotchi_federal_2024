# Load packages ----------------------------------------------------------
library(nnet)
library(tidyverse)
library(caret)
library(dplyr)
library(tidyr)
library(ggplot2)

# On va utiliser la CV via caret::train, donc on a besoin d'un summaryFunction adapté
library(yardstick)  # pour calculer metrics multiclasses
library(pbapply)    # pour une barre de progression (optionnel)

# ------------------------------------------------------------------------
# 1) Chargement des données
# ------------------------------------------------------------------------
DataModel <- readRDS("_SharedFolder_datagotchi_federal_2024/data/pilote/DataCleanPilot_2025Janv30.rds")
candidate_interactions <- read.csv("_SharedFolder_datagotchi_federal_2024/data/modele/combined_predictors_leaderboard.csv")
# ------------------------------------------------------------------------
# 2) Sélection des variables
# ------------------------------------------------------------------------
DataModel <- DataModel |> 
  select(
    id, 
    ses_immigrant,
    ses_region,
    ses_age, ses_age_4Cat, ses_ageGroup5Years, 
    lifestyle_hasTattoos, lifestyle_numberTattoos, 
    ses_dwelling, ses_dwelling_cat, 
    ses_ethnicity, ses_ethnicityWB, ses_ethnicityWhite,
    ses_sexOrientation, ses_sexOrientationHetero, 
    ses_gender, ses_genderFemale, 
    lifestyle_clothingStyle, lifestyle_clothingStyleGroups,
    lifestyle_typeTransport, lifestyle_consClothes, lifestyle_exercise, 
    lifestyle_eatMeatFreq, lifestyle_favAlcool, lifestyle_consCoffee, 
    ses_language, lifestyle_smokeFreq, 
    lifestyle_ownPet, lifestyle_ownPet_bin,
    lifestyle_goHuntingFreq, lifestyle_goHuntingFreq_bin, lifestyle_goHuntingFreq_factor, lifestyle_goHuntingFreq_numeric,
    lifestyle_goFishingFreq, lifestyle_goFishingFreq_bin, lifestyle_goFishingFreq_factor, lifestyle_goFishingFreq_numeric, 
    lifestyle_goMuseumsFreq, lifestyle_goMuseumsFreq_bin, lifestyle_goMuseumsFreq_factor, lifestyle_goMuseumsFreq_numeric,
    lifestyle_volunteeringFreq, lifestyle_volunteeringFreq_bin, lifestyle_volunteeringFreq_factor, lifestyle_volunteeringFreq_numeric,
    ses_educ, ses_educ_3Cat, ses_educ_5Cat, 
    ses_income, ses_income_3Cat, ses_incomeCensus,
    lifestyle_motorizedActFreq, lifestyle_motorizedActFreq_bin, lifestyle_motorizedActFreq_factor, lifestyle_motorizedActFreq_numeric,
    dv_voteChoice
  ) %>%
  drop_na() %>%
  filter(dv_voteChoice != "other")

# La cible en facteur
DataModel$dv_voteChoice <- factor(DataModel$dv_voteChoice)
DataModel$lifestyle_consCoffee <- factor(DataModel$lifestyle_consCoffee, ordered = FALSE)
DataModel$lifestyle_consClothes <- factor(DataModel$lifestyle_consClothes, ordered = FALSE)

# ------------------------------------------------------------------------
# 3) Séparation Train/Test
# ------------------------------------------------------------------------
set.seed(42)
trainIndex <- createDataPartition(DataModel$dv_voteChoice, p = 0.8, list = FALSE)
DfTrain    <- DataModel[trainIndex, ]
DfTest     <- DataModel[-trainIndex, ]

# ------------------------------------------------------------------------
# 4) Définition des différentes codifications possibles
# ------------------------------------------------------------------------
variable_options <- list(
  age = c("ses_age", "ses_age_4Cat", "ses_ageGroup5Years"),
  dwelling = c("ses_dwelling", "ses_dwelling_cat"),
  ethnicity = c("ses_ethnicity", "ses_ethnicityWB", "ses_ethnicityWhite"),
  sexOrientation = c("ses_sexOrientation", "ses_sexOrientationHetero"),
  gender = c("ses_gender", "ses_genderFemale"),
  clothingStyle = c("lifestyle_clothingStyle", "lifestyle_clothingStyleGroups"),
  goHuntingFreq = c("lifestyle_goHuntingFreq", "lifestyle_goHuntingFreq_bin", 
                    "lifestyle_goHuntingFreq_factor", "lifestyle_goHuntingFreq_numeric"),
  goFishingFreq = c("lifestyle_goFishingFreq", "lifestyle_goFishingFreq_bin", 
                    "lifestyle_goFishingFreq_factor", "lifestyle_goFishingFreq_numeric"),
  goMuseumsFreq = c("lifestyle_goMuseumsFreq", "lifestyle_goMuseumsFreq_bin", 
                    "lifestyle_goMuseumsFreq_factor", "lifestyle_goMuseumsFreq_numeric"),
  volunteeringFreq = c("lifestyle_volunteeringFreq", "lifestyle_volunteeringFreq_bin", 
                       "lifestyle_volunteeringFreq_factor", "lifestyle_volunteeringFreq_numeric"),
  motorizedActFreq = c("lifestyle_motorizedActFreq", "lifestyle_motorizedActFreq_bin", 
                       "lifestyle_motorizedActFreq_factor", "lifestyle_motorizedActFreq_numeric"),
  tattoo = c("lifestyle_hasTattoos", "lifestyle_numberTattoos"),
  educ = c("ses_educ", "ses_educ_3Cat", "ses_educ_5Cat"),
  income = c("ses_income", "ses_income_3Cat", "ses_incomeCensus"),
  ownPet = c("lifestyle_ownPet", "lifestyle_ownPet_bin")
)

# Variables qu'on prend toujours
other_variables <- c(
  "ses_region",
  "ses_immigrant",
  "lifestyle_typeTransport",
  "lifestyle_consClothes",
  "lifestyle_exercise",
  "lifestyle_eatMeatFreq",
  "lifestyle_favAlcool",
  "lifestyle_consCoffee",
  "ses_language",
  "lifestyle_smokeFreq"
)

# ------------------------------------------------------------------------
# 5) Fonction de résumé multiclasses (Accuracy, Kappa, LogLoss)
# ------------------------------------------------------------------------
# Cette fonction sera appelée par caret pour chaque CV-fold.
multiClassSummary2 <- function(data, lev = NULL, model = NULL) {
  # 1) Accuracy
  acc <- yardstick::accuracy_vec(data$obs, data$pred)
  
  # 2) Kappa
  kap <- yardstick::kap_vec(data$obs, data$pred)
  
  # 3) LogLoss
  eps <- 1e-15
  n   <- nrow(data)
  ll  <- 0
  for (i in seq_len(n)) {
    obs_class <- as.character(data$obs[i])
    prob_col  <- paste0("prob.", obs_class)
    p <- data[[prob_col]][i]
    p <- max(p, eps)
    ll <- ll - log(p)
  }
  ll <- ll / n
  
  # 4) Calcul d'une pénalité personnalisée pour les erreurs critiques
  # On reconstruit une matrice de confusion à partir des prédictions et observations
  conf <- table(data$pred, data$obs)
  penalty <- 0
  penalty_value <- 5  # Coefficient de pénalité (à ajuster)
  
  # Vérifier que les labels concernés existent dans le vecteur lev
  if ("npd" %in% lev && "cpc" %in% lev) {
    # Pénaliser : observation "npd" mais prédiction "cpc"
    if ("cpc" %in% rownames(conf) && "npd" %in% colnames(conf)) {
      penalty <- penalty + penalty_value * conf["cpc", "npd"]
    }
  }
  if ("gpc" %in% lev && "cpc" %in% lev) {
    # Pénaliser : observation "gpc" mais prédiction "cpc"
    if ("cpc" %in% rownames(conf) && "gpc" %in% colnames(conf)) {
      penalty <- penalty + penalty_value * conf["cpc", "gpc"]
    }
  }
  
  # Vous pouvez ensuite décider comment intégrer cette pénalité dans votre score global.
  # Par exemple, définir un score composite qui soustrait la pénalité de l'accuracy.
  composite_score <- acc - (penalty / n)  # L'idée ici est d'avoir une "accuracy ajustée"
  
  out <- c(accuracy = acc,
           kappa = kap,
           logLoss = ll,
           penalty = penalty,
           composite_score = composite_score)
  return(out)
}
# ------------------------------------------------------------------------
# 6) Fonction d'une itération de modèle
# ------------------------------------------------------------------------
one_iteration <- function(model_id, DfTrain, var_options, other_vars, 
  candidate_interactions = NULL, n_interactions = 3) {

# a) Sélection aléatoire des codifications pour chaque groupe de variables
selected_vars <- other_vars
var_codings_used <- list()  # pour stocker la correspondance variable -> groupe

for (var_group in names(var_options)) {
codings <- var_options[[var_group]]
chosen_coding <- sample(codings, 1)   # on pioche 1 codage au hasard
selected_vars <- c(selected_vars, chosen_coding)
var_codings_used[[chosen_coding]] <- var_group
}

# b) Sélectionner les interactions parmi les candidates
interactions_to_include <- c()
if (!is.null(candidate_interactions)) {
for (i in seq_len(nrow(candidate_interactions))) {
# On ne considère que les lignes de type "interaction"
if (candidate_interactions$predictor_type[i] == "interaction") {
var1 <- candidate_interactions$var1[i]
var2 <- candidate_interactions$var2[i]
# On ajoute l'interaction si ET seulement si les 2 variables sont sélectionnées
if (var1 %in% selected_vars && var2 %in% selected_vars) {
interactions_to_include <- c(interactions_to_include, paste0(var1, ":", var2))
if (length(interactions_to_include) >= n_interactions) break
}
}
}
}

# c) Construction de la formule
main_effects <- paste(selected_vars, collapse = " + ")
if (length(interactions_to_include) > 0) {
interactions_str <- paste(interactions_to_include, collapse = " + ")
formula_str <- paste("dv_voteChoice ~", main_effects, "+", interactions_str)
} else {
formula_str <- paste("dv_voteChoice ~", main_effects)
}
fmla <- as.formula(formula_str)

# Optionnel : afficher la formule pour vérification
cat("Itération", model_id, ":\n")
cat("  Formule utilisée:", deparse(fmla), "\n")

# d) Définir la validation croisée (ici 5-fold CV) et la fonction de résumé
train_control <- trainControl(
method = "cv",
number = 5,
summaryFunction = multiClassSummary2,
classProbs = TRUE,
savePredictions = "final"
)

# e) Entraîner le modèle multinomial via caret avec la formule construite
cv_model <- train(
  fmla,
  data      = DfTrain,
  method    = "multinom",
  trControl = train_control,
  metric    = "composite_score",    # on cherche à maximiser ce score
  maximize  = TRUE,
  MaxNWts   = 100000,
  trace     = FALSE
)


# f) Récupérer la performance sur la CV
results_cv <- cv_model$results
accuracy_cv <- results_cv$accuracy[1]
kappa_cv    <- results_cv$kappa[1]
logloss_cv  <- results_cv$logLoss[1]

# g) Stocker la configuration utilisée : variables principales et interactions
config_used <- data.frame(
model_id  = model_id,
variable  = I(list(selected_vars)),
interaction = I(list(interactions_to_include)),
accuracy_cv = accuracy_cv,
kappa_cv    = kappa_cv,
logloss_cv  = logloss_cv,
stringsAsFactors = FALSE
)

return(config_used)
}

# ------------------------------------------------------------------------
# 7) Boucle sur M itérations (avec barre de progression)
# ------------------------------------------------------------------------
M <- 90
set.seed(2023)

all_iterations <- pblapply(seq_len(M), function(i) {
  cat("Itération n°", i, "sur", M, "\n")
  one_iteration(
    model_id    = i,
    DfTrain     = DfTrain,
    var_options = variable_options,
    other_vars  = other_variables,
    candidate_interactions = candidate_interactions,  # passage du data.frame
    n_interactions = 3  # par exemple, on essaie d'ajouter jusqu'à 3 interactions
  )
})

results_train <- bind_rows(all_iterations)
saveRDS(results_train, "_SharedFolder_datagotchi_federal_2024/data/modele/resultsTrainAvecInteractionsV2.rds")



# ------------------------------------------------------------------------
# 8) Synthèse des résultats sur la CV du train
# ------------------------------------------------------------------------
# Pour grouper par la configuration utilisée, nous allons transformer
# les colonnes 'variable' et 'interaction' (qui sont des listes) en chaînes de caractères.
results_train <- results_train %>%
  mutate(
    variables_str = sapply(variable, function(x) paste(x, collapse = ", ")),
    interactions_str = sapply(interaction, function(x) if(length(x) > 0) paste(x, collapse = ", ") else "None")
  )

summary_train <- results_train %>%
  group_by(variables_str, interactions_str) %>%
  summarise(
    mean_accuracy = mean(accuracy_cv),
    sd_accuracy   = sd(accuracy_cv),
    mean_kappa    = mean(kappa_cv),
    sd_kappa      = sd(kappa_cv),
    mean_logloss  = mean(logloss_cv),
    sd_logloss    = sd(logloss_cv),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_accuracy)) # ou trier par une autre métrique

print(summary_train)

# ------------------------------------------------------------------------
# 9) Identification du meilleur modèle (selon accuracy_cv, par exemple)
# ------------------------------------------------------------------------
best_iterations <- results_train %>%
  group_by(model_id) %>%
  summarise(
    score_iter_accuracy = first(accuracy_cv),
    score_iter_kappa    = first(kappa_cv),
    score_iter_logloss  = first(logloss_cv),
    .groups = "drop"
  ) %>%
  arrange(desc(score_iter_accuracy))

best_id <- best_iterations$model_id[1]

cat("Meilleur modèle trouvé (ID) =", best_id, "\n")

best_config <- results_train %>%
  filter(model_id == best_id)

# ------------------------------------------------------------------------
# 10) Construction du modèle final sur TOUT le training set (avec la config retenue)
# ------------------------------------------------------------------------
# Extraction des variables principales et interactions depuis la configuration du meilleur modèle
final_vars <- unlist(best_config$variable)           # conversion de la liste en vecteur de noms
final_interactions <- unlist(best_config$interaction)

# Construction de la formule finale
main_effects <- paste(final_vars, collapse = " + ")
if(length(final_interactions) > 0){
  interactions_str <- paste(final_interactions, collapse = " + ")
  formula_str <- paste("dv_voteChoice ~", main_effects, "+", interactions_str)
} else {
  formula_str <- paste("dv_voteChoice ~", main_effects)
}
final_formula <- as.formula(formula_str)

cat("Formule finale :", deparse(final_formula), "\n")

# Entraîner le modèle final sur le training set complet
# Ici, nous utilisons directement la formule finale.
final_model <- multinom(final_formula, data = DfTrain, trace = FALSE, MaxNWts = 100000)
print(final_model)

# ------------------------------------------------------------------------
# 11) Évaluation sur le jeu de test (jamais vu jusqu'ici)
# ------------------------------------------------------------------------
# Pour évaluer le modèle final, nous utilisons à nouveau la formule finale.
# Notez que si votre modèle a été construit avec la formule, la transformation des
# variables (dummy encoding, etc.) se fait automatiquement par la fonction multinom.

pred_test_final_class <- predict(final_model, newdata = DfTest)
acc_test_final <- mean(pred_test_final_class == DfTest$dv_voteChoice)
cat("Accuracy (test) :", acc_test_final, "\n")

# Calcul manuel de la logLoss sur le jeu de test
pred_test_final_prob <- predict(final_model, newdata = DfTest, type = "probs")
levelz <- levels(DfTest$dv_voteChoice)
logloss_test <- 0
n_test <- nrow(DfTest)
for (i in seq_len(n_test)) {
  obs_class <- DfTest$dv_voteChoice[i]
  class_idx <- which(levelz == obs_class)
  p <- pred_test_final_prob[i, class_idx]
  p <- max(p, 1e-15)  # éviter log(0)
  logloss_test <- logloss_test - log(p)
}
logloss_test <- logloss_test / n_test
cat("LogLoss (test) :", logloss_test, "\n")

# Tableau de confusion
table_test <- table(
  predicted = pred_test_final_class,
  actual    = DfTest$dv_voteChoice
)
print(table_test)


# Nombre d'itérations pour l'évaluation
n_iter <- 10

# Initialiser un data.frame pour stocker les résultats
results_eval <- data.frame(
  iteration = integer(),
  accuracy = numeric(),
  logloss = numeric(),
  stringsAsFactors = FALSE
)

for (i in 1:n_iter) {
  
  # Pour varier le sous-échantillon à chaque itération, on fixe un seed différent
  set.seed(2025 + i)  # modifiez le seed pour obtenir des tirages différents
  
  # Ici, on sélectionne par exemple 80% des observations du jeu de test
  sample_indices <- sample(1:nrow(DfTest), size = floor(0.8 * nrow(DfTest)))
  test_sample <- DfTest[sample_indices, ]
  
  ## Prédictions du modèle final sur le sous-échantillon
  
  # Prédictions de la classe
  pred_test_class <- predict(final_model, newdata = test_sample)
  
  # Calcul de l'accuracy
  acc_test <- mean(pred_test_class == test_sample$dv_voteChoice)
  
  # Prédictions des probabilités
  pred_test_prob <- predict(final_model, newdata = test_sample, type = "probs")
  
  # Calcul manuel de la logLoss
  levelz <- levels(test_sample$dv_voteChoice)
  logloss_test <- 0
  n_test <- nrow(test_sample)
  
  for (j in seq_len(n_test)) {
    obs_class <- test_sample$dv_voteChoice[j]
    # Identifier l'indice de la classe observée
    class_idx <- which(levelz == obs_class)
    # Récupérer la probabilité prédite pour la classe observée
    p <- pred_test_prob[j, class_idx]
    p <- max(p, 1e-15)  # pour éviter log(0)
    logloss_test <- logloss_test - log(p)
  }
  logloss_test <- logloss_test / n_test
  
  # Stocker les métriques dans le data.frame
  results_eval <- rbind(
    results_eval,
    data.frame(iteration = i, accuracy = acc_test, logloss = logloss_test)
  )
  
  # Affichage des résultats et de la matrice de confusion pour l'itération courante
  cat("Itération", i, ":\n")
  cat("  Accuracy :", acc_test, "\n")
  cat("  LogLoss  :", logloss_test, "\n")
  cat("  Matrice de confusion :\n")
  print(table(predicted = pred_test_class, actual = test_sample$dv_voteChoice))
  cat("------------------------------------------------\n")
}

# Afficher le résumé de toutes les itérations
print(results_eval)

# ------------------------------------------------------------------------
# 12) Sauvegarder le modèle final
# ------------------------------------------------------------------------
saveRDS(final_model, "_SharedFolder_datagotchi_federal_2024/data/modele/finalmodelV5.rds")
cat("Modèle sauvegardé avec succès.\n")