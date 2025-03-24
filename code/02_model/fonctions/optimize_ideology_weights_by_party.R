optimize_ideology_weights_by_party <- function(model, validation_data, true_labels, 
  weight_range = c(0.5, 0.95), step = 0.05) {
# Récupérer les partis
parties <- levels(true_labels)

# Initialiser les poids optimaux à la valeur par défaut
optimal_weights <- rep(0.7, length(parties))
names(optimal_weights) <- parties

# Évaluation de base avec poids uniformes
temp_model <- model
temp_model$ideology_params <- list(original_weight = 0.7, party_weights = NULL)

if(!("multinom_enhanced_v2" %in% class(temp_model))) {
class(temp_model) <- c("multinom_enhanced_v2", 
class(temp_model)[!class(temp_model) %in% c("multinom_enhanced", "multinom_enhanced_v2")])
}

# Définir la fonction predict.multinom_enhanced_v2 si elle n'existe pas
if(!exists("predict.multinom_enhanced_v2")) {
# Version améliorée de la fonction predict
predict.multinom_enhanced_v2 <- function(object, newdata, type = "class", use_ideology = TRUE, ...) {
# Vérifier si le modèle a la matrice de proximité idéologique
if (use_ideology && is.null(object$ideological_proximity)) {
warning("La matrice de proximité idéologique n'est pas définie. Continuera sans correction idéologique.")
use_ideology <- FALSE
}

# Faire la prédiction standard de multinom
if (type == "class") {
# Pour les classes, on utilise le comportement standard
pred_class <- NextMethod("predict")
if (!use_ideology) {
return(pred_class)
}
# Pour appliquer l'idéologie, on a besoin des probabilités
pred_probs <- NextMethod("predict", type = "probs")
} else if (type == "probs") {
# Pour les probabilités, on utilise le comportement standard
pred_probs <- NextMethod("predict", type = "probs")
if (!use_ideology) {
return(pred_probs)
}
} else {
stop("Type de prédiction non supporté: ", type)
}

# Appliquer la logique idéologique pour lisser les probabilités
if (use_ideology) {
smoothed_probs <- matrix(0, nrow = nrow(pred_probs), ncol = ncol(pred_probs))
colnames(smoothed_probs) <- colnames(pred_probs)

# Récupérer les poids par parti s'ils existent
if (!is.null(object$ideology_params$party_weights)) {
party_weights <- object$ideology_params$party_weights
} else {
# Sinon utiliser le poids original pour tous les partis
party_weights <- rep(object$ideology_params$original_weight, ncol(pred_probs))
names(party_weights) <- colnames(pred_probs)
}

for (i in 1:nrow(pred_probs)) {
for (j in 1:ncol(pred_probs)) {
party_j <- colnames(pred_probs)[j]

# Utiliser le poids spécifique à ce parti
original_weight <- party_weights[party_j]
ideology_weight <- 1 - original_weight

# Pondérer par la proximité idéologique
weighted_sum <- 0
for (k in 1:ncol(pred_probs)) {
party_k <- colnames(pred_probs)[k]

# Récupérer le facteur de proximité (ou utiliser 0 si non défini)
prox_factor <- object$ideological_proximity[tolower(party_j), tolower(party_k)]
weighted_sum <- weighted_sum + (pred_probs[i, k] * prox_factor)
}

# Combiner probabilité originale et lissage idéologique
smoothed_probs[i, j] <- pred_probs[i, j] * original_weight + weighted_sum * ideology_weight
}
}

# Renormaliser pour que la somme des probabilités soit 1
row_sums <- rowSums(smoothed_probs)
smoothed_probs <- smoothed_probs / row_sums
pred_probs <- smoothed_probs
}

if (type == "class") {
# Retourner les classes prédites basées sur les probabilités modifiées
predicted_indices <- max.col(pred_probs)
return(factor(colnames(pred_probs)[predicted_indices], levels = colnames(pred_probs)))
} else {
# Retourner les probabilités modifiées
return(pred_probs)
}
}
}

# Définir la méthode si non définie
if(!exists("predict.multinom_enhanced_v2", mode = "function")) {
assignInNamespace("predict.multinom_enhanced_v2", predict.multinom_enhanced_v2, ns = "package:nnet")
}

# Prédiction de base
base_pred <- predict(temp_model, validation_data, use_ideology = TRUE)
base_accuracy <- mean(base_pred == true_labels)

cat("Accuracy de base avec poids uniforme (0.7):", round(base_accuracy * 100, 2), "%\n")

# Optimiser séparément pour chaque parti
for(p in 1:length(parties)) {
party <- parties[p]
cat("\nOptimisation pour", party, "...\n")

# Indices des observations de ce parti
party_indices <- which(true_labels == party)

if(length(party_indices) == 0) {
cat("Aucune observation pour ce parti dans l'ensemble de validation\n")
next
}

best_acc <- 0
best_weight <- 0.7  # Valeur par défaut

# Tester différents poids
weights <- seq(from = weight_range[1], to = weight_range[2], by = step)

for(weight in weights) {
# Créer des poids par parti, en utilisant le poids actuel pour ce parti
party_weights <- optimal_weights
party_weights[party] <- weight

# Configurer le modèle avec ces poids
temp_model$ideology_params$party_weights <- party_weights

# Prédire et évaluer
tryCatch({
# Prédire sur tout le jeu de validation, mais évaluer seulement pour ce parti
pred <- predict(temp_model, validation_data, use_ideology = TRUE)
party_acc <- mean(pred[party_indices] == true_labels[party_indices], na.rm = TRUE)

cat("  Poids:", weight, "- Accuracy pour", party, ":", 
round(party_acc * 100, 1), "%\n")

if(party_acc > best_acc) {
best_acc <- party_acc
best_weight <- weight
}
}, error = function(e) {
cat("  Erreur avec poids", weight, "pour", party, ":", e$message, "\n")
})
}

cat("Meilleur poids pour", party, ":", best_weight, "avec accuracy:", 
round(best_acc * 100, 1), "%\n")
optimal_weights[party] <- best_weight
}

# Évaluation finale avec les poids optimisés
temp_model$ideology_params$party_weights <- optimal_weights
final_pred <- predict(temp_model, validation_data, use_ideology = TRUE)
final_accuracy <- mean(final_pred == true_labels)

cat("\nAccuracy finale avec poids par parti:", round(final_accuracy * 100, 2), "%\n")
cat("Amélioration par rapport au poids uniforme:", 
round((final_accuracy - base_accuracy) * 100, 2), "%\n")

# Évaluer par parti
for(party in parties) {
party_indices <- which(true_labels == party)
if(length(party_indices) > 0) {
base_party_acc <- mean(base_pred[party_indices] == true_labels[party_indices])
final_party_acc <- mean(final_pred[party_indices] == true_labels[party_indices])

cat("  ", party, "- Avant:", round(base_party_acc * 100, 1), "%, Après:", 
round(final_party_acc * 100, 1), "%, Diff:", 
round((final_party_acc - base_party_acc) * 100, 1), "%\n")
}
}

return(list(
party_weights = optimal_weights,
accuracy = final_accuracy
))
}