optimize_ideological_matrix_improved <- function(model, val_data, true_labels, 
  parties, iterations = 150,
  learning_rate = 0.05, patience = 20) {
# Fonction d'évaluation pour une matrice donnée
evaluate_matrix <- function(matrix) {
tryCatch({
# Copier le modèle et configurer la matrice
temp_model <- model
temp_model$ideological_proximity <- matrix

# S'assurer que la classe est bien définie
if(!("multinom_enhanced" %in% class(temp_model))) {
class(temp_model) <- c("multinom_enhanced", class(temp_model))
}

# Prédire et calculer l'accuracy
pred <- predict(temp_model, val_data, use_ideology = TRUE)
acc <- mean(pred == true_labels, na.rm = TRUE)

# Calculer aussi les performances par parti
party_metrics <- list()
for(party in unique(true_labels)) {
party_indices <- which(true_labels == party)
if(length(party_indices) > 0) {
party_acc <- mean(pred[party_indices] == party, na.rm = TRUE)
party_metrics[[party]] <- party_acc
}
}

return(list(accuracy = acc, party_metrics = party_metrics))
}, error = function(e) {
cat("Erreur d'évaluation:", e$message, "\n")
return(list(accuracy = -1, party_metrics = NULL))
})
}

# Initialiser avec la matrice actuelle si disponible
if(!is.null(model$ideological_proximity)) {
current_matrix <- model$ideological_proximity
best_matrix <- current_matrix
} else {
# Sinon, créer une nouvelle matrice diagonale (identité)
n_parties <- length(parties)
current_matrix <- matrix(0, nrow = n_parties, ncol = n_parties)
rownames(current_matrix) <- parties
colnames(current_matrix) <- parties
diag(current_matrix) <- 1.0
best_matrix <- current_matrix
}

# Évaluation initiale
result <- evaluate_matrix(current_matrix)
best_accuracy <- result$accuracy
best_party_metrics <- result$party_metrics

cat("Accuracy initiale:", best_accuracy, "\n")
if(!is.null(best_party_metrics)) {
cat("Métriques par parti:\n")
for(party in names(best_party_metrics)) {
cat("  ", party, ":", round(best_party_metrics[[party]] * 100, 1), "%\n")
}
}

# Variables pour la détection de convergence
no_improvement_counter <- 0
current_lr <- learning_rate

# Optimisation itérative
for(iter in 1:iterations) {
improved <- FALSE

# Essayer d'améliorer la matrice actuelle
for(i in 1:nrow(current_matrix)) {
for(j in 1:nrow(current_matrix)) {
if(i != j) {  # Éviter la diagonale
# Matrice test avec une petite perturbation
test_matrix <- current_matrix

# Appliquer une perturbation aléatoire entre -lr et +lr
delta <- runif(1, -current_lr, current_lr)
new_value <- max(0, min(1, current_matrix[i, j] + delta))

# Mettre à jour la valeur et sa symétrique
test_matrix[i, j] <- new_value
test_matrix[j, i] <- new_value  # Maintenir la symétrie

# Évaluer cette nouvelle matrice
result <- evaluate_matrix(test_matrix)

# Si meilleure globalement, mettre à jour
if(result$accuracy > best_accuracy) {
best_matrix <- test_matrix
best_accuracy <- result$accuracy
best_party_metrics <- result$party_metrics
current_matrix <- test_matrix  # Continuer à partir de cette amélioration
improved <- TRUE

cat("Itération", iter, "- Nouvelle meilleure accuracy:", 
round(best_accuracy * 100, 2), "%\n")

# Réinitialiser le compteur de patience
no_improvement_counter <- 0
}
}
}
}

# Si pas d'amélioration dans cette itération
if(!improved) {
no_improvement_counter <- no_improvement_counter + 1

# Réduire le learning rate après un certain nombre d'itérations sans amélioration
if(no_improvement_counter %% 5 == 0) {
current_lr <- current_lr * 0.8
cat("Réduction du learning rate à", current_lr, "\n")
}

# Arrêter si aucune amélioration depuis trop longtemps
if(no_improvement_counter >= patience) {
cat("Arrêt anticipé après", iter, "itérations sans amélioration\n")
break
}
}
}

# Afficher les métriques finales par parti
cat("\nMétriques finales par parti:\n")
if(!is.null(best_party_metrics)) {
for(party in names(best_party_metrics)) {
cat("  ", party, ":", round(best_party_metrics[[party]] * 100, 1), "%\n")
}
}

return(list(
matrix = best_matrix,
accuracy = best_accuracy,
party_metrics = best_party_metrics
))
}
