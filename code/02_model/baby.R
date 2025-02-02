library(nnet)  
library(caret) 
library(tidyverse)

# 1. Get top predictors from our leaderboard
# Top single variables
top_singles <- all_results %>%
  filter(predictor_type == "single",
         p_value < 0.05) %>%
  head(15) %>%  
  pull(var1)

# Top interactions
top_interactions <- all_results %>%
  filter(predictor_type == "interaction",
         p_value < 0.05) %>%
  head(15) %>%  
  select(var1, var2)

# 2. Prepare the data
model_data <- DataModel %>%
  select(dv_voteChoice, all_of(unique(c(
    top_singles,
    top_interactions$var1,
    top_interactions$var2
  )))) %>%
  na.omit()

# 3. Create formula with both main effects and interactions
main_effects <- paste(unique(c(
  top_singles,
  top_interactions$var1,
  top_interactions$var2
)), collapse = " + ")

interaction_terms <- apply(top_interactions, 1, function(row) {
  paste(row[1], row[2], sep = ":")
})
interaction_effects <- paste(interaction_terms, collapse = " + ")

formula_str <- paste("dv_voteChoice ~", main_effects, "+", interaction_effects)

# 4. Split data
set.seed(123)
train_index <- createDataPartition(model_data$dv_voteChoice, p = 0.7, list = FALSE)
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

# 5. Train model with cross-validation
ctrl <- trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE,
  summaryFunction = multiClassSummary
)

# Train multinomial model
set.seed(123)
vote_model <- train(
  as.formula(formula_str),
  data = train_data,
  method = "multinom",
  trControl = ctrl,
  trace = FALSE
)

# 6. Make predictions
train_pred <- predict(vote_model, train_data)
test_pred <- predict(vote_model, test_data)

# Calculate accuracies
train_accuracy <- mean(train_pred == train_data$dv_voteChoice)
test_accuracy <- mean(test_pred == test_data$dv_voteChoice)

print(paste("Training Accuracy:", round(train_accuracy, 3)))
print(paste("Test Accuracy:", round(test_accuracy, 3)))

# 7. Detailed Evaluation with Confusion Matrices
# Training set confusion matrix
train_conf_matrix <- confusionMatrix(
  factor(train_pred, levels = levels(factor(train_data$dv_voteChoice))),
  factor(train_data$dv_voteChoice)
)

# Test set confusion matrix
test_conf_matrix <- confusionMatrix(
  factor(test_pred, levels = levels(factor(test_data$dv_voteChoice))),
  factor(test_data$dv_voteChoice)
)

# Print detailed evaluation results
print("TRAINING SET EVALUATION:")
print("------------------------")
print("Confusion Matrix:")
print(train_conf_matrix$table)
print("\nDetailed Statistics:")
print(train_conf_matrix$byClass)
print("\nOverall Statistics:")
print(train_conf_matrix$overall)

print("\nTEST SET EVALUATION:")
print("------------------------")
print("Confusion Matrix:")
print(test_conf_matrix$table)
print("\nDetailed Statistics:")
print(test_conf_matrix$byClass)
print("\nOverall Statistics:")
print(test_conf_matrix$overall)

# Calculate per-class metrics
party_metrics <- data.frame(
  Party = levels(factor(test_data$dv_voteChoice)),
  Precision = test_conf_matrix$byClass[,"Precision"],
  Recall = test_conf_matrix$byClass[,"Recall"],
  F1_Score = test_conf_matrix$byClass[,"F1"]
)

print("\nPer-Party Performance Metrics:")
print(party_metrics)

# 8. Create prediction function for new surveys
predict_vote <- function(new_data) {
  # Get probability predictions
  pred_probs <- predict(vote_model, newdata = new_data, type = "prob")
  
  # Get class prediction
  pred_class <- predict(vote_model, newdata = new_data)
  
  # Create result list
  result <- list(
    predicted_party = pred_class,
    probabilities = as.data.frame(pred_probs),
    confidence = apply(pred_probs, 1, max)
  )
  
  return(result)
}

# 9. Save model
saveRDS(vote_model, "multinomial_vote_model.rds")

# Example of how to use the prediction function:
# new_survey <- data.frame(
#   var1 = "value1",
#   var2 = "value2",
#   ...
# )
# prediction <- predict_vote(new_survey)
# print("Predicted Party:", prediction$predicted_party)
# print("Confidence:", prediction$confidence)
# print("All Probabilities:")
# print(prediction$probabilities)
