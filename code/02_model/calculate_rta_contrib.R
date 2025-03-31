# Load required packages
library(tidyverse)

# Load the RTA predictions
rta_predictions <- read.csv("_SharedFolder_datagotchi_federal_2024/data/modele/rta_predictions_partis.csv",
                           stringsAsFactors = FALSE)

# Load the model to get the coefficients
final_model <- readRDS("_SharedFolder_datagotchi_federal_2024/data/modele/finalmodel_withRTAPredictions_2025-04-15.rds")

# Get the symmetric coefficients
sym_coef <- final_model$sym_coef

# Extract the RTA-related coefficients for each party
rta_coefficients <- data.frame(
  party = rownames(sym_coef),
  prediction_CPC = sym_coef[, "prediction_CPC"],
  prediction_LPC = sym_coef[, "prediction_LPC"],
  prediction_NDP = sym_coef[, "prediction_NDP"],
  prediction_GPC = sym_coef[, "prediction_GPC"],
  prediction_BQ = sym_coef[, "prediction_BQ"]
)

# Create a function to calculate the pre-computed contribution for each RTA
calculate_rta_contribution <- function(rta_row, rta_coefficients) {
  # Create an empty dataframe to store results
  results <- data.frame(
    rta = rta_row$rta,
    party = rta_coefficients$party,
    contribution = NA_real_
  )
  
  for (i in 1:nrow(rta_coefficients)) {
    party_row <- rta_coefficients[i, ]
    party_name <- party_row$party
    
    # Calculate the contribution: sum of (prediction_X * coefficient for prediction_X)
    contribution <- 
      rta_row$CPC * party_row$prediction_CPC +
      rta_row$LPC * party_row$prediction_LPC +
      rta_row$NDP * party_row$prediction_NDP +
      rta_row$GPC * party_row$prediction_GPC +
      rta_row$BQ * party_row$prediction_BQ
    
    # Store in results
    results[results$party == party_name, "contribution"] <- contribution
  }
  
  return(results)
}

# Pre-calculate the contribution for each RTA
rta_contributions <- list()

for (i in 1:nrow(rta_predictions)) {
  rta_row <- rta_predictions[i, ]
  contributions <- calculate_rta_contribution(rta_row, rta_coefficients)
  rta_contributions[[i]] <- contributions
}

# Combine all results
all_contributions <- do.call(rbind, rta_contributions)

# Reshape to wide format for easier use
rta_contribution_wide <- all_contributions %>%
  pivot_wider(
    id_cols = rta,
    names_from = party,
    values_from = contribution
  )

# Calculate default values for RTAs not in the table
default_contributions <- data.frame(
  rta = "DEFAULT", 
  stringsAsFactors = FALSE
)

# Calculate the average for each party to use as default
for (party in rta_coefficients$party) {
  mean_contribution <- mean(all_contributions[all_contributions$party == party, "contribution"])
  default_contributions[[party]] <- mean_contribution
}

# Add the default row to the table
rta_contribution_wide <- rbind(rta_contribution_wide, default_contributions) %>%
  select(rta, lpc, gpc, cpc, bq, ndp)

# Save the pre-calculated contributions
write.csv(rta_contribution_wide, 
          "_SharedFolder_datagotchi_federal_2024/data/modele/rta_precalculated_contributions.csv", 
          row.names = FALSE)

# Print summary
cat("Pre-calculated RTA contributions for", nrow(rta_predictions), "RTAs\n")
cat("For", length(unique(all_contributions$party)), "parties:", 
    paste(unique(all_contributions$party), collapse=", "), "\n")
cat("Output saved to: _SharedFolder_datagotchi_federal_2024/data/modele/rta_precalculated_contributions.csv\n")

# Example of how to use the lookup table (for developers)
example_rta <- "K1P"
lookup_example <- if (example_rta %in% rta_contribution_wide$rta) {
  rta_contribution_wide[rta_contribution_wide$rta == example_rta, ]
} else {
  # Use default if not found
  rta_contribution_wide[rta_contribution_wide$rta == "DEFAULT", ]
}

cat("\nExample lookup for RTA", example_rta, ":\n")
print(lookup_example)
