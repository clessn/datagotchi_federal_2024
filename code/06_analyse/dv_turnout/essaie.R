library(dplyr)
library(ggplot2)
library(forcats)
library(stringr)

# Load data
df <- readRDS("_SharedFolder_datagotchi_federal_2024/data/app/dataClean/datagotchi2025_canada_app_20250403.rds") 

# Define the dependent variable
dv <- "dv_turnout_bin"

# Select SES variables
df_ses <- df %>%
  select(
    ses_genderFemale,
    ses_age_4Cat,
    ses_region,
    ses_language,
    ses_educ_5Cat,
    ses_income3Cat,
    ses_incomeCensus,
    ses_ethnicityWhite,
    ses_sexOrientationHetero,
    ses_immigrant,
    ses_dwelling_cat,
    ses_riding_id,
  )
ses <- c(names(df_ses))
print(ses)

# Select lifestyle variables
df_lifestyle <- df %>%
  select(starts_with("lifestyle_")) %>%
  select(
    starts_with("lifestyle_exercise"),
    -lifestyle_exercise,
    lifestyle_goFishingFreq_bin,
    lifestyle_goHuntingFreq_bin,
    lifestyle_goMuseumsFreq_bin,
    lifestyle_motorizedActFreq_bin,
    lifestyle_volunteeringFreq_bin,
    starts_with("lifestyle_typeTransport"),
    -lifestyle_typeTransport,
    starts_with("lifestyle_consClothes"),
    -lifestyle_consClothes,
    starts_with("lifestyle_consCoffee"),
    -lifestyle_consCoffee,
    lifestyle_ownPet_bin,
    starts_with("lifestyle_favAlcool"),
    -lifestyle_favAlcool,
    starts_with("lifestyle_clothingStyle"),
    -lifestyle_clothingStyle,
    starts_with("lifestyle_hasTattoo")
  )
lifestyle <- c(names(df_lifestyle))
print(lifestyle)

# Run models function
run_models <- function(lifestyle_vars, ses_vars, dv, data) {
  models <- list()
  
  for (lv in lifestyle_vars) {
    formula_str <- paste0(
      dv, " ~ ", lv, " + ", 
      paste(ses_vars, collapse = " + ")
    )
    formula_obj <- as.formula(formula_str)
    
    model <- glm(formula_obj, data = data, family = "binomial")
    models[[lv]] <- model
  }
  
  return(models)
}

# Run models for all lifestyle variables
models <- run_models(lifestyle, ses, dv, df)

# Extract coefficients
model_results <- data.frame(
  lifestyle_var = character(),
  estimate = numeric(),
  std_error = numeric(),
  z_value = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

for (lv in names(models)) {
  model <- models[[lv]]
  coef_summary <- summary(model)$coefficients
  
  # Get the row for the lifestyle variable
  if (lv %in% rownames(coef_summary)) {
    lv_row <- coef_summary[lv, ]
    
    model_results <- rbind(
      model_results,
      data.frame(
        lifestyle_var = lv,
        estimate = lv_row[1],
        std_error = lv_row[2],
        z_value = lv_row[3],
        p_value = lv_row[4],
        stringsAsFactors = FALSE
      )
    )
  } else {
    cat("Warning: Variable", lv, "not found in coefficient summary\n")
  }
}

# Sort by p-value
model_results <- model_results %>%
  arrange(p_value)

# Add confidence intervals and clean variable names
model_results <- model_results %>%
  mutate(
    conf_low = estimate - 1.96 * std_error,
    conf_high = estimate + 1.96 * std_error,
    significant = p_value < 0.05,
    # Clean up variable names for display
    lifestyle_var_clean = str_replace_all(lifestyle_var, "lifestyle_", ""),
    lifestyle_var_clean = str_replace_all(lifestyle_var_clean, "([a-z])([A-Z])", "\\1 \\2"),
    lifestyle_var_clean = str_replace_all(lifestyle_var_clean, "_", " "),
    lifestyle_var_clean = str_to_title(lifestyle_var_clean),
    # For variables with "bin" suffix, remove it
    lifestyle_var_clean = str_replace(lifestyle_var_clean, " Bin$", "")
  )

# Keep only top 25 most significant results (if there are too many)
top_results <- model_results %>%
  head(25)

# Reorder factors by estimate value
top_results <- top_results %>%
  mutate(lifestyle_var_clean = fct_reorder(lifestyle_var_clean, estimate))

# Create the coefficient plot
coef_plot <- ggplot(top_results, aes(x = estimate, y = lifestyle_var_clean)) +
  # Add a vertical line at x=0 for reference
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  # Add horizontal error bars for confidence intervals
  geom_errorbarh(aes(xmin = conf_low, xmax = conf_high, color = significant), 
                height = 0.3, linewidth = 0.5) +
  # Add points for estimates
  geom_point(aes(color = significant), size = 3) +
  # Set colors
  scale_color_manual(values = c("FALSE" = "gray50", "TRUE" = "#0072B2")) +
  # Labels and title
  labs(
    title = "Effect of Lifestyle Factors on Voter Turnout",
    subtitle = "Logistic Regression Coefficients with 95% Confidence Intervals",
    x = "Log Odds Coefficient",
    y = NULL,
    caption = "Controls: demographics, socioeconomic status, and geographic factors\nSignificant coefficients (p < 0.05) shown in blue"
  ) +
  # Theme customizations
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "none",
    axis.text.y = element_text(hjust = 0)
  )

# Save the plot (optional)
# ggsave("lifestyle_effects_on_turnout.png", plot = coef_plot, width = 10, height = 8)

# Display the plot
print(coef_plot)

# Create an alternative plot with color indicating p-value strength
pvalue_plot <- ggplot(top_results, aes(x = estimate, y = lifestyle_var_clean)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbarh(aes(xmin = conf_low, xmax = conf_high), 
                height = 0.3, linewidth = 0.5, color = "gray70") +
  geom_point(aes(color = -log10(p_value)), size = 3) +
  scale_color_gradient(low = "gray70", high = "#0072B2",
                       name = "-log10(p-value)") +
  labs(
    title = "Effect of Lifestyle Factors on Voter Turnout",
    subtitle = "Logistic Regression Coefficients with 95% Confidence Intervals",
    x = "Log Odds Coefficient",
    y = NULL,
    caption = "Controls: demographics, socioeconomic status, and geographic factors\nDarker blue indicates greater statistical significance"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(hjust = 0)
  )

# Display the alternative plot
print(pvalue_plot)
