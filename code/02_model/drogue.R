# First, let's organize our data
library(tidyverse)

DataModel <- readRDS("_SharedFolder_datagotchi_federal_2024/data/pilote/DataCleanPilot_2025Janv30.rds")

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
  drop_na()

# La cible en facteur
DataModel$dv_voteChoice <- factor(DataModel$dv_voteChoice)

# 1. Get both lifestyle and ses variables
predictor_vars <- names(DataModel)[grep("^(lifestyle_|ses_)", names(DataModel))]

# 2. Function to calculate Cramer's V
calculate_cramers_v <- function(x, y) {
    complete_cases <- complete.cases(x, y)
    x <- x[complete_cases]
    y <- y[complete_cases]
    
    cont_table <- table(x, y)
    
    if(any(dim(cont_table) <= 1) || any(rowSums(cont_table) == 0) || any(colSums(cont_table) == 0)) {
        return(list(cramers_v = NA, p_value = NA))
    }
    
    chi_test <- tryCatch({
        chisq.test(cont_table)
    }, warning = function(w) {
        chisq.test(cont_table)
    }, error = function(e) {
        return(NULL)
    })
    
    if(is.null(chi_test)) {
        return(list(cramers_v = NA, p_value = NA))
    }
    
    n <- sum(cont_table)
    min_dim <- min(nrow(cont_table), ncol(cont_table)) - 1
    cramers_v <- sqrt(chi_test$statistic / (n * min_dim))
    
    return(list(cramers_v = cramers_v, p_value = chi_test$p.value))
}

# 3. Analyze single variables
single_var_results <- data.frame(
    predictor_type = character(),
    var1 = character(),
    var2 = character(),
    cramers_v = numeric(),
    p_value = numeric(),
    n_categories = numeric(),
    n_valid_cases = numeric(),
    var1_type = character(),
    var2_type = character(),
    stringsAsFactors = FALSE
)

# Process single variables
for(var in predictor_vars) {
    x <- DataModel[[var]]
    y <- DataModel$dv_voteChoice
    
    var_type <- ifelse(grepl("^lifestyle_", var), "lifestyle", "ses")
    
    valid_cases <- sum(complete.cases(x, y))
    n_cats <- length(unique(x[!is.na(x)]))
    
    if(valid_cases > 0 && n_cats > 1) {
        test_result <- calculate_cramers_v(x, y)
        if(!is.na(test_result$cramers_v)) {
            single_var_results <- rbind(single_var_results, data.frame(
                predictor_type = "single",
                var1 = var,
                var2 = NA,
                cramers_v = test_result$cramers_v,
                p_value = test_result$p_value,
                n_categories = n_cats,
                n_valid_cases = valid_cases,
                var1_type = var_type,
                var2_type = NA
            ))
        }
    }
}

# 4. Analyze interactions
interaction_results <- data.frame(
    predictor_type = character(),
    var1 = character(),
    var2 = character(),
    cramers_v = numeric(),
    p_value = numeric(),
    n_categories = numeric(),
    n_valid_cases = numeric(),
    var1_type = character(),
    var2_type = character(),
    stringsAsFactors = FALSE
)

total_pairs <- length(predictor_vars) * (length(predictor_vars) - 1) / 2
pair_count <- 0

print(paste("Testing", total_pairs, "possible interactions..."))

for(i in 1:(length(predictor_vars)-1)) {
    for(j in (i+1):length(predictor_vars)) {
        pair_count <- pair_count + 1
        if(pair_count %% 100 == 0) {
            print(paste("Processed", pair_count, "of", total_pairs, "pairs"))
        }
        
        var1 <- predictor_vars[i]
        var2 <- predictor_vars[j]
        
        var1_type <- ifelse(grepl("^lifestyle_", var1), "lifestyle", "ses")
        var2_type <- ifelse(grepl("^lifestyle_", var2), "lifestyle", "ses")
        
        tryCatch({
            x <- interaction(DataModel[[var1]], DataModel[[var2]])
            y <- DataModel$dv_voteChoice
            
            valid_cases <- sum(complete.cases(x, y))
            n_cats <- length(unique(x[!is.na(x)]))
            
            if(valid_cases > 0 && n_cats > 1) {
                test_result <- calculate_cramers_v(x, y)
                
                if(!is.na(test_result$cramers_v)) {
                    interaction_results <- rbind(interaction_results, data.frame(
                        predictor_type = "interaction",
                        var1 = var1,
                        var2 = var2,
                        cramers_v = test_result$cramers_v,
                        p_value = test_result$p_value,
                        n_categories = n_cats,
                        n_valid_cases = valid_cases,
                        var1_type = var1_type,
                        var2_type = var2_type
                    ))
                }
            }
        }, error = function(e) {
            message("Error processing interaction between: ", var1, " & ", var2)
        })
    }
}

# 5. Combine and sort results
all_results <- rbind(single_var_results, interaction_results)
all_results <- all_results[!is.na(all_results$cramers_v), ]
all_results <- all_results[order(-all_results$cramers_v), ]

# 6. Print summary statistics
print("\nAnalysis Summary:")
print(paste("Total single variables analyzed:", nrow(single_var_results)))
print(paste("Total interactions analyzed:", nrow(interaction_results)))
print(paste("Significant predictors (p < 0.05):", 
           sum(all_results$p_value < 0.05, na.rm = TRUE)))

# 7. Print top 50 leaderboard
print("\nTop 50 Predictors (Single Variables and Interactions):")
top_50 <- head(all_results, 50)
print(top_50)

# 8. Save detailed results
write.csv(all_results, "data/models/combined_predictors_leaderboard.csv", row.names = FALSE)

# 9. Create a clean leaderboard for viewing
leaderboard <- data.frame(
    Rank = 1:nrow(all_results),
    Type = ifelse(all_results$predictor_type == "single", "Single Variable", "Interaction"),
    Predictor = ifelse(all_results$predictor_type == "single", 
                      as.character(all_results$var1), 
                      paste(all_results$var1, "*", all_results$var2)),
    `Cramers_V` = all_results$cramers_v,
    `P_value` = all_results$p_value,
    Categories = all_results$n_categories
) %>%
    head(50)

# Save clean leaderboard
write.csv(leaderboard, "top_50_predictors_leaderboard.csv", row.names = FALSE)
