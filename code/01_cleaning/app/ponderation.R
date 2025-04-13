library(dplyr)
library(tidyr)

# Function to add weights to a dataset
add_weights_to_dataframe <- function(DataClean, df_weights_path = "_SharedFolder_datagotchi_federal_2024/data/weights/df_weights_clean.rds") {
  # Load weighting data
  df_weights_clean <- readRDS(df_weights_path)
  
  # Prepare weight data
  weights_base <- df_weights_clean %>%
    mutate(
      # Convert value to character if needed
      value = as.character(value)
    ) %>%
    # Calculate proportions for each variable
    group_by(variable) %>%
    mutate(
      total_population = sum(population),
      proportion = population / total_population
    ) %>%
    ungroup()
  
  # Determine which variables to use for weighting
  # Find which variables in the weights exist in our dataset
  var_names <- unique(weights_base$variable)
  available_vars <- intersect(names(DataClean), var_names)
  
  # If no matching variables are found, return the dataset with uniform weights
  if (length(available_vars) == 0) {
    warning("No matching variables found for weighting. Using uniform weights.")
    return(DataClean %>% mutate(weight = 1.0))
  }
  
  # Prepare the dataset for weighting
  data_converted <- DataClean %>%
    mutate(across(all_of(available_vars), as.character))
  
  # Initialize with unit weights
  result <- data_converted %>% mutate(weight = 1.0)
  
  # Weight by each available variable
  for (var in available_vars) {
    # Check for missing values
    missing_count <- sum(is.na(result[[var]]))
    if (missing_count > 0) {
      warning(paste0("Variable '", var, "' contains ", missing_count, " missing values"))
    }
    
    # Current distribution
    current_dist <- result %>% 
      group_by(!!sym(var)) %>%
      summarise(count = n(), .groups = "drop") %>%
      mutate(proportion = count / sum(count))
    
    # Target distribution
    target_dist <- weights_base %>%
      filter(variable == var) %>%
      select(value, proportion)
    
    # Calculate weighting factors
    weight_factors <- current_dist %>%
      left_join(target_dist, by = setNames("value", var)) %>%
      mutate(
        factor = ifelse(!is.na(proportion.y), 
                        proportion.y / proportion.x, 
                        1)
      )
    
    # Apply weighting factors
    result <- result %>%
      left_join(
        weight_factors %>% 
          select(!!sym(var), factor), 
        by = setNames(var, var)
      ) %>%
      mutate(
        weight = weight * ifelse(!is.na(factor), factor, 1),
        factor = NULL
      )
  }
  
  # Cap weights at a maximum of 5
  result <- result %>% 
    mutate(weight = pmin(weight, 5))
  
  # Normalize weights
  result <- result %>% 
    mutate(weight = weight * (nrow(DataClean) / sum(weight)))
  
  # Restore original column types
  for (col in names(DataClean)) {
    if (col %in% names(result) && !col %in% available_vars) {
      result[[col]] <- DataClean[[col]]
    }
  }
  
  # Return the weighted dataset
  return(result)
}

# Example usage:
DataClean <- add_weights_to_dataframe(DataClean)
