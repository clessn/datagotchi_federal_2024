# Packages ---------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(cluster)
library(factoextra)
library(tidyr)
library(ggcorrplot)

# Description des clusters -----------------------------------------------

#### Function
describe_clusters <- function(data, variables_to_describe, cluster_var){
  data$cluster_var <- data[[cluster_var]]
  ### variables to dummy
  non_numeric_vars <- variables_to_describe[!sapply(data[,variables_to_describe], is.numeric)]
  if (!purrr::is_empty(non_numeric_vars)) {
    df_dummy_only <- data |> 
      select(all_of(non_numeric_vars)) |> 
      fastDummies::dummy_columns(
        select_columns = non_numeric_vars,
        omit_colname_prefix = TRUE,
        remove_selected_columns = TRUE
      ) |> 
      janitor::clean_names()
    variables_to_describe <- c(variables_to_describe[!variables_to_describe %in% non_numeric_vars], names(df_dummy_only))
    df_description <- cbind(data, df_dummy_only) |> 
      tidyr::pivot_longer(
        cols = all_of(variables_to_describe),
        names_to = "variable",
        values_to = "value"
      )
  } else {
      df_description <- data |> 
        tidyr::pivot_longer(
          cols = all_of(variables_to_describe),
          names_to = "variable",
          values_to = "value"
        )
  }
  df_mean_all <- df_description |> 
    group_by(variable) |> 
    summarise(
      mean_value = mean(value),
      sd_value = sd(value)
    )
  df_mean_by_cluster <- df_description |> 
    group_by(cluster_var, variable) |> 
    summarise(mean_cluster = mean(value)) %>% 
    left_join(
      ., df_mean_all, by = "variable"
    ) |> 
    mutate(z_score = (mean_cluster - mean_value) / sd_value)
  return(df_mean_by_cluster)
}

variables_to_describe <- c(
  "act_VisitsMuseumsGaleries", 
  "act_Volunteering", 
  "act_Yoga", 
  "act_Run", 
  "act_Gym", 
  "act_MotorizedOutdoorActivities",
  "app_noTattoo", 
  "app_swag_Casual", 
  "app_swag_VintageHippBoheme",
  "cons_regBeers", 
  "cons_cocktailsDrink", 
  "cons_microBeers", 
  "cons_redWineDrink", 
  "cons_noDrink",
  "cons_brand_ChainesB", 
  "cons_brand_GSurf", 
  "cons_brand_MaR", 
  "cons_brand_Frip",
  "cons_coffee_Starbucks", 
  "cons_coffee_place_noCoffee",
  "cons_Meat",
  "cons_SmokeNever", 
  "cons_Smoke",
  "immigrant", 
  "educUniv", 
  "age55p", 
  "male", 
  "ses_hetero", 
  "langFr", 
  "incomeHigh",
  "ses_dwelling_condo", 
  "ses_dwelling_detachedHouse",
  "act_transport_PublicTransportation", 
  "act_transport_Car"
)

describe_clusters(
  data_select,
  variables_to_describe = variables_to_describe,
  cluster_var = "cluster_10"
) |> print(n = 340) 
