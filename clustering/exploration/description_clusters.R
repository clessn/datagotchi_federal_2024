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
"immigrant",
"act_transport_PublicTransportation",
"act_transport_Car",
"ses_dwelling_house",
"age34m",
"age55p",
"langFr",
"langEn",
"ses_languageOther",
"female",
"incomeHigh",
"act_VisitsMuseumsGaleries",
"act_Fishing",
"act_Hunting",
"act_MotorizedOutdoorActivities",
"act_Volunteering",
"act_Gym",
"cons_coffee_McDo",
"cons_coffee_place_noCoffee",
"cons_coffee_TimH",
"cons_Meat",
"cons_Vege",
"cons_Vegan",
"cons_regBeers",
"cons_redWineDrink",
"cons_noDrink",
"educHS" 
)

describe_clusters(
  data_num_reduit,
  variables_to_describe = variables_to_describe,
  cluster_var = "cluster_5"
) |> print(n = 271) 