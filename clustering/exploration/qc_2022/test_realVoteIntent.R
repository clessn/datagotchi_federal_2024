
# Library ----------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)


# Load Centroids et Data -------------------------------------------------

# We'll reuse your original objects if needed:
# model0 <- readRDS(".../multinom_model_2022.rds")  # Potentially not needed if not re-fitting daily
kmeans_result <- readRDS("_SharedFolder_datagotchi_federal_2024/clustering/data/kmeans_results2022.rds")

# The 2022 app data need some cleaning
app2022 <- readRDS("/home/alexab/Dropbox/Ulaval/CLESSN/_SharedFolder_datagotchi-developpement/quebec_prov_2022/hub/data-hub-clean-2022-10-27_clean.rds") |> 
  rename(
    ses_genderOther = genderOther,
    act_TeamSport = act_Team,
    cons_regBeers = cons_beerDrink,
    cons_cocktailsDrink = cons_cocktailDrink,
    ses_dwelling_app = ses_dwelling_App,
    cons_coffee_place_noCoffee = cons_coffee_None,
    ses_ethn_Asiatique = ses_ethn_Asian
  ) |>
  mutate(
    # Income 
    incomeLow = if_else(ses_income_None == 1 | ses_income_i1to30 == 1, 1, 0),
    incomeMid = if_else(ses_income_i31to60 == 1 | ses_income_i61to90 == 1 | ses_income_i91to110 == 1, 1, 0),
    incomeHigh = if_else(ses_income_i111to150 == 1 | ses_income_i151to200 == 1, 1, 0),
    
    # Meat consumption 
    cons_low_Meat = if_else(cons_meat_never == 1 | cons_meat_almost_never == 1 | cons_meat_once_month == 1, 1, 0),
    cons_mid_Meat = if_else(cons_meat_once_week == 1 | cons_meat_few_week == 1, 1, 0),
    cons_much_Meat = if_else(cons_meat_daily == 1 | cons_meat_few_daily == 1, 1, 0)
  )

# Create a uniform vote_intent and date fields
app_data <- app2022 |> 
  mutate(
    vote_intent = case_when(
      op_intent == "CAQ"           ~ "CAQ",
      op_intent == "PQ"            ~ "PQ",
      op_intent == "PLQ"           ~ "PLQ",
      op_intent == "QS"            ~ "QS",
      op_intent == "PCQ"           ~ "PCQ",
      op_intent == "Other"         ~ "Autre",
      op_intent == "Did not vote"  ~ "NoVote",
      TRUE                         ~ NA_character_
    ),
    date = as.Date(time)
  ) |> 
  drop_na(vote_intent)  # Only keep rows with a recognized vote intent

# You also have data_prior from "4-2022_model" if needed for reference. 
# For cluster assignment to new respondents, the key is consistent processing 
# with how your K-means was originally done.


# Functions -------------------------------------------

# to process app data
process_app_data <- function(data, center_vals, scale_vals) {
  # 1) Select the same columns you used for K-means
  data_selected <- data %>%
    select(
      vote_intent,
      male,
      female,
      ses_genderOther,
      age34m,
      age3554,
      age55p,
      langEn,
      langFr,
      ses_languageOther,
      act_Gym,
      act_TeamSport,
      act_Run,
      act_Yoga,
      act_None,
      act_Fishing,
      act_Hunting,
      act_VisitsMuseumsGaleries,
      act_MotorizedOutdoorActivities,
      act_Volunteering,
      animal_cat,
      animal_dog,
      cons_brand_MaR,
      cons_brand_Frip,
      educBHS,
      educCollege,
      educUniv,
      cons_redWineDrink,
      cons_regBeers,
      cons_cocktailsDrink,
      cons_noDrink,
      incomeLow,
      incomeMid,
      incomeHigh,
      ses_dwelling_app,
      ses_dwelling_detachedHouse,
      act_transport_Car,
      act_transport_Walk,
      act_transport_PublicTransportation,
      vehicule_PickUp,
      vehicule_noCar,
      immigrant,
      cons_coffee_TimH,
      cons_coffee_Starbucks,
      cons_coffee_place_noCoffee,
      app_noTattoo,
      cons_low_Meat,
      cons_mid_Meat,
      cons_much_Meat,
      ses_ethn_White,
      ses_ethn_Black,
      ses_ethn_Asiatique,
      ses_hetero,
      ses_gai,
      ses_bisex
    ) %>%
    drop_na()  # remove any incomplete rows
  
  # 2) Isolate vote_intent so we do NOT scale it
  vote_int <- data_selected$vote_intent
  
  # 3) Prepare to scale the numeric columns
  #    (all columns except vote_intent)
  numeric_cols <- setdiff(names(data_selected), "vote_intent")
  data_numeric <- data_selected[, numeric_cols, drop = FALSE]  # 54 numeric columns

  # 4) Scale using the original center/scale
  #    We assume center_vals and scale_vals each have entries named exactly 
  #    like 'male', 'female', 'age34m', etc.
  #    => If some columns are missing from center_vals, you must fix that.
  data_scaled <- sweep(data_numeric, 2, center_vals[numeric_cols], FUN = "-")
  data_scaled <- sweep(data_scaled, 2, scale_vals[numeric_cols], FUN = "/")

  # 5) Put vote_intent back
  data_scaled$vote_intent <- vote_int

  # Return the scaled dataframe (54 numeric cols + 1 factor/char col for vote)
  data_scaled
}

# to assign Clusters to Each Respondent 
assign_clusters <- function(data, centroids) {
  # data: scaled numeric columns (no vote_intent)
  # centroids: same columns, from kmeans_result$centers
  distances <- matrix(NA, nrow = nrow(data), ncol = nrow(centroids))

  for (k in seq_len(nrow(centroids))) {
    diff <- sweep(data, 2, centroids[k, ], FUN = "-")
    distances[, k] <- rowSums(diff^2)  # Euclidean dist^2
  }

  cluster_assignment <- apply(distances, 1, which.min)
  cluster_assignment
}


# tracking each day respondents ------------------------------------------
##Convert Dates to Day Indices
# Convert the date to day indices
app_data$date <- as.Date(app_data$date)
unique_dates <- sort(unique(app_data$date))
date_to_day_number <- data.frame(
  date = unique_dates,
  day  = seq_along(unique_dates)
)

app_data <- app_data %>%
  left_join(date_to_day_number, by = "date") %>%
  arrange(date)

# We'll store daily cluster assignments
all_assignments <- data.frame()

centroids <- as.data.frame(kmeans_result$centers)  # scaled centroids from your K-means

for (current_day in date_to_day_number$day) {
  
  # Filter up to current day (cumulative)
  data_day <- app_data %>%
    filter(day <= current_day)
  
  if (nrow(data_day) == 0) next
  
  # Process & scale
  data_day_processed <- process_app_data(data_day, orig_center, orig_scale)
  if (nrow(data_day_processed) == 0) next
  
  # Exclude vote_intent from distance calculations
  feature_cols <- setdiff(names(data_day_processed), "vote_intent")
  
  # Assign each respondent to the nearest centroid
  cluster_assignment <- assign_clusters(
    data_day_processed[, feature_cols, drop = FALSE],
    centroids[, feature_cols, drop = FALSE]
  )
  
  # Store cluster & day
  data_day_processed$cluster <- cluster_assignment
  data_day_processed$day     <- current_day
  
  # Append to all_assignments
  all_assignments <- bind_rows(all_assignments, data_day_processed)
}

# 'all_assignments' now contains for each respondent:
# - their cluster
# - their actual vote_intent
# - (plus all the other columns for reference)
# - the day index (which you can interpret as "all data up to this day" 
#   if cumulative).



# Graph ------------------------------------------------------------------

# Group by day, cluster, and actual vote_intent
vote_distribution <- all_assignments %>%
  group_by(day, cluster, vote_intent) %>%
  summarise(num_respondents = n(), .groups = "drop") %>%
  group_by(day, cluster) %>%
  mutate(
    cluster_total = sum(num_respondents),
    percent = 100 * num_respondents / cluster_total
  )

# Let's see how it looks
head(vote_distribution)

# Basic plot:
ggplot(vote_distribution, aes(x = day, y = percent, color = vote_intent)) +
  geom_line() +
  facet_wrap(~ cluster) +
  labs(
    title = "Real Vote by Cluster Over Time",
    x = "Day",
    y = "Vote Intent (%)"
  ) +
  theme_minimal()