# Packages ---------------------------------------------------------------
library(dplyr)

# Data -------------------------------------------------------------------

model <- readRDS("_SharedFolder_datagotchi_federal_2024/clustering/data/multinom_model_simple.rds")

kmeans_result <- readRDS("_SharedFolder_datagotchi_federal_2024/clustering/data/kmeans_results8.rds")

app_data <- readRDS("_SharedFolder_datagotchi_federal_2024/clustering/data/app_datagotchi_clean.rds") |> 
  mutate(
    vote_intent = case_when(
      vote_intent == "bloc"    ~ "Bloc",
      vote_intent == "bq"    ~ "Bloc",
      vote_intent == "pcc" ~ "Conservative",
      vote_intent == "vert"   ~ "Green",
      vote_intent == "npd"     ~ "NDP",
      vote_intent == "plc"     ~ "Liberal",
    ),
    vote_intent = factor(vote_intent),
    vote_intent = relevel(vote_intent, ref = "Liberal")
  )

# Create priors ----------------------------------------------------------

coefficients <- as.data.frame(model$coefficients)
errors <- as.data.frame(model$standard.errors)

# Extraire les coefficients
coefficients <- model$coefficients

# Convertir les coefficients en data frame long
coefficients_long <- as.data.frame(as.table(coefficients)) |> 
  filter(Var2 != "(Intercept)")
names(coefficients_long) <- c("category", "variable", "value")

priors <- coefficients_long %>%
  mutate(
    prior = paste0("normal(", round(value, 3), ", 1)"),
    dpar = paste0("mu", category),  # Format attendu des paramètres
    variable_clean = gsub("[()]", "", variable)  # Nettoyer les noms des variables
  ) %>%
  group_by(category, variable_clean) %>%
  summarize(
    prior = paste0(
      "brms::prior(", prior, ", class = 'b', coef = '", variable_clean, "', dpar = '", dpar, "')"
    ),
    .groups = "drop"
  )

# Générer les priors_text comme précédemment
priors_text <- paste(priors$prior, collapse = ",\n")

# Convertir priors_text en liste utilisable par brm
priors_list <- eval(parse(text = paste0("c(", priors_text, ")")))

new_app_data <- app_data[1:100,] |> 
  select(-created)

brms::get_prior(
  vote_intent ~ age55p + age34m + cons_Meat,
  data = new_app_data,
  family = brms::categorical(link = "logit")
)

model <- brms::brm(
  formula = vote_intent ~ .,
  family = brms::categorical(link = "logit"),
  data = new_app_data,
  prior = priors_list,
  chains = 4,
  iter = 2000
)




## Loop: pour chaque jour, faire un modèle bayésien vote_intent ~ SES + lifestyle qui prend comme priors les coefficients du modèle du 1er script
## Prédire ce modèle pour chaque centroid
## Enregistrer une dataframe avec la prédiction de chaque cluster par jour