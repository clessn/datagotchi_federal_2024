## PLAN

### 1. Simuler un dataset de 200-300 répondants par RTA en simulant les données de recensement par RTA et par circonscription
### 2. Modèle multinomial riding ~ rta + ses
### 3. Prédire le modèle pour chaque répondant dans notre sondage. Avoir une probabilité par circonscription
### 4. Pour les RTA couvertes par une seule circonscription, s'assurer qu'elles prédisent la seule circonscription

#### On veut une fonction predict_electoral_riding() qui prend comme arguments:

# predict_spatial_target(
#   survey_data,                 # Données du sondage contenant les répondants à prédire
#   ses,                         # Il faudrait un genre de vecteur qui lie chaque SES de survey_data à ceux des census_data
#   origin,                      # Nom de la variable de l'unité spatiale qu'on a dans le sondage, exemple: "rta"
#   target,                      # Nom de la variable de l'unité spatiale qu'on veut prédire, exemple: electoral_riding
#   spatial_origin,              # Données spatiales de l'unité d'origine
#   spatial_target,               # Données spatiales de l'unité à prédire
#   census_origin,               # Données du recensement par unité d'origine
#   census_target,                # Données du recensement par unité à prédire
#   return = c("probabilities", "class") # est-ce qu'on retourne une probabilité par unité à prédire ou seulement l'unité avec la plus grande prob 
# )

### Et retourne une matrice ou un vecteur


library(dplyr)

survey_data <- readRDS("_SharedFolder_datagotchi_federal_2024/data/pilote/dataClean/datagotchi2025_canada_pilot_20250319.rds") |> 
  # filter(ses_postalCode %in% c("G2L", "J3H", "A0A")) |> 
  select(
    rta = ses_postalCode,
    ses_gender = ses_genderMale, 
    ses_age = ses_ageGroup5Years,
    ses_income = ses_incomeCensus
  ) |> 
  mutate(
    rta = toupper(rta),
    ses_gender = ifelse(ses_gender == 1, "male", "female")
  )

df_census_rta <- cartessn::census_canada_2022_rta |> 
  filter(category != "male")
df_census_ridings <- cartessn::census_canada_2022_electoral_ridings |> 
  filter(category != "20_24")

df <- cartessn::predict_spatial_target(
    survey_data = survey_data[1:15,],
    ses = c("ses_gender", "ses_age", "ses_income"),
    origin = "rta",
    target = "id_riding",
    spatial_origin = cartessn::spatial_canada_2021_rta,
    spatial_target = cartessn::spatial_canada_2022_electoral_ridings,
    census_origin = df_census_rta,
    census_target = df_census_ridings,
    return = "probabilities",
    n_sim = 200
  )
