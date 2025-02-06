library(tidyverse)
library(factoextra)


fct_02_preparation_donnees <- function(df_data) {
  ##----------------------------------------
  ## 1- Préparation des variables d'activité
  ##----------------------------------------
  df_act <- df_data %>%
    select(all_of(variables_act))

  # df_act$act_Gym <- as.numeric(df_act$act_Gym)
  # df_act$act_Walk <- as.numeric(df_act$act_Walk)
  # df_act$act_Run <- as.numeric(df_act$act_Run)
  # df_act$act_Yoga <- as.numeric(df_act$act_Yoga)
  # df_act$act_Other <- as.numeric(df_act$act_Other)
  # df_act$act_None <- as.numeric(df_act$act_None)

  # Transformer les variables avec nuance (0, 0.25, 0.5, 0.75, 1)

  # Comme les 1 sont peu nombreux, il semble avantageux de transformer les 
  # variables en 0, 0.5 et 1 où
  # - 0.25 est intégré à 0.5
  # - 0.75 est intégré à 1

  df_act$act_Fishing <- ifelse(df_act$act_Fishing == 0.25, 0.5, ifelse(df_act$act_Fishing == 0.75, 1, df_act$act_Fishing))
  df_act$act_Hunting <- ifelse(df_act$act_Hunting == 0.25, 0.5, ifelse(df_act$act_Hunting == 0.75, 1, df_act$act_Hunting))
  df_act$act_VisitsMuseumsGaleries <- ifelse(df_act$act_VisitsMuseumsGaleries == 0.25, 0.5, ifelse(df_act$act_VisitsMuseumsGaleries == 0.75, 1, df_act$act_VisitsMuseumsGaleries))
  df_act$act_MotorizedOutdoorActivities <- ifelse(df_act$act_MotorizedOutdoorActivities == 0.25, 0.5, ifelse(df_act$act_MotorizedOutdoorActivities == 0.75, 1, df_act$act_MotorizedOutdoorActivities))
  df_act$act_Volunteering <- ifelse(df_act$act_Volunteering == 0.25, 0.5, ifelse(df_act$act_Volunteering == 0.75, 1, df_act$act_Volunteering))

  ##--------------------------------------
  ## 2- Préparation des variables de style
  ##--------------------------------------
  df_style <- df_data %>%
    select(all_of(variables_style))

  # Regroupement de app_swag
  # Les variables app_swag sont telles que certaines catégories sont très peu peuplées. Regrouper les styles pour en faire des styles plus généraux.
  df_style <- df_style %>%

    # Regrouper Formel et Chic sous Classique
    mutate(
      app_swag_Classique = if_else(
        app_swag_Formel == 1 | app_swag_Chic == 1,
        1,
        app_swag_Classique
      )
    ) %>%
    select(-app_swag_Formel, -app_swag_Chic) %>%

    # Regrouper HippBoheme, Punk, Rock sous Other
    mutate(
      app_swag_Other = if_else(
        app_swag_HippBoheme == 1 | app_swag_Punk == 1 | app_swag_Rock == 1,
        1,
        app_swag_Other
      )
    ) %>%
    select(-app_swag_HippBoheme, -app_swag_Punk, -app_swag_Rock)

  # Changement de sens de noTatoo
  # Retourner le sens logique pour que vrai soit avec tatou => deviendra Tatoo
  df_style <- df_style %>%
    mutate(app_withTattoo = if_else(app_noTattoo == 1, 0, 1)) %>%
    select(-app_noTattoo)

  # Regroupement de animal
  # Regrouper certaines classes d'animal pour avoir des catégories plus générales
  df_style <- df_style %>%
    mutate(animal_other = if_else(
      animal_domestic == 1 | animal_farm == 1,
      1,
      0
    )
    ) %>%
    select(-animal_domestic, -animal_farm)

  ##--------------------------------------
  ## 3- Préparation des variables de santé
  ##--------------------------------------
  df_sante <- df_data %>%
    select(all_of(variables_sante))

  # Transformation de cons_meat en variable ordinale
  #   - 0 Not more than once a month
  #   - 0.5 Few weekly
  #   - 1 Daily
  df_sante <- df_sante %>%

    mutate(
      cons_Meat = if_else(
        cons_meat_never == 1 | cons_meat_almost_never == 1 | cons_meat_once_month == 1,
        0,
        if_else(
          cons_meat_once_week == 1 | cons_meat_few_week == 1,
          0.5,
          1
        )
      )
    ) %>%
    select(
      -cons_meat_never,
      -cons_meat_almost_never,
      -cons_meat_once_month,
      -cons_meat_once_week,
      -cons_meat_few_week,
      -cons_meat_daily,
      -cons_meat_few_daily
    )

  # - cons_..._Drink : On peut conserver tel quel

  # Transformation de cons_Smoke en variable ordinale
  #   - 0 Never
  #   - 0.5 Not daily
  #   - 1 Daily
  df_sante <- df_sante %>%

    mutate(
      cons_Smoke = if_else(
        cons_Smoke_never == 1,
        0,
        if_else(
          cons_Smoke_few_times_year == 1 | cons_Smoke_month == 1 | cons_Smoke_once_week == 1 | cons_Smoke_few_times_week == 1,
          0.5,
          1
        )
      )
    ) %>%
    select(
      -cons_Smoke_never,
      -cons_Smoke_few_times_year,
      -cons_Smoke_month,
      -cons_Smoke_once_week,
      -cons_Smoke_few_times_week,
      -cons_Smoke_once_day,
      -cons_Smoke_few_times_day
    )

  ##--------------------------------------------
  ## 4- Préparation des variables de mode de vie
  ##--------------------------------------------
  df_mode_de_vie <- df_data %>%
    select(all_of(variables_mode_de_vie))

  # - Regroupement de ses_dwelling
  #   - ses_dwelling_Condo : ses_dwelling_Condo + ses_dwelling_Loft + ses_dwelling_Tour
  #   - ses_dwelling_Other : ses_dwelling_Other + ses_dwelling_Coop + ses_dwelling_HLM + ses_dwelling_Mobile
  df_mode_de_vie <- df_mode_de_vie %>%

    mutate(
      ses_dwelling_Condo = if_else(
        ses_dwelling_Loft == 1 | ses_dwelling_Tour == 1,
        1,
        ses_dwelling_Condo
      ),
      ses_dwelling_Other = if_else(
        ses_dwelling_Coop == 1 | ses_dwelling_HLM == 1 | ses_dwelling_Mobile == 1,
        1,
        ses_dwelling_Other
      )
    ) %>%
    select(
      -ses_dwelling_Loft,
      -ses_dwelling_Tour,
      -ses_dwelling_Coop,
      -ses_dwelling_HLM,
      -ses_dwelling_Mobile
    )

  # - Regroupement des vehicules
  #   - vehicule_ToutTerrain : vehicule_4x4	+ vehicule_PickUp
  #   - vehicle_Voiture : vehicule_Berline + vehicule_Cabriolet + vehicule_luxury + vehicule_sport
  #   - vehicule_Van
  #   - vehicule_electric
  #   - vehicule_VUS
  #   - vehicule_other
  #   - vehicule_noCar
  df_mode_de_vie <- df_mode_de_vie %>%

    mutate(
      vehicule_ToutTerrain = if_else(
        vehicule_4x4 == 1 | vehicule_PickUp == 1,
        1,
        0
      ),
      vehicule_Voiture = if_else(
        vehicule_Berline == 1 | vehicule_Cabriolet == 1 | vehicule_luxury == 1 | vehicule_sport,
        1,
        0
      )
    ) %>%
    select(
      -vehicule_4x4,
      -vehicule_PickUp,
      -vehicule_Berline,
      -vehicule_Cabriolet,
      -vehicule_luxury,
      -vehicule_sport
    )

  # - Regroupement des act_transport
  #   - act_transport_Car : act_transport_Car + act_transport_Moto (parce qu'il y avraiment trop peu de moto, assigner la classe la plus populeuse)
  #   - Les autres ne sont pas regroupés
  df_mode_de_vie <- df_mode_de_vie %>%

    mutate(
      act_transport_Car = if_else(
        act_transport_Moto == 1,
        1,
        act_transport_Car
      )) %>%
    select(
      -act_transport_Moto
    )

  ##--------------------------------------
  ## 5- Préparation des variables commerce
  ##--------------------------------------
  df_commerce <- df_data %>%
    select(all_of(variables_commerce))

  # - cons_brand : Aucun changement

  # - cons_coffee : Regrouper SC (second cup) avec Other
  df_commerce <- df_commerce %>%

    mutate(
      cons_coffee_Other = if_else(
        cons_coffee_SC == 1,
        1,
        cons_coffee_Other
      )
    ) %>%
    select(-cons_coffee_SC)

  ##---------------------------------
  ## 6- Préparation des variables ses
  ##---------------------------------
  df_ses <- df_data %>%
    select(all_of(variables_ses))

  # Transformation de age en variable ordinale
  df_ses <- df_ses %>%

    mutate(
      age = if_else(
        age34m == 1,
        0,
        if_else(
          age3554 == 1,
          0.5,
          1
        )
      )
    ) %>%
    select(
      -age34m,
      -age3554,
      -age55p
    )

  # Transformation de educ en variable ordinale
  df_ses <- df_ses %>%

    mutate(
      educ = if_else(
        educBHS == 1,
        0,
        if_else(
          educCollege == 1,
          0.5,
          1
        )
      )
    ) %>%
    select(
      -educBHS,
      -educCollege,
      -educUniv
    )

  # Transformation de income en variable ordinale
  df_ses <- df_ses %>%
    mutate(
      ses_income_None = 0,
      ses_income_i1to30    = ses_income_i1to30/7,
      ses_income_i31to60   = (1*ses_income_i31to60)/7 + ses_income_i31to60/7,
      ses_income_i61to90   = (2*ses_income_i61to90)/7 + ses_income_i61to90/7,
      ses_income_i91to110  = (3*ses_income_i91to110)/7 + ses_income_i91to110/7,
      ses_income_i111to150 = (4*ses_income_i111to150)/7 + ses_income_i111to150/7,
      ses_income_i151to200 = (5*ses_income_i151to200)/7 + ses_income_i151to200/7,
      ses_income_i201toInf = (ses_income_i201toInf)
    ) %>%
    mutate (
      ses_income = ses_income_None +
        ses_income_i31to60 +
        ses_income_i61to90 +
        ses_income_i91to110 +
        ses_income_i111to150 +
        ses_income_i151to200 +
        ses_income_i201toInf
    ) %>%
    select(
      -ses_income_None,
      -ses_income_i201toInf,
      -ses_income_i31to60,
      -ses_income_i61to90,
      -ses_income_i91to110,
      -ses_income_i111to150,
      -ses_income_i151to200,
      -ses_income_i201toInf
    )

  #---------------------------------------------------
  # Assembler les données préparées pour le clustering
  #---------------------------------------------------
  df_data_clust <- df_act %>%
    left_join(df_style, by = "id") %>%
    left_join(df_sante, by = "id") %>%
    left_join(df_mode_de_vie, by = "id") %>%
    left_join(df_commerce, by = "id") %>%
    left_join(df_ses, by = "id")

  return(df_data_clust)
}

# Charger les données
df_pilot1_2022 <- readRDS("_PrivateFolder_datagotchi_federal_2025/data/clustering/qc2022/01_pilot1_2022.rds")
df_pilot1_2022_clust <- fct_02_preparation_donnees(df_pilot1_2022)
saveRDS(df_pilot1_2022_clust, file = "_PrivateFolder_datagotchi_federal_2025/data/clustering/qc2022/02_pilot1_2022.rds")

df_app_2022 <- readRDS("_PrivateFolder_datagotchi_federal_2025/data/clustering/qc2022/01_app_2022.rds")
df_app_2022_clust <- fct_02_preparation_donnees(df_app_2022)
saveRDS(df_app_2022_clust, file = "_PrivateFolder_datagotchi_federal_2025/data/clustering/qc2022/02_app_2022.rds")

