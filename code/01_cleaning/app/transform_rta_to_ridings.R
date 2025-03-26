
print("ATTENTION: étape plus longue pour ajouter les circonscriptions. Prévoir 10 minutes environ")
# Vérification des données d'entrée pour les NA
print("Vérification des données d'entrée pour valeurs manquantes...")
df_survey_for_riding <- DataClean |>
  select(
    rta = ses_postalCode,
    ses_gender = ses_genderMale,
    ses_age = ses_ageGroup5Years,
    ses_income = ses_incomeCensus
  )
# Afficher le nombre de NA par colonne
na_count <- sapply(df_survey_for_riding, function(y) sum(is.na(y)))
print(na_count)
# Filtrer les valeurs NA avant de procéder
df_survey_for_riding <- df_survey_for_riding |>
  filter(!is.na(rta), !is.na(ses_gender), !is.na(ses_age), !is.na(ses_income)) |>
  mutate(
    rta = toupper(rta),
    ses_gender = ifelse(ses_gender == 1, "male", "female")
  )
print(paste("Nombre de lignes après filtrage des NA:", nrow(df_survey_for_riding)))

# Vérifier les RTA valides
spatial_rta <- unique(cartessn::spatial_canada_2021_rta$rta)
df_survey_for_riding <- df_survey_for_riding |>
  filter(rta %in% spatial_rta)
print(paste("Nombre de lignes après filtrage des RTA invalides:", nrow(df_survey_for_riding)))

# Prédiction de la circonscription électorale
DataClean$ses_riding_id <- NA # Initialiser avec NA
respondents_with_data <- DataClean$id %in% row.names(df_survey_for_riding)
if(sum(respondents_with_data) > 0) {
  riding_predictions <- cartessn::predict_spatial_target(
    survey_data = df_survey_for_riding,
    origin_col = "rta",
    target_col = "id_riding",
    ses_vars = c("ses_gender", "ses_age", "ses_income"),
    census_origin = cartessn::census_canada_2022_rta,
    census_target = cartessn::census_canada_2022_electoral_ridings,
    spatial_origin = cartessn::spatial_canada_2021_rta,
    spatial_target = cartessn::spatial_canada_2022_electoral_ridings,
    return_type = "class",
    n_sim = 400
  )
  # Assigner les prédictions aux lignes correspondantes
  DataClean$ses_riding_id[respondents_with_data] <- riding_predictions
}
# Ajout des noms des circonscriptions électorales
df_ridings_names <- cartessn::names_canada_2022_electoral_ridings |>
  select(id_riding, ses_name_riding_fr = name_riding_fr, ses_name_riding_en = name_riding_en)
# Jointure avec DataClean pour ajouter les noms des circonscriptions
DataClean <- left_join(DataClean, df_ridings_names, by = c("ses_riding_id" = "id_riding"))
print("L'étape la plus longue est terminée, bravo pour votre patience. - Codeur PRX")