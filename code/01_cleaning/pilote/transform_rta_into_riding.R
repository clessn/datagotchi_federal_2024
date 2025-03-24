df_survey_for_riding <- DataClean |>
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

DataClean$ses_riding_id <- cartessn::predict_spatial_target(
  survey_data = df_survey_for_riding,
  ses = c("ses_gender", "ses_age", "ses_income"),
  origin = "rta",
  target = "id_riding",
  spatial_origin = cartessn::spatial_canada_2021_rta,
  spatial_target = cartessn::spatial_canada_2022_electoral_ridings,
  census_origin = cartessn::census_canada_2022_rta,
  census_target = cartessn::census_canada_2022_electoral_ridings,
  return = "class",
  n_sim = 400
)

df_ridings_names <- cartessn::names_canada_2022_electoral_ridings |> 
  select(id_riding, ses_name_riding_fr = name_riding_fr, ses_name_riding_en = name_riding_en)

DataClean <- left_join(DataClean, df_ridings_names, by = c("ses_riding_id" = "id_riding"))
