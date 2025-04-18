# Packages ---------------------------------------------------------------
library(dplyr)
library(ggplot2)

# Data -------------------------------------------------------------------
df_raw <- haven::read_sav("_SharedFolder_datagotchi_federal_2024/data/panel/panel_questionnaire_April+17,+2025_22.56.sav")

codebook <- sondr::sav_to_codebook(df_raw)

df_clean <- data.frame(
  id = 1:nrow(df_raw)
)

# SES ------------------------------------------------------------------

## Date -------------------------------------------------------------------

df_clean$date <- as.Date(df_raw$RecordedDate)

df_clean$date_time <- df_raw$RecordedDate

df_clean |> 
  group_by(date) |> 
  summarise(n = n())

## Gender -----------------------------------------------------------------

attributes(df_raw$ses_sex)
table(df_raw$ses_sex)
df_clean$ses_gender_woman <- NA
df_clean$ses_gender_woman[df_raw$ses_sex == 1] <- 1
df_clean$ses_gender_woman[df_raw$ses_sex %in% c(2, 3, 4, 5, 8)] <- 0
table(df_clean$ses_gender_woman)

## Age --------------------------------------------------------------------

attributes(df_raw$ses_age)
df_clean$ses_age <- cut(
  df_raw$ses_age,
  breaks = c(18, 20, seq(25, 100, by = 5), Inf),
  labels = c(
    "18_19", "20_24", "25_29", "30_34", "35_39",
    "40_44", "45_49", "50_54", "55_59", "60_64",
    "65_69", "70_74", "75_79", "80_84", "85_89",
    "90_94", "95_99", "100+"
  ),
  right = FALSE,
  include.lowest = TRUE
)
table(df_clean$ses_age)

## Language ---------------------------------------------------------------

attributes(df_raw$ses_language)
table(df_raw$ses_language)
df_clean$ses_lang_french <- NA
df_clean$ses_lang_french[df_raw$ses_language == 2] <- 1
df_clean$ses_lang_french[df_raw$ses_language %in% c(1, 3)] <- 0
table(df_clean$ses_lang_french)

## Education --------------------------------------------------------------

attributes(df_raw$ses_educ)
table(df_raw$ses_educ)
df_clean$ses_educ <- NA
df_clean$ses_educ[df_raw$ses_educ %in% c(1, 4, 5)] <- "high_school"
df_clean$ses_educ[df_raw$ses_educ %in% c(6)] <- "college"
df_clean$ses_educ[df_raw$ses_educ %in% c(7, 8, 9)] <- "univ"
table(df_clean$ses_educ)

# Dependent variables ----------------------------------------------------

## RCI --------------------------------------------------------------------

rci_curve <- readRDS("_SharedFolder_datagotchi_federal_2024/data/panel/rci_curve.rds")

df_rci <- df_raw |> 
  select(starts_with("dv_potgrowth")) |> 
  mutate(across(everything(), ~ .x - 1)) |> 
  rename(
    dv_potgrowth_lpc = dv_potgrowth_1,
    dv_potgrowth_cpc = dv_potgrowth_2,
    dv_potgrowth_ndp = dv_potgrowth_3,
    dv_potgrowth_bq = dv_potgrowth_4,
    dv_potgrowth_gpc = dv_potgrowth_5,
    dv_potgrowth_ppc = dv_potgrowth_6,
  ) %>% 
  mutate(id = 1:nrow(.)) %>%
  potgrowth::compute_rci(., prefix = "dv_potgrowth_", id_col = "id") |>
  tidyr::pivot_longer(
    cols = starts_with("rci_"),
    names_prefix = "rci_",
    names_to = "party",
    values_to = "rci"
  ) |> 
  left_join(rci_curve, by = "rci") |> 
  tidyr::pivot_wider(
      id_cols = id,
      names_from = party,
      values_from = vote_prob,
      names_prefix = "voteprob_"
    ) |> 
  ungroup() |> 
  select(-id)

df_output <- cbind(df_clean, df_rci) |> 
  select(-voteprob_gpc, -voteprob_ppc)

## Competence -------------------------------------------------------------

attributes(df_raw$Competence_1)
df_output$competence_lpc <- sondr::clean_likert_numeric_vector(df_raw$Competence_1)

attributes(df_raw$Competence_2)
df_output$competence_cpc <- sondr::clean_likert_numeric_vector(df_raw$Competence_2)

attributes(df_raw$Competence_3)
df_output$competence_ndp <- sondr::clean_likert_numeric_vector(df_raw$Competence_3)

attributes(df_raw$Competence_4)
df_output$competence_bq <- sondr::clean_likert_numeric_vector(df_raw$Competence_4)
hist(df_output$competence_bq)

## People pred ------------------------------------------------------------

attributes(df_raw$people_pred)
table(df_raw$people_pred)

df_output$people_pred_lpc <- sondr::clean_likert_numeric_vector(df_raw$people_pred, revert = TRUE)
table(df_output$people_pred_lpc)

df_output$people_pred_cpc <- sondr::clean_likert_numeric_vector(df_raw$people_pred, revert = FALSE)
table(df_output$people_pred_cpc)

## Province (filter out ROC respondents) ---------------------------------------------------------------

attributes(df_raw$ses_province)
table(df_raw$ses_province)
df_final <- df_output[which(df_raw$ses_province == 11),]

saveRDS(df_final, "_SharedFolder_datagotchi_federal_2024/data/panel/01_panelclean_effet_debat.rds")


