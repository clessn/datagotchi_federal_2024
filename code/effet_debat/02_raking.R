# Packages ---------------------------------------------------------------
library(cancensus)
library(dplyr)
library(survey)
library(slider)

# Data -------------------------------------------------------------------

data <- readRDS("_SharedFolder_datagotchi_federal_2024/data/panel/01_panelclean_effet_debat.rds") |> 
  tidyr::drop_na(
    starts_with(c("voteprob", "competence", "people_pred"))
  )

# Get margins ------------------------------------------------------------

gender_vector <- cancensus::get_census(
  dataset = "CA21",
  regions = list(PR = "24"),
  vectors = c(
    "0" = "v_CA21_9",
    "1" = "v_CA21_10"
  )
) |> 
  select(`0`, `1`) |> 
  unlist() |>
  {\(x) x / sum(x)}()

age_vector <- cancensus::get_census(
  dataset = "CA21",
  regions = list(PR = "24"),
  vectors = c(
    "18_19" = "v_CA21_71",
    "20_24" = "v_CA21_89",
    "25_29" = "v_CA21_107",
    "30_34" = "v_CA21_125",
    "35_39" = "v_CA21_143",
    "40_44" = "v_CA21_161",
    "45_49" = "v_CA21_179",
    "50_54" = "v_CA21_197",
    "55_59" = "v_CA21_215",
    "60_64" = "v_CA21_233",
    "65_69" = "v_CA21_254",
    "70_74" = "v_CA21_272",
    "75_79" = "v_CA21_290",
    "80_84" = "v_CA21_308",
    "85_89" = "v_CA21_329",
    "90_94" = "v_CA21_347",
    "95_99" = "v_CA21_365",
    "100+" = "v_CA21_383"
  )
) |> 
  select(`18_19`, `20_24`, `25_29`, `30_34`, `35_39`, `40_44`, `45_49`, `50_54`, `55_59`, `60_64`, `65_69`, `70_74`, `75_79`, `80_84`, `85_89`, `90_94`, `95_99`, `100+`) |> 
  unlist() |>
  {\(x) x / sum(x)}()

lang_vector <- cancensus::get_census(
  dataset = "CA21",
  regions = list(PR = "24"),
  vectors = c(
    "0a" = "v_CA21_1162",
    "0b" = "v_CA21_1171",
    "1a" = "v_CA21_1165",
    "1b" = "v_CA21_1168"
  )
) |> 
  transmute(
  `0` = `0a` + `0b`,
  `1` = `1a` + `1b`
  ) |> 
  select(`0`, `1`) |> 
  unlist() |> 
  {\(x) x / sum(x)}()


educ_vector <- cancensus::get_census(
  dataset = "CA21",
  regions = list(PR = "24"),
  vectors = c(
    "high_school1" = "v_CA21_5820",
    "high_school2" = "v_CA21_5823",
    "college" = "v_CA21_5829",
    "univ" = "v_CA21_5847"
  )
) |> 
mutate(
  high_school = high_school1 + high_school2
  ) |> 
  select(high_school, college, univ) |>
  unlist() |>
  {\(x) x / sum(x)}()


# Raking by day ----------------------------------------------------------

gender_margin <- tibble(
  ses_gender_woman = names(gender_vector),
  Freq = as.numeric(gender_vector)
)
age_margin <- tibble(
  ses_age = names(age_vector),
  Freq = as.numeric(age_vector)
)
lang_margin <- tibble(
  ses_lang_french = names(lang_vector),
  Freq = as.numeric(lang_vector)
)
educ_margin <- tibble(
  ses_educ = names(educ_vector),
  Freq = as.numeric(educ_vector)
)

# Recode les dates selon heure
data <- data |>
  mutate(
    adjusted_date = case_when(
      date == as.Date("2025-04-16") & lubridate::hour(date_time) < 18 ~ as.Date("2025-04-15"),
      date == as.Date("2025-04-16") & lubridate::hour(date_time) >= 18 ~ as.Date("2025-04-17"),
      TRUE ~ date
    )
  )

dates_unique <- sort(unique(data$adjusted_date))
data_raked <- list()

for (i in seq_along(dates_unique)) {

  current_date <- dates_unique[i]

  # Cas du 15 avril (pré-débat, incluant les 16 avril < 18h)
  if (current_date < as.Date("2025-04-17")) {
    
    window_dates <- rev(dates_unique[1:i]) |> head(3)

    df_window <- data |>
      filter(
        adjusted_date %in% window_dates,
        # Filtrer le 16 avril pour ne garder que les obs < 18h
        !(date == as.Date("2025-04-16") & lubridate::hour(date_time) >= 18)
      ) |>
      mutate(weight_temp = case_when(
        adjusted_date == window_dates[1] ~ 0.60,
        length(window_dates) >= 2 & adjusted_date == window_dates[2] ~ 0.25,
        length(window_dates) >= 3 & adjusted_date == window_dates[3] ~ 0.15,
        TRUE ~ 0
      ))

  # Cas à partir du 17 avril (post-débat, incluant les 16 avril ≥ 18h)
  } else if (current_date >= as.Date("2025-04-17")) {

    window_dates <- rev(dates_unique[1:i]) |> head(3)

    df_window <- data |>
      filter(adjusted_date %in% window_dates) |>
      mutate(weight_temp = case_when(
        adjusted_date == window_dates[1] ~ 0.75,
        length(window_dates) >= 2 & adjusted_date == window_dates[2] ~ 0.2,
        length(window_dates) >= 3 & adjusted_date == window_dates[3] ~ 0.05,
        TRUE ~ 0
      ))
  
  # Toutes les autres dates (ex. 16 avril brut), on les ignore : déjà reclassées
  } else {
    next
  }

  # Nettoyage et vérifications
  df_window <- df_window |>
    filter(
      weight_temp > 0,
      !is.na(ses_gender_woman),
      !is.na(ses_age),
      !is.na(ses_lang_french),
      !is.na(ses_educ)
    )

  if (nrow(df_window) < 20) {
    message("Skip ", current_date, ": insufficient data")
    next
  }

  des <- svydesign(ids = ~1, data = df_window, weights = ~weight_temp)

  gm <- gender_margin |> filter(ses_gender_woman %in% df_window$ses_gender_woman)
  am <- age_margin    |> filter(ses_age %in% df_window$ses_age)
  lm <- lang_margin   |> filter(ses_lang_french %in% df_window$ses_lang_french)
  em <- educ_margin   |> filter(ses_educ %in% df_window$ses_educ)

  raked <- tryCatch({
    rake(
      design = des,
      sample.margins = list(
        ~ses_gender_woman,
        ~ses_age,
        ~ses_lang_french,
        ~ses_educ
      ),
      population.margins = list(
        gm,
        am,
        lm,
        em
      ),
      control = list(maxit = 100, epsilon = 1e-6)
    )
  }, error = function(e) {
    message("Raking failed for ", current_date, ": ", conditionMessage(e))
    return(NULL)
  })

  if (is.null(raked)) next

  df_result <- df_window |>
    mutate(
      date_target = current_date,
      weight_raked = weights(raked),
      weight_raked = weight_raked / sum(weight_raked)
    )

  data_raked[[as.character(current_date)]] <- df_result
}

# Final bind
data_raked <- bind_rows(data_raked)

saveRDS(data_raked, "_SharedFolder_datagotchi_federal_2024/data/panel/02_panelraked_effet_debat.rds")

