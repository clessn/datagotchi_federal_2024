
## Doc roxygen


# Préparation des simulations ou prédictions directes
prepare_simulations <- function(
  census_origin, census_target,
  spatial_intersection,
  origin_col, target_col,
  n_sim = 500,
  origins = NULL
) {
  message("Simuler ", n_sim, " répondants par `", origin_col, "`...")
  rta_cases <- spatial_intersection |>
    group_by(.data[[origin_col]]) |>
    summarise(case = n(), .groups = "drop") |>
    mutate(case = ifelse(case > 1, 2, 1)) |>
    tibble::deframe()

  if (!is.null(origins)) {
    rta_cases <- rta_cases[names(rta_cases) %in% origins]
  }

  unique_predictions <- list()
  simulated_datasets <- list()

  total <- length(rta_cases)

  for (i in seq_along(rta_cases)) {
    origin_id <- names(rta_cases)[i]
    case_i <- rta_cases[[i]]


    if (case_i == 1) {
      target_id <- spatial_intersection |>
        filter(.data[[origin_col]] == origin_id) |>
        pull(.data[[target_col]]) |>
        unique()
      unique_predictions[[origin_id]] <- setNames(1, target_id)
    } else {
      simulated_data <- suppressMessages(
        cartessn::simulate_respondents_from_census_unit(
          origin_unit_id = origin_id,
          n = n_sim,
          spatial_intersection = spatial_intersection,
          origin_id_col = origin_col,
          target_id_col = target_col,
          census_origin = census_origin,
          census_target = census_target
        )
      )
      simulated_datasets[[origin_id]] <- simulated_data
    }
    pct <- floor(i / total * 100)
    cat(sprintf("\r         Progression: %3d%% — %s", pct, origin_id))
    flush.console()
  }

  cat("\n")

  list(
    cases = rta_cases,
    unique_predictions = unique_predictions,
    simulated_datasets = simulated_datasets
  )
}



predict_spatial_target <- function(
  survey_data,
  origin_col = "rta",
  target_col = "id_riding",
  ses_vars,
  census_origin,
  census_target,
  spatial_origin,
  spatial_target,
  return_type = c("probabilities", "class"),
  n_sim = 500
) {

  message("Validation des arguments de la fonction...")
  return_type <- match.arg(return_type)

  # Vérifie que survey_data contient seulement origin_col + ses_vars
  expected_vars <- c(origin_col, ses_vars)
  extra_vars <- setdiff(names(survey_data), expected_vars)
  if (length(extra_vars) > 0) {
    stop(
      "`survey_data` ne devrait contenir que la colonne d'origine (`", origin_col, "`) et les variables SES dans `ses_vars`.\n",
      "Variables supplémentaires détectées: ", paste(extra_vars, collapse = ", ")
    )
  }

  # Vérifie que toutes les ses_vars sont présentes dans survey_data
  missing_ses <- setdiff(ses_vars, names(survey_data))
  if (length(missing_ses) > 0) {
    stop(
      "Certaines variables dans `ses_vars` ne sont pas présentes dans `survey_data` : ",
      paste(missing_ses, collapse = ", ")
    )
  }

  # Vérifie que les census sont dans un format long avec les colonnes attendues
  required_cols <- c("variable", "category", "count", "prop")

  check_census_format <- function(census_df, name) {
    missing_cols <- setdiff(required_cols, names(census_df))
    if (length(missing_cols) > 0) {
      stop(
        "Le jeu de données `", name, "` n'est pas dans le bon format.\n",
        "Colonnes manquantes : ", paste(missing_cols, collapse = ", "), "\n",
        "Référez-vous au format de `cartessn::census_canada_2022_electoral_ridings` pour suivre le format attendu par cette fonction."
      )
    }
  }

  check_census_format(census_origin, "census_origin")
  check_census_format(census_target, "census_target")

  # Vérifie que toutes les SES sont présentes dans la colonne `variable` des deux census
  missing_origin <- setdiff(ses_vars, unique(census_origin$variable))
  missing_target <- setdiff(ses_vars, unique(census_target$variable))

  if (length(missing_origin) > 0 || length(missing_target) > 0) {
    stop(
      "Certaines variables SES dans `ses_vars` sont absentes dans la colonne `variable` des données de recensement :\n",
      if (length(missing_origin) > 0)
        paste0("- Manquantes dans `census_origin`: ", paste(missing_origin, collapse = ", "), "\n"),
      if (length(missing_target) > 0)
        paste0("- Manquantes dans `census_target`: ", paste(missing_target, collapse = ", "))
    )
  }

  # Vérifie que toutes les catégories observées dans survey_data sont présentes dans les recensements
  missing_entries <- list()

  for (var in ses_vars) {
    survey_categories <- unique(survey_data[[var]]) |> na.omit()

    origin_categories <- census_origin |>
      filter(variable == var) |>
      pull(category) |>
      unique()

    target_categories <- census_target |>
      filter(variable == var) |>
      pull(category) |>
      unique()

    # Enregistrer les manquants
    missing_in_origin <- setdiff(survey_categories, origin_categories)
    missing_in_target <- setdiff(survey_categories, target_categories)

    if (length(missing_in_origin) > 0) {
      missing_entries <- c(
        missing_entries,
        lapply(missing_in_origin, \(cat) c("census_origin", var, cat))
      )
    }

    if (length(missing_in_target) > 0) {
      missing_entries <- c(
        missing_entries,
        lapply(missing_in_target, \(cat) c("census_target", var, cat))
      )
    }
  }

  if (length(missing_entries) > 0) {
    missing_msg <- vapply(missing_entries, paste, character(1), collapse = " → ")
    stop(
      "Certaines catégories observées dans `survey_data` sont absentes du recensement :\n",
      paste(missing_msg, collapse = "\n")
    )
  }

  
  message("Calculer les intersections géographiques entre `", origin_col, "` et `", target_col, "`...")
  spatial_intersection <- suppressWarnings(
    cartessn::intersect_spatial_objects(
      spatial_ref = spatial_origin,
      id_ref = origin_col,
      spatial_target = spatial_target,
      id_target = target_col
    )
  )

  sim_result <- prepare_simulations(
      census_origin = census_origin,
      census_target = census_target,
      spatial_intersection = spatial_intersection,
      origin_col = origin_col,
      target_col = target_col,
      n_sim = n_sim,
      origins = unique(survey_data[[origin_col]])
    )

  cases <- sim_result$cases
  unique_predictions <- sim_result$unique_predictions
  simulated_datasets <- sim_result$simulated_datasets

  list_predictions <- vector("list", nrow(survey_data))

  message("Entrainement et prédiction des modèles sur chaque rangée de `survey_data`...")
  total <- nrow(survey_data)
  for (i in seq_len(total)) {
    origin_id <- survey_data[[origin_col]][i]

    case_i <- cases[[origin_id]]

    if (case_i == 1) {
      preds <- unique_predictions[[origin_id]]
    } else {
      non_empty_vars <- survey_data[i, ses_vars] |>
        select(where(~ !is.na(.))) |>
        names()

      model <- suppressMessages(
        nnet::multinom(
          formula = paste0(target_col, " ~ ", paste0(non_empty_vars, collapse = " + ")),
          data = simulated_datasets[[origin_id]],
          trace = FALSE
        )
      )

      preds <- predict(model, newdata = survey_data[i, ], type = "prob")

      if (length(model$lev) == 2){
        preds <- setNames(
          c(`[[1]]` = 1 - preds, `[[2]]` = preds),
          model$lev
        )
      }
    }

    list_predictions[[i]] <- round(preds, 2)

    pct <- floor(i / total * 100)
    cat(sprintf("\r         Progression: %3d%%", pct))
    flush.console()
  }

  df_predictions <- bind_rows(list_predictions) |>
    mutate(across(everything(), ~ tidyr::replace_na(.x, 0)))

  if (return_type == "class") {
    return(df_predictions %>% 
      mutate(.prediction = names(.)[max.col(.)]) |>
      pull(.prediction))
  }

  return(df_predictions)
}

