fit_unit_level_models <- function(
  origin_units,
  spatial_intersection,
  origin_id_col,
  target_id_col,
  census_origin,
  census_target,
  ses,
  n_sim = 500
) {
  message("Simulation et modélisation par unité d'origine...")

  models <- list()
  unique_assignments <- list()

  for (unit in origin_units) {
    targets <- spatial_intersection %>%
      filter(.data[[origin_id_col]] == unit) %>%
      pull(.data[[target_id_col]]) %>%
      unique()

    if (length(targets) == 0) {
      warning("Aucune cible trouvée pour l’unité : ", unit)
      next
    }

    if (length(targets) == 1) {
      unique_assignments[[unit]] <- as.character(targets[1])
      next
    }

    sim_data <- tryCatch({
      cartessn::simulate_respondents_from_census_unit(
        origin_unit_id = unit,
        n = n_sim,
        spatial_intersection = spatial_intersection,
        origin_id_col = origin_id_col,
        target_id_col = target_id_col,
        census_origin = census_origin,
        census_target = census_target
      )
    }, error = function(e) {
      warning("Échec de simulation pour ", unit, " : ", conditionMessage(e))
      return(NULL)
    })

    if (is.null(sim_data) || nrow(sim_data) == 0 || all(is.na(sim_data[[target_id_col]]))) {
      warning("Aucune donnée simulée pour ", unit)
      next
    }

    sim_data[[target_id_col]] <- as.factor(as.character(sim_data[[target_id_col]]))

    classes <- levels(sim_data[[target_id_col]])
    if (length(classes) <= 1) {
      warning("Impossible d’entraîner un modèle multinomial pour ", unit, " (1 seule classe)")
      unique_assignments[[unit]] <- classes[1]
      next
    }

    model <- tryCatch({
      formula <- as.formula(paste(target_id_col, "~", paste(ses, collapse = " + ")))
      nnet::multinom(formula, data = sim_data, trace = FALSE)
    }, error = function(e) {
      warning("Échec de l'entraînement pour ", unit, " : ", conditionMessage(e))
      return(NULL)
    })

    if (!is.null(model)) {
      models[[unit]] <- model
    }
  }

  list(models = models, unique_assignments = unique_assignments)
}



predict_with_unit_models <- function(
  survey_data,
  origin_id_col,
  target_id_col,
  ses,
  model_list,
  return = c("probabilities", "class")
) {
  return <- match.arg(return)
  models <- model_list$models
  uniques <- model_list$unique_assignments

  message("Prédiction individuelle...")

  # 🔒 Inclure toutes les cibles vues dans les modèles et affectations
  all_targets <- sort(unique(c(
    unlist(uniques),
    unlist(lapply(models, function(m) {
      if (!is.null(m)) colnames(predict(m, type = "probs")) else NULL
    }))
  )))

  pred_list <- vector("list", nrow(survey_data))

  for (i in seq_len(nrow(survey_data))) {
    row <- survey_data[i, ]
    unit <- row[[origin_id_col]]

    if (anyNA(row[, ses, drop = FALSE])) {
      if (return == "class") {
        pred <- NA
      } else {
        pred <- matrix(NA, nrow = 1, ncol = length(all_targets))
        colnames(pred) <- all_targets
      }

    } else if (unit %in% names(uniques)) {
      rid <- as.character(uniques[[unit]])

      if (return == "class") {
        pred <- rid
      } else {
        p <- setNames(rep(0, length(all_targets)), all_targets)
        if (rid %in% names(p)) {
          p[[rid]] <- 1
        } else {
          p <- c(p, setNames(1, rid))
        }
        pred <- matrix(p, nrow = 1)
        colnames(pred) <- all_targets
      }

    } else if (unit %in% names(models)) {
      model <- models[[unit]]
      newdata <- row[, ses, drop = FALSE]

      if (return == "class") {
        pred <- as.character(predict(model, newdata = newdata, type = "class"))
      } else {
        prob <- predict(model, newdata = newdata, type = "probs")

        if (is.vector(prob)) {
          prob <- matrix(prob, nrow = 1)
          colnames(prob) <- colnames(predict(model, type = "probs"))
        }

        p <- setNames(rep(0, length(all_targets)), all_targets)
        p[colnames(prob)] <- prob[1, ]
        pred <- matrix(p, nrow = 1)
        colnames(pred) <- all_targets
      }

    } else {
      stop("Aucune prédiction possible pour l'unité : ", unit)
    }

    pred_list[[i]] <- pred
  }

  if (return == "class") {
    return(vapply(pred_list, function(x) {
      if (is.null(x) || is.na(x)) NA_character_ else as.character(x)
    }, character(1)))
  } else {
    return(do.call(rbind, pred_list))
  }
}






#' Prédire une unité spatiale cible à partir de données sociodémographiques et spatiales
#'
#' @inheritParams simulate_respondents_from_census_unit
#' @param survey_data Données du sondage à prédire (doit contenir `origin` + variables SES).
#' @param ses Variables sociodémographiques à utiliser pour l'entraînement.
#' @param origin Nom de la variable spatiale d'origine (ex: "rta").
#' @param target Nom de la variable spatiale cible (ex: "id_riding").
#' @param return `"class"` ou `"probabilities"`.
#' @param n_sim Nombre de répondants à simuler par unité d'origine (défaut: 500).
#'
#' @return Un vecteur (`class`) ou une matrice (`probabilities`).
#' @export
predict_spatial_target <- function(
  survey_data,
  ses,
  origin,
  target,
  spatial_origin,
  spatial_target,
  census_origin,
  census_target,
  return = c("probabilities", "class"),
  n_sim = 500
) {
  return <- match.arg(return)

  # --- Vérifications de base ---
  if (!origin %in% names(survey_data)) stop("Colonne d'origine absente de survey_data.")
  if (!all(ses %in% names(survey_data))) stop("Certaines SES manquent dans survey_data.")
  if (!all(ses %in% census_origin$variable)) stop("Certaines SES manquent dans census_origin.")
  if (!all(ses %in% census_target$variable)) stop("Certaines SES manquent dans census_target.")

  if (length(unique(spatial_origin[[origin]])) != length(unique(census_origin[[origin]]))) {
    warning("Nombre d'unités dans census_origin ≠ spatial_origin.")
  }

  if (length(unique(spatial_target[[target]])) != length(unique(census_target[[target]]))) {
    warning("Nombre d'unités dans census_target ≠ spatial_target.")
  }

  # --- Étape 1 : Calcul des intersections spatiales ---
  message("Calcul des intersections spatiales...")
  spatial_intersection <- cartessn::intersect_spatial_objects(
    spatial_ref = spatial_origin,
    id_ref = origin,
    spatial_target = spatial_target,
    id_target = target
  )

  # --- Étape 2 : Modélisation individuelle par unité d'origine ---
  model_list <- fit_unit_level_models(
    origin_units = unique(survey_data[[origin]]),
    spatial_intersection = spatial_intersection,
    origin_id_col = origin,
    target_id_col = target,
    census_origin = census_origin,
    census_target = census_target,
    ses = ses,
    n_sim = n_sim
  )

  # --- Étape 3 : Prédictions ---
  preds <- predict_with_unit_models(
    survey_data = survey_data,
    origin_id_col = origin,
    target_id_col = target,
    ses = ses,
    model_list = model_list,
    return = return
  )

  return(preds)
}
