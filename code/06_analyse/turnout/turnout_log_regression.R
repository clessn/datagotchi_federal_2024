# ───── 1. Charger les librairies ─────
library(tidyverse)
library(survey)

# ───── 2. Charger les données (.rds) ─────
datagotchi <- readRDS("_SharedFolder_datagotchi_federal_2024/data/app/dataClean/datagotchi2025_canada_appPonderees_20250330.rds")

# ───── 3. Définir la variable cible ─────
target_var <- "dv_turnout_bin"

# ───── 4. Identifier les colonnes SES ─────
cols_ses <- names(datagotchi)[startsWith(names(datagotchi), "ses")]

# ───── 5. Identifier les colonnes à standardiser ─────
cols_to_scale <- datagotchi %>%
  select(-id, -weight, -all_of(c(cols_ses, target_var))) %>%
  select(where(is.numeric)) %>%
  names()

# ───── 6. Standardiser les colonnes choisies (pondération mathématique) ─────
datagotchi_scaled <- datagotchi %>%
  mutate(across(all_of(cols_to_scale), ~scale(.)[,1]))

# ───── 7. Créer le jeu de données du modèle (avec poids) ─────
df_model <- datagotchi_scaled %>%
  select(all_of(c(cols_to_scale, cols_ses, target_var, "weight"))) %>%
  drop_na()

# ───── 8. Enlever les colonnes sans variance (au moins 2 valeurs différentes) ─────
poids <- df_model$weight

df_model_filtered <- df_model %>%
  select(-weight) %>%
  select(where(~ n_distinct(.) > 1) | all_of(target_var)) %>%
  mutate(weight = poids)

# ───── 9. Construire le design pondéré ─────
design <- svydesign(ids = ~1, weights = ~weight, data = df_model_filtered)

# 10. Créer la formule complète
formula <- as.formula(paste(target_var, "~ ."))

# 11. Extraire uniquement les variables disponibles dans le jeu de données filtré
vars_dispo <- intersect(all.vars(formula), names(df_model_filtered))

# 12. Créer la formule corrigée (sans les variables manquantes)
formula_corrigee <- as.formula(
  paste(target_var, "~", paste(setdiff(vars_dispo, target_var), collapse = " + "))
)

# 13. Refaire le design pondéré (si modifié entre-temps)
design <- svydesign(ids = ~1, weights = ~weight, data = df_model_filtered)

# 14. Régression logistique pondérée
modele <- svyglm(formula_corrigee, design = design, family = quasibinomial())

# 15. Résumé du modèle
summary(modele)

setdiff(all.vars(formula), names(df_model_filtered))
#######################################################################
# ───── 1. Charger les librairies ─────
library(tidyverse)
library(survey)

# ───── 2. Charger les données ─────
datagotchi <- readRDS("_SharedFolder_datagotchi_federal_2024/data/app/dataClean/datagotchi2025_canada_appPonderees_20250330.rds")

# ───── 3. Définir la variable cible ─────
target_var <- "dv_turnout_bin"

# ───── 4. Identifier les variables à exclure de la pondération ─────
vars_exclure <- names(datagotchi)[
  startsWith(names(datagotchi), "ses") |
    startsWith(names(datagotchi), "tactical_") |
    names(datagotchi) %in% c(target_var, "weight", "id")
]

# ───── 5. Sélectionner les variables à pondérer ─────
vars_a_ponderer <- setdiff(names(datagotchi), vars_exclure)

# ───── 6. Appliquer la pondération aux variables choisies ─────
df_ponderes <- datagotchi %>%
  mutate(across(all_of(vars_a_ponderer), ~ . * weight))

# ───── 7. Recolle les colonnes non pondérées (ses_, target et poids) ─────
df_final <- bind_cols(
  df_ponderes %>% select(all_of(vars_a_ponderer)),
  datagotchi %>% select(starts_with("ses"), all_of(target_var), weight)
) %>%
  drop_na()

# ───── 8. Créer la formule automatiquement ─────
vars_explicatives <- setdiff(names(df_final), c(target_var, "weight"))
formula_logit <- as.formula(
  paste(target_var, "~", paste(vars_explicatives, collapse = " + "))
)

# ───── 9. Créer le design pondéré ─────
design <- svydesign(ids = ~1, weights = ~weight, data = df_final)

# ───── 10. Régression logistique pondérée ─────
modele <- svyglm(formula_logit, design = design, family = quasibinomial())

# ───── 11. Résumé du modèle ─────
summary(modele)
############################################################################################

# 1. Extraire les noms des variables explicatives binaires utilisées dans la régression
vars_binaires <- attr(terms(modele), "term.labels")

# 2. Créer un dataframe avec les prédictions du modèle
df_plot <- df_final %>%
  select(all_of(c(vars_binaires, target_var))) %>%
  mutate(prob_predite = predict(modele, type = "response"))

# 3. Créer les graphiques pour chaque variable binaire
library(ggplot2)
plots <- lapply(vars_binaires, function(var) {
  ggplot(df_plot, aes_string(x = var, y = target_var)) +
    geom_jitter(height = 0.05, width = 0.1, alpha = 0.2, color = "purple") +
    geom_smooth(aes_string(y = "prob_predite"), method = "loess", color = "darkblue", se = FALSE) +
    labs(
      title = "Régression logistique pondérée",
      subtitle = paste("Effet de", var, "sur", target_var),
      x = var,
      y = "Probabilité de voter"
    ) +
    theme_minimal()
})

# 4. Afficher un premier graphique pour test
plots[[1]]

# Pour afficher les autres :
# plots[[2]], plots[[3]], etc.

