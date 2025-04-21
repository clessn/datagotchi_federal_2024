library(dplyr)
library(ggplot2)
library(forecast)

# Data -------------------------------------------------------------------
df <- readRDS("_SharedFolder_datagotchi_federal_2024/data/panel/02_panelraked_effet_debat.rds")

# Étape 1 — Agrégation pondérée quotidienne
df_ndp <- df |>
  group_by(date_target) |>
  summarise(
    voteprob_ndp = weighted.mean(voteprob_ndp, w = weight_raked, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    intervention = as.integer(date_target >= as.Date("2025-04-16"))
  )

# Étape 2 — Modèle ARIMA + intervention (Box-Tiao)
ts_ndp <- ts(df_ndp$voteprob_ndp, frequency = 1)
fit_ndp <- auto.arima(ts_ndp, xreg = df_ndp$intervention)

# Étape 3 — Intervalle de confiance approximatif (in-sample)
se <- sqrt(fit_ndp$sigma2)
df_ndp <- df_ndp |>
  mutate(
    fitted = fitted(fit_ndp),
    lower = fitted - 1.96 * se,
    upper = fitted + 1.96 * se
  )

# Étape 4 — Visualisation (Box-Tiao)
ggplot(df_ndp, aes(x = date_target)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey80", alpha = 0.4) +
  geom_line(aes(y = voteprob_ndp), color = "#1D3557", size = 0.7) +
  #geom_line(aes(y = fitted), color = "#E07A5F", size = 0.7, linetype = "dashed") +
  geom_point(aes(y = fitted), color = "orange") +
  geom_vline(xintercept = as.Date("2025-04-16"), linetype = "dotted") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Date",
    y = "Probabilité de vote NPD (pondérée)",
    title = "Effet du débat du 16 avril — modèle ARIMA + intervention",
    subtitle = "Ligne pointillée = débat, ligne orange = modèle ajusté"
  ) +
  clessnize::theme_clean_light()


cutoff <- as.POSIXct("2025-04-16 18:00:00")
df_rd <- df |>
  mutate(
    time_from_cutoff = as.numeric(difftime(date_time, cutoff, units = "days")),
    treatment = as.integer(time_from_cutoff >= 0)
  ) |>
  filter(abs(time_from_cutoff) <= 5)


plot_rdd <- function(data, vd, cutoff_datetime = as.POSIXct("2025-04-16 18:00:00")) {
  
  vd_sym <- rlang::sym(vd)
  
  df_rd <- data |>
    mutate(
      time_from_cutoff = as.numeric(difftime(date_time, cutoff_datetime, units = "days")),
      treatment = as.integer(time_from_cutoff >= 0)
    ) |>
    filter(
      abs(time_from_cutoff) <= 5,
      !is.na(!!vd_sym),
      !is.na(weight_raked)
    )

  # Modèle avec pentes différentes
  model <- lm(
    formula = as.formula(paste(vd, "~ time_from_cutoff * treatment")),
    data = df_rd,
    weights = weight_raked
  )

  print(summary(model))  # Résumé du modèle en console

  # Graphique
  ggplot(df_rd, aes(x = time_from_cutoff, y = !!vd_sym)) +
    geom_point(aes(weight = weight_raked), alpha = 0.6, size = 1.5) +
    geom_smooth(
      method = "lm", se = FALSE, color = "#E07A5F",
      data = df_rd |> filter(treatment == 0)
    ) +
    geom_smooth(
      method = "lm", se = FALSE, color = "#1D3557",
      data = df_rd |> filter(treatment == 1)
    ) +
    geom_vline(xintercept = 0, linetype = "dotted") +
    labs(
      title = paste0("Discontinuité sur ", vd, " au moment du débat"),
      subtitle = "Régression locale ± 5 jours autour du 16 avril, 18h",
      x = "Jours depuis le débat",
      y = "Valeur pondérée"
    ) +
    clessnize::theme_clean_light()
}

plot_rdd(
  df, "competence_ndp"
)

plot_rdd(
  df, "competence_lpc"
)

plot_rdd(
  df, "competence_cpc"
)

plot_rdd(
  df, "competence_bq"
)

plot_rdd(
  df, "voteprob_bq"
)

plot_rdd(
  df, "voteprob_cpc"
)

plot_rdd(
  df, "voteprob_lpc"
)
