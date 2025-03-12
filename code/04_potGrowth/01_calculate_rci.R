## Prend les données du pilote et on calculte le RCI pour chaque parti

# Packages ---------------------------------------------------------------
library(dplyr)

# Data -------------------------------------------------------------------

## J'ai pris celles de clustering. J'imagine c'est la meilleure option
df_pilote <- readRDS("_SharedFolder_datagotchi_federal_2024/data/pilote/dataClean/datagotchi2025_canada_pilotClustering_20250310.rds")

# Calculate RCI ----------------------------------------------------------

df_pilote_with_rci <- df_pilote |> 
  select(id, starts_with("dv_potgrowth"), -starts_with("dv_potgrowthQc")) |> 
  tidyr::pivot_longer(
    cols = starts_with("dv_potgrowth"),
    names_to = "party",
    names_prefix = "dv_potgrowth",
    values_to = "potgrowth"
  ) |> 
  group_by(id) |> 
  mutate(
    max_potgrowth = max(potgrowth),
    leader = ifelse(potgrowth == max_potgrowth, 1, 0),
    trailer = ifelse(potgrowth != max_potgrowth, 1, 0),
    n_leaders = sum(leader),
    potgrowth_trailers = ifelse(trailer == 1, potgrowth, NA),
    second_potgrowth = case_when(
      n_leaders == 1 ~ max(potgrowth_trailers, na.rm = TRUE),
      n_leaders >= 2 ~ max_potgrowth
    ),
    rci = case_when(
      leader == 1 ~ potgrowth - second_potgrowth,
      trailer == 1 ~ potgrowth - max_potgrowth
    )
  ) |>
  select(id, party, rci) %>% 
  tidyr::pivot_wider(., id_cols = "id",
              values_from = "rci",
              names_from = "party",
              names_prefix = "rci_") |>
  ungroup() %>% 
  select(-id) %>%
  cbind(
    df_pilote, .
  )

# Save it ----------------------------------------------------------------

saveRDS(df_pilote_with_rci, "_SharedFolder_datagotchi_federal_2024/data/potGrowth/01_pilote_with_rci.rds")
