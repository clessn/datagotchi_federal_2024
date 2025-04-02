## Ce script fait simplement lister toutes les possibilités de correspondance entre les RTA et les circonscriptions pour qu'on puisse demander la circonscription dans l'app

# Packages ---------------------------------------------------------------
library(dplyr)

sf_rta <- cartessn::spatial_canada_2021_rta

sf_ridings <- cartessn::spatial_canada_2022_electoral_ridings

df_int <- cartessn::intersect_spatial_objects(
  sf_rta,
  "rta",
  sf_ridings,
  "id_riding"
) |> 
  arrange(rta) |> 
  select(rta, id_riding, prop_of_rta_area_covered_by_riding = prop_of_ref_area_covered_by_target)

writexl::write_xlsx(df_int, "_SharedFolder_datagotchi_federal_2024/data/pour_hugo/code_postal_to_circonscription.xlsx")

df_riding_names <- cartessn::names_canada_2022_electoral_ridings

writexl::write_xlsx(df_riding_names, "_SharedFolder_datagotchi_federal_2024/data/pour_hugo/noms_circonscriptions.xlsx")
