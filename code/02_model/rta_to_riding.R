# Charger les packages nécessaires
library(cartessn)
library(dplyr)
library(sf)

# Charger les données spatiales du package
rta <- cartessn::spatial_canada_2021_rta  # RTA canadiennes
circo <- cartessn::spatial_canada_2022_electoral_ridings # Circonscriptions électorales alignées

# S'assurer que les deux objets utilisent le même système de coordonnées
st_crs(rta) == st_crs(circo)  # Vérifier si les CRS sont identiques
# Si non, transformer un des objets pour qu'ils correspondent
if (st_crs(rta) != st_crs(circo)) {
  rta <- st_transform(rta, st_crs(circo))
}

# Calculer l'intersection entre les RTA et les circonscriptions
# en utilisant la fonction du package
intersections <- cartessn::intersect_spatial_objects(
  spatial_ref = rta,
  id_ref = "rta",  # Identifiant des RTA (vérifiez le nom exact de la colonne)
  spatial_target = circo,
  id_target = "id_riding",  # Identifiant des circonscriptions (vérifiez le nom exact)
  dTolerance = 50  # Ajustez selon la précision désirée
)

# Joindre les noms des circonscriptions pour plus de lisibilité
# Supposons que les noms sont dans la colonne "fedname" de l'objet circo
noms_circo <- circo %>%
  st_drop_geometry() %>%
  select(id_riding, name_riding_en)

# Créer le tableau final avec les proportions d'appartenance
rta_circo_appartenance <- intersections %>%
  left_join(noms_circo, by = "id_riding") %>%
  # Convertir en pourcentage pour plus de lisibilité
  mutate(pourcentage_appartenance = prop_of_ref_area_covered_by_target * 100) %>%
  # Sélectionner les colonnes pertinentes
  select(
    rta = rta,
    circo_id = id_riding,
    circo_nom = name_riding_en,
    aire_intersection_m2 = area_covered_by_target_m2,
    pourcentage_appartenance
  ) %>%
  # Trier par RTA et pourcentage d'appartenance décroissant
  arrange(rta, desc(pourcentage_appartenance))

# Exporter le résultat en CSV
write.csv(rta_circo_appartenance, "_SharedFolder_datagotchi_federal_2024/data/modele/rta_circonscriptions_appartenance.csv", row.names = FALSE)

# Pour vérification, afficher les 5 premières lignes du résultat
head(rta_circo_appartenance, 5)
