# Modèle Datagotchi Canada 2025

Ce dossier contient les scripts pour le modèle Datagotchi Canada 2025, qui prédit les intentions de vote basées sur divers facteurs sociodémographiques et géographiques.

## Structure du projet

- `model_upgrade2_2025_04_15.R` : Script principal de construction du modèle
- `fonctions_modelisation/` : Fonctions spécifiques au modèle
- `preprocessing/` : Scripts de traitement des données
- `utils/` : Scripts utilitaires
- `compare_models/` : Scripts de comparaison des modèles
- `fonctions/` : Fonctions réutilisables
- `shiny_datagotchi_canada_2025/` : Application Shiny pour visualiser les prédictions
- `previous/` : Versions précédentes des scripts (archive)

## Flux de travail

1. Prétraitement des données (scripts dans `preprocessing/`)
2. Construction et entraînement du modèle (`model_upgrade2_2025_04_15.R`)
3. Utilisation du modèle via l'application Shiny (`shiny_datagotchi_canada_2025/app.R`)

## Dépendances

- R >= 4.1.0
- Packages: tidyverse, nnet, caret, sf, cartessn, pbapply, yardstick, openxlsx

## Description des scripts principaux

### Scripts de prétraitement
- `preprocessing/rta_to_riding.R` : Calcule les prédictions politiques par RTA basées sur l'appartenance aux circonscriptions

### Fonctions de modélisation
- `fonctions_modelisation/calculate_rta_contrib.R` : Calcule les contributions pré-calculées des RTA aux prédictions par parti

### Utilitaires
- `utils/wrangle_coef_xlsx.R` : Traite les coefficients du modèle pour export

### Script principal
- `model_upgrade2_2025_04_15.R` : Construction du modèle RTA amélioré (version 2)