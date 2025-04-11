# Mise à jour 3 du modèle (Avril 2025)

Cette mise à jour introduit des interactions régionales dans notre modèle de prédiction électorale.

## Structure des dossiers

- **quebec_roc/** - Implémentation avec distinction Québec vs Reste du Canada
  - `model_upgrade3_quebec_roc.R` - Script principal pour le modèle avec dummies is_quebec et is_roc

- **regions_6/** - Implémentation avec 6 régions distinctes du Canada
  - `model_upgrade3_regions6.R` - Script principal pour le modèle avec 6 dummies régionales
    - Ontario
    - Québec
    - Colombie-Britannique
    - Prairies
    - Provinces atlantiques
    - Territoires

## Objectif

Ces modèles visent à capturer les différences régionales dans les comportements électoraux à travers le Canada en utilisant des coefficients spécifiques à chaque région pour les variables sociodémographiques et les prédictions RTA.

## Comparaison

Les résultats des deux approches peuvent être comparés pour déterminer si une distinction fine entre 6 régions apporte une meilleure prédiction qu'une simple distinction Québec vs ROC.