Dans ce dossier, nous voulons avoir le potentiel de croissance de chaque
parti fédéral (5), par cluster (X?) et pour chaque jour.

Voici comment j’ai pensé la structure du dossier:

-   `01\_calculate_rci.R`: prend les données du pilote et on calculte le RCI pour chaque parti (HUBERT).
-   `02_assign_clusters.R`: prend les données du pilote avec le RCI (output du script `01\_calculate_rci.R`) et on ajoute une colonne `cluster` qui contient le cluster de chaque répondant (SARAH).
- `03_agregate_rci.R`: prend l'output du script `02_assign_clusters.R` et agrège le RCI par cluster par jour. On peut faire une moyenne mobile qui prend en compte la moyenne des derniers jours (HUBERT et SARAH).

Également, dans `_SharedFolder_datagotchi_federal_2024/data`, j'ai ajouté le dossier `04_potGrowth`. On peut mettre nos jeux de données dans ce dossier.

- `01_pilote_with_rci.rds`: output du script `01\_calculate_rci.R`
