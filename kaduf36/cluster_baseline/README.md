# Création d'un ensemble de clusters baseline

Description des étapes pour créer des clusters avec un minimum de choix par rapport aux variables. 

Les clusters créés à cette première étape de création de cluster serviront à 

- S'assurer que le processus de clustering fonctionne bien
- Créer un ensemble de cluster simple qui a demandé le minimum d'effort qui serviront de point de départ aux comparaisons pour amélioration.

## 01 - Initialisation

À l'étape 01, on initialise les tables de données avec lesquelles nous travaillerons.

Il faut s'assurer que les variables sont identiques entre le pilote et l'application. Typiquement, il y a plus de variables dans le pilote, il faudra s'assurer de ne conserver que les variables qui sont également présente dans l'app.

Il est possible qu'il y ait des différences au niveau des noms pour une même variable entre le pilote et l'app. C'est à cette étape que le nom des variables devra être harmonisée entre le pilote et l'app.

La sélection et l'harmonisation des variables entre le pilote et l'application sont effectuées dans le fichier `01-2_initialisation.R`.

Les variables qui seront utilisées, sont regroupées en sous-ensemble de thèmes semblables. Cela a l'avantage de travailler avec un petit groupe de variables à la fois, ce qui facilite grandement les analyses préliminaires et la familiarisation avec les variables et leurs valeurs.

Les sous-groupes de variables sont définis dans le fichier `01-1_variables.R`.

## 02 - Préparation des données

À l'étape 02, on prépare les données pour le clustering.

Sous-ensemble par sous-ensemble, les variables sont analysées surtout en fonction de leur distribution.

Chaque variable est transformée selon les besoins du clustering en prenant des décisions qui sont également cohérentes avec la théorie.

Les notebooks sont privilégiés à cette étape, car il y a de l'exploration à faire avant de prendre des décisions sur chacune des variables.

À la fin de chaque notebook, il est suggéré de faire une activité de clustering sur les variables de ce sous-ensemble. Cette activité est facultative et pout servir à voir si notre transformation de variable a du sens lorsque l'on veut faire des cluster. Elle sert principalement à développer notre intuition sur les variables.

Le dernier notebook de cette étape sert à assembler les tables de variables préparées en sous-ensemble.

Finalement, le fichier `02-X_variables.R` qui est la dernière sous-étape de cette étape permet de déclarer des variables contenant les nouveaux noms de variables par sous-ensemble. La préparation des variables changes parfois la nature ou le nombre de variables d'un sous-groupe et par conséquent, les sous-groupes de variables définis en 01 ne sont plus valables pour la suite.

À la sortie de cette étape, on a 

- `02_pilot1_2022.rds` : Un fichier contenant les données prêtes à être utilisées dans le clustering

## 03 - Clustering

À l'étape 03, on construit les clusters. Les étapes sont les suivantes

03-0 : Copie du fichier `02-X_variables.R` qui contient le nom des variables disponibles pour le clustering. C'est à partir de ce fichier que la sélection des variables pour le clustering s'effectue     

03-1 : Utilisation d'une méthode de clustering et d'une sélection de variables pour construire des clusters

03-2 : Permet de voir la distribution des répondants à travers les clusters

03-3 : Permet de connaître les variables les plus distinctives entre les clusters

03-4 : Créé des prompts pour demander à ChatGPT une suggestion pour le nom et la description des clusters

03-5 : Affiche la distribution des clusters à travers le territoire