
data <- readRDS("_SharedFolder_datagotchi_federal_2024/data/app/dataClean/datagotchi2025_canada_app_20250314.rds")

# Afficher la structure du jeu de données
str(data)

# Obtenir un résumé statistique
summary(data)

# Afficher les premières lignes
head(data)

# Afficher les noms des colonnes
names(data)

# Vérifier les dimensions du jeu de données
dim(data)

# Vérifier s'il y a des valeurs manquantes
colSums(is.na(data))
