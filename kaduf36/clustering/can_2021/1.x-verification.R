###### Assurance qualité des données de la partie 1.2 ######
#-----------------------------------------------------------

# Lecture du fichier contenant les données nettoyées de l'application
df_datagotchi_2021 <- read.csv(
  "data/DatagotchiHub-federal-2021-08-03-2022-.csv"
)

# Ne conserver que les réponses du Québec (comme pour les pilotes)
df_datagotchi_2021 <- df_datagotchi_2021 %>%
  filter(quebec == 1)

# Lecture du fichier contenant les réponses brutes provenant de l'application
df_datagotchi_2021_raw <- read.csv("data/RawData-Hub.csv")

# Ne conserver que les réponses du Québec (comme pour les pilotes)
df_datagotchi_2021_raw <- df_datagotchi_2021_raw |>
  filter(answers.province == "Québec")

# ????? Comment on fait pour savoir que les lignes sont associées aux
# mêmes personnes : Pour le moment, les deux fichiers ont le même
# nombre de répondants, il est présumé que l'ordre est le même.

# Vérifions tout de même s'il y a des colonnes communes et,
# le cas échéant, vérifier que les valeurs sont les mêmes

# N'utiliser que les colonnes qui ont des valeurs codées de la même façon
common_cols <- c("answers.music.genre", "answers.ethnicity")

# Vérifier si les colonnes communes ont les mêmes valeurs sur les mêmes lignes
same_values <- sapply(common_cols, function(col) all(df_datagotchi_2021[[col]] == df_datagotchi_2021_raw[[col]]))

# Résultat : TRUE si les valeurs sont identiques, FALSE sinon, pour chaque colonne commune
print(same_values)

# Comparer les colonnes communes et identifier les lignes différentes
diff_rows <- which(!apply(df_datagotchi_2021[common_cols] == df_datagotchi_2021_raw[common_cols], 1, all))

# Créer une table combinée pour voir les différences côte à côte
comparison_table <- data.frame(
  ligne = diff_rows,
  df_2021 = df_datagotchi_2021[diff_rows, common_cols, drop = FALSE],
  df_2021_raw = df_datagotchi_2021_raw[diff_rows, common_cols, drop = FALSE]
)

# Voir les différences
# View(comparison_table)

###### ----> Pour ces deux variables, il y a deux lignes qui ne correspondent pas (inversées?)
###### ----> Faut-il retirer ces deux lignes?
df_datagotchi_2021 <- df_datagotchi_2021[-comparison_table$ligne, ]
df_datagotchi_2021_raw <- df_datagotchi_2021_raw[-comparison_table$ligne, ]

###### ----> Est-ce que ça vaut la peine de pousser l'analyse plus loin? Vérifier les autres variables?

# Vérification des autres variables
common_cols <- intersect(colnames(df_datagotchi_2021), colnames(df_datagotchi_2021_raw))
common_cols <- setdiff(common_cols, c("answers.music.genre", "answers.ethnicity"))
print(common_cols)

sum(table(df_datagotchi_2021$answers.alcohol, df_datagotchi_2021_raw$answers.alcohol) != 0) == length(unique(df_datagotchi_2021$answers.alcohol))
sum(table(df_datagotchi_2021$answers.cinema.genre, df_datagotchi_2021_raw$answers.cinema.genre) != 0) == length(unique(df_datagotchi_2021$answers.cinema.genre)) # FALSE parce qu'il y a une colonne de zeros
sum(table(df_datagotchi_2021$answers.clothing, df_datagotchi_2021_raw$answers.clothing) != 0) == length(unique(df_datagotchi_2021$answers.clothing))
sum(table(df_datagotchi_2021$answers.coffee_shop, df_datagotchi_2021_raw$answers.coffee_shop) != 0) == length(unique(df_datagotchi_2021$answers.coffee_shop))
sum(table(df_datagotchi_2021$answers.dwelling, df_datagotchi_2021_raw$answers.dwelling) != 0) == length(unique(df_datagotchi_2021$answers.dwelling))
sum(table(df_datagotchi_2021$answers.food, df_datagotchi_2021_raw$answers.food) != 0) == length(unique(df_datagotchi_2021$answers.food))
sum(table(df_datagotchi_2021$answers.pets, df_datagotchi_2021_raw$answers.pets) != 0) == length(unique(df_datagotchi_2021$answers.pets))
sum(table(df_datagotchi_2021$answers.shopping, df_datagotchi_2021_raw$answers.shopping) != 0) == length(unique(df_datagotchi_2021$answers.shopping))
sum(table(df_datagotchi_2021$answers.smoking, df_datagotchi_2021_raw$answers.smoking) != 0) == length(unique(df_datagotchi_2021$answers.smoking))
sum(table(df_datagotchi_2021$answers.sport, df_datagotchi_2021_raw$answers.sport) != 0) == length(unique(df_datagotchi_2021$answers.sport))
sum(table(df_datagotchi_2021$answers.transport, df_datagotchi_2021_raw$answers.transport) != 0) == length(unique(df_datagotchi_2021$answers.transport))
sum(table(df_datagotchi_2021$answers.vehicle, df_datagotchi_2021_raw$answers.vehicle) != 0) == length(unique(df_datagotchi_2021$answers.vehicle))

### Tout semble OK :)

#######----------- Fin temporaire de l'analyse