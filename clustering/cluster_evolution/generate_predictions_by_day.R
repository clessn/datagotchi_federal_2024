Loader le modèle du 1er script
Loader le kmeans_result
Loader les données de lapp
Loop: pour chaque jour, faire un modèle bayésien vote_intent ~ SES + lifestyle qui prend comme priors les coefficients du modèle du 1er script
Prédire ce modèle pour chaque centroid
Enregistrer une dataframe avec la prédiction de chaque cluster par jour