# Version simplifiée et robuste du code
DataClean$dv_voteChoice <- NA  # Initialisation

# Création d'un vecteur de la bonne taille
temp_vote <- rep(NA, nrow(DataRaw))

# Traitement des cas valides sans utiliser de fonctions qui risquent de retourner 0 éléments
for (i in 1:nrow(DataRaw)) {
  # Vérifie si confirmation = 1
  if (!is.na(DataRaw$confirmation[i]) && DataRaw$confirmation[i] == 1) {
    # Vérifie si result et correction sont tous deux non NA
    if (!is.na(DataRaw$result[i]) && !is.na(DataRaw$correction[i])) {
      # Cas 1: result = correction
      if (DataRaw$result[i] == DataRaw$correction[i]) {
        temp_vote[i] <- DataRaw$result[i]
      }
      # Cas 2: result ≠ correction et correction est entre 1-5
      else if (DataRaw$correction[i] %in% 1:5) {
        temp_vote[i] <- DataRaw$correction[i]
      }
      # Autre cas: laisse NA
    }
  }
}

# Transfert des résultats vers DataClean
DataClean$dv_voteChoice <- factor(temp_vote,
                               levels = 1:5,
                               labels = c("lpc", "cpc", "npd", "bq", "gpc"))

# Vérification
print(table(DataClean$dv_voteChoice, useNA = "always"))
