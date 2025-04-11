## cleaning artiste et genre musical --------------------------------------------------------------

library(jsonlite)

# Fonction sécurisée avec double vérification
extract_field <- function(field) {
  function(x) {
    if (is.na(x) || x == "") return(NA_character_)
    
    out <- tryCatch({
      value <- fromJSON(x)[[field]]
      if (length(value) == 0) NA_character_ else value
    }, error = function(e) NA_character_)
    
    return(out)
  }
}

# Application
DataClean <- DataRaw %>%
  mutate(
    artist_name = map_chr(music, extract_field("artist")),
    genre = map_chr(music, extract_field("genre"))
  )
