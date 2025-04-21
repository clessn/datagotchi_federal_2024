## cleaning artiste et genre musical --------------------------------------------------------------

library(dplyr)
library(jsonlite)
library(tidyr)
library(purrr)

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
DataTmp <- DataRaw %>%
  mutate(
    artist_name = map_chr(music, extract_field("artist")),
    genre = map_chr(music, extract_field("genre")),
    movie = map_chr(cinema, extract_field("title")),
  )

DataClean$lifestyle_favArtist <- DataTmp$artist_name
DataClean$lifestyle_musicGenre <- DataTmp$genre
DataClean$lifestyle_favMovie <- DataTmp$movie
