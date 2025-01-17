export_artists_csv <- function(artists){
  file <- "output/artists.csv"
  write_csv(artists, file)
  return(file)
}

export_users_csv <- function(survey){
  file <- "output/users.csv"
  write_csv(survey, file)
  return(file)
}