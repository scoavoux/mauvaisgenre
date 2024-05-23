
# Make base data ------
# make_artist_list <- function(){
#   # Returns a list of artists that will be
#   # analyzed. Main variable is artist_id
#   #   artist_id
#   #   artist_name
#   #   genre
#   artists_list <- read_csv()
#   return(artists_list)
# }

make_genres_data <- function(){
  require(tidyverse)
  s3 <- initialize_s3()
  f <- s3$get_object(Bucket = "scoavoux", Key = "records_w3/artists_genre_weight.csv")
  genres <- f$Body %>% rawToChar() %>% read_csv()
  genres <- genres %>% 
    select(-artist_name) %>% 
    filter(n_playlists_used > 5) %>% 
    pivot_longer(african:soulfunk, names_to = "genre") %>% 
    # We use the main genre with at least 33% of playlists
    filter(value > .33) %>%
    arrange(artist_id, desc(value)) %>% 
    slice(1, .by = artist_id) %>% 
    select(-n_playlists_used, -value)
  return(genres)
  # Check coverage of genre definition
  # user_artist_peryear %>% 
  #   filter(!is.na(artist_id)) %>% 
  #   left_join(genres) %>% 
  #   mutate(na = is.na(genre)) %>% 
  #   group_by(na) %>% 
  #   summarise(n=sum(n_play))
  # With .4 threshold, 70% of plays; with .3, 79%
}

make_genre_aliases <- function(){
  require(tidyverse)
  genre_aliases <- read_csv("data/genres.csv")
  genre_aliases_char <- genre_aliases$streaminggenre
  names(genre_aliases_char) <- genre_aliases$surveygenre
  return(genre_aliases_char)
}

# Compute aggregate stats on artists ------
# compute_endo_leg <- function(){
#   # input is stream data + user data
#   # output: artist_id, endo_dipl, endo_isei
#   
# }
# 
# compute_exo_press <- function(){
#   
# }

compute_exo_radio <- function(){
  require(tidytable)
  s3 <- initialize_s3()
  f <- s3$get_object(Bucket = "scoavoux", Key = "records_w3/radio/radio_plays_with_artist_id.csv")
  radio <- f$Body %>% rawToChar() %>% fread()
  radio_leg <- c("France Inter", "France Musique", "Fip", "Radio Nova")
  r <- radio %>%
    filter(!is.na(artist_id)) %>% 
    count(artist_id, radio) %>% 
    mutate(leg = if_else(radio %in% radio_leg, n, 0)) %>% 
    group_by(artist_id) %>% 
    summarize(radio_total = sum(n),
              radio_leg = sum(leg))
  return(r)
}

join_artist <- function(...){
  # Make artist dataset by joining all indicators
  s3 <- initialize_s3()
  # artists <- artists_list %>%
  #   left_join()
  f <- s3$get_object(Bucket = "scoavoux", Key = "records_w3/artists.csv")
  artists <- f$Body %>% rawToChar %>% read_csv()
  
  # Proper join
  l <- list(...)
  for(i in seq_along(l)){
    artists <- artists %>% 
      left_join(l[[i]])
  }
  
  # Also recoding artist dataset
  
  artists <- artists %>% 
# turn NA to 0 in radio plays
    mutate(across(starts_with("radio"), ~if_else(is.na(.x), 0, .x))) %>% 
# Scaling legitimacy variables
    mutate(sc_endo_isei = center_scale(endo_isei_mean_pond),
           sc_endo_educ = center_scale(endo_share_high_education_pond),
           sc_exo_press = center_scale(log(total_n_pqnt_texte+1)),
           sc_exo_score = center_scale(senscritique_meanscore),
           sc_exo_radio = center_scale(log(radio_leg+1))
           )

  return(artists)
}

# Compute aggregate stats on users ------

# Code artists ------
filter_artists <- function(artists){
    # Rules of inclusion/exclusion of artists
  artists_filtered <- artists %>% 
    filter(!is.na(genre),
           !is.na(senscritique_meanscore), # has score on senscritique
           parse # has been looked up in press data
           )
  
  # Add PCA
  x <- compute_pca(artists_filtered)
  artists_filtered <- artists_filtered %>% 
    mutate(sc_exo_pca = x$ind$coord[,1])
  
  return(artists_filtered)
}

# compute_exo_pca <- function(artists){
#   artists %>% 
#     select()
# }
