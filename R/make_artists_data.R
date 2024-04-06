
# Make base data ------
make_artist_list <- function(){
  # Returns a list of artists that will be
  # analyzed. Main variable is artist_id
  #   artist_id
  #   artist_name
  #   genre
  artists_list <- read_csv()
  return(artists_list)
}


# Compute aggregate stats on artists ------
compute_endo_leg <- function(){
  # input is stream data + user data
  # output: artist_id, endo_dipl, endo_isei
  
}

compute_exo_press <- function(){
  
}

compute_exo_radio <- function(){
  
}

join_artist <- function(artists_list, ...){
  # Make artist dataset by joining all indicators
  s3 <- initialize_s3()
  # artists <- artists_list %>%
  #   left_join()
  f <- s3$get_object(Bucket = "scoavoux", Key = "records_w3/artists.csv")
  writeBin(f$Body, con = "artists.csv")
  artists <- read_csv("artists.csv")
  
  # Also recoding artist dataset
  artists <- artists %>% 
    # correct artist that do not appear in press search
    # should be moved to computing press coverage
    mutate(total_n_pqnt_texte = ifelse(total_n_pqnt_texte == 0, NA, total_n_pqnt_texte)) %>% 
    # Scale endo and exo variables
    mutate(sc_endo_isei = center_scale(endo_isei_mean_pond),
           sc_endo_educ = center_scale(endo_share_high_education_pond),
           sc_exo_press = center_scale(log(total_n_pqnt_texte+1)),
           sc_exo_score = center_scale(senscritique_meanscore))

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
  return(artists_filtered)
}
