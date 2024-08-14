# Make artists popularity data ------
# For each artist, compute their use in control group
# and in respondent group (criteria for selecting artists + assess
# the coverage of our sample)
make_artist_popularity_data <- function(user_artist_peryear){
  library(tidyverse)
  library(tidytable)
  library(arrow)
  s3 <- initialize_s3()
  
  # To remove "fake" artists: accounts that compile anonymous music
  to_remove <- fread("data/artists_to_remove.csv") %>% 
    select(artist_id)

  # compute artist/hashed_id pairs (collapse years)
  pop_raw <- user_artist_peryear %>% 
    filter(!is.na(artist_id)) %>% 
    group_by(artist_id, hashed_id) %>% 
    summarise(l_play = sum(l_play, na.rm=TRUE),
              n_play = sum(n_play, na.rm=TRUE)) %>% 
    anti_join(to_remove)
  
  # separate between survey respondants and control group
  f <- s3$download_file(Bucket = "scoavoux", 
                        Key = "records_w3/RECORDS_hashed_user_group.parquet", 
                        Filename = "data/temp/RECORDS_hashed_user_group.parquet")
  us <- read_parquet("data/temp/RECORDS_hashed_user_group.parquet")
  rm(f)
  
  pop_control <- us %>% 
    filter(is_in_control_group) %>% 
    select(hashed_id) %>% 
    inner_join(pop_raw) %>% 
    group_by(artist_id) %>% 
    summarise(l_play = sum(l_play, na.rm=TRUE),
              n_play = sum(n_play, na.rm=TRUE),
              n_users = n_distinct(hashed_id)) %>% 
    mutate(f_l_play = l_play / sum(l_play, na.rm=TRUE),
           f_n_play = n_play / sum(n_play, na.rm=TRUE)) %>% 
    rename_with(~paste0("control_", .x), -artist_id)
  
  pop_respondants <- us %>% 
    filter(is_respondent) %>% 
    select(hashed_id) %>% 
    inner_join(pop_raw) %>% 
    group_by(artist_id) %>% 
    summarise(l_play = sum(l_play, na.rm=TRUE),
              n_play = sum(n_play, na.rm=TRUE),
              n_users = n_distinct(hashed_id)) %>% 
    mutate(f_l_play = l_play / sum(l_play, na.rm=TRUE),
           f_n_play = n_play / sum(n_play, na.rm=TRUE)) %>% 
    rename_with(~paste0("respondent_", .x), -artist_id)
  
  pop <- full_join(pop_control, pop_respondants)
  return(pop)
}

# Make endogenenous legitimacy data ------
# For each artist, compute mean ISEI and share of audience with a 
# high education
make_endogenous_legitimacy_data <- function(user_artist_peryear, isei, survey_raw){
  library(tidyverse)
  library(tidytable)
  
  isei <- filter(isei, !is.na(isei))

  pop <- user_artist_peryear %>% 
    filter(!is.na(artist_id)) %>% 
    group_by(artist_id, hashed_id) %>% 
    summarise(l_play = sum(l_play, na.rm=TRUE))
  
  artist_mean_isei <- pop %>% 
    inner_join(isei) %>% 
    group_by(artist_id) %>% 
    mutate(f = l_play / sum(l_play)) %>% 
    summarise(n_isei = n(),
              endo_isei_mean_pond = sum(f*isei)) %>% 
    filter(!is.na(endo_isei_mean_pond))
  
  ed <- survey_raw %>% 
    filter(E_diploma != "", !is.na(E_diploma)) %>% 
    mutate(higher_ed = as.numeric(E_diploma %in% c("Master, diplôme d'ingénieur.e, DEA, DESS", "Doctorat (y compris médecine, pharmacie, dentaire), HDR" ))) %>% 
    select(hashed_id, higher_ed) %>% 
    filter(!is.na(higher_ed))
  
  artist_share_higher_education <- pop %>% 
    inner_join(ed) %>% 
    group_by(artist_id) %>% 
    mutate(f = l_play / sum(l_play)) %>% 
    summarise(endo_share_high_education_pond = sum(f*higher_ed)) %>% 
    filter(!is.na(endo_share_high_education_pond))
  
  return(full_join(artist_mean_isei, artist_share_higher_education))
}

# Make senscritique ratings ------
make_senscritique_ratings_data <- function(senscritique_mb_deezer_id, track_weight = .2){
  # track_weight is the weight given to tracks relative to albums for computing 
  # average at the artist level. Defaults to .2 => one track is 1/5 of one album
  require(tidyverse)
  s3 <- initialize_s3()

  ## Albums ratings
  f <- s3$get_object(Bucket = "scoavoux", Key = "senscritique/ratings.csv")
  ratings <- f$Body %>% rawToChar() %>% read_csv()
  rm(f)
  f <- s3$get_object(Bucket = "scoavoux", Key = "senscritique/contacts_albums_link.csv")
  contacts_albums_list <- f$Body %>% rawToChar() %>% read_csv()
  rm(f)

  # signification of contact_subtype_id?
  # 11 = personne artiste
  # 12 = label
  # 13 = groupe artiste
  # 26 = producteur
  
  contacts_albums_list <- contacts_albums_list %>% 
    filter(contact_subtype_id %in% c(11, 13))
  
  albums_ratings <- ratings %>%
    group_by(product_id) %>% 
    summarize(n = n(),
              mean = mean(rating)) %>% 
    # OK let us consider that 4 grades is enough
    filter(n > 3) %>% 
    inner_join(contacts_albums_list) %>% 
    mutate(weight = 1) %>% 
    select(-contact_subtype_id)
  
  ## Tracks ratings
  f <- s3$get_object(Bucket = "scoavoux", Key = "senscritique/tracks.csv")
  tracks <- f$Body %>% rawToChar() %>% read_csv()
  rm(f)
  f <- s3$get_object(Bucket = "scoavoux", Key = "senscritique/contact_tracks_link.csv")
  contacts_tracks_list <- f$Body %>% rawToChar() %>% read_csv()
  rm(f)
  
  tracks_ratings <- tracks %>% 
      filter(rating_count > 3) %>% 
      mutate(mean = rating_average/10, weight = track_weight) %>% 
      select(product_id, mean, n= "rating_count", weight) %>% 
      inner_join(contacts_tracks_list)

  # in_sc_not_deezer <- bind_rows(albums_ratings, tracks_ratings) %>% 
  #   distinct(contact_id) %>% 
  #   anti_join(select(senscritique_mb_deezer_id, contact_id, consolidated_artist_id) %>% 
  #                distinct())
  
  
  cora <- bind_rows(albums_ratings, tracks_ratings) %>% 
    # HERE IS A BOTTLENECK: artists that have ratings but senscritique id is 
    # not paired to deezer id. Let us try to find a link
    inner_join(select(senscritique_mb_deezer_id, contact_id, consolidated_artist_id) %>% 
                 distinct()) %>% 
    group_by(consolidated_artist_id) %>% 
    summarise(senscritique_maxscore = max(mean),
              senscritique_minscore = min(mean),
              senscritique_meanscore = sum(mean * weight)/sum(weight),
              senscritique_meanscore_onlyalbums = sum(mean * (weight-track_weight))/sum(weight-track_weight),
              senscritique_n_albums = sum(weight == 1),
              senscritique_n_tracks = sum(weight == track_weight)) %>% 
    rename(artist_id = "consolidated_artist_id")
  return(cora)
}

# Make genre data ------

make_genres_data <- function(.source = "deezer_editorial_playlists", senscritique_mb_deezer_id){
  require(tidyverse)
  s3 <- initialize_s3()
  if(.source == "deezer_editorial_playlists"){
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
      select(-n_playlists_used, -value) %>% 
      mutate(genre = recode_vars(genre, .source))
    # Check coverage of genre definition
    # user_artist_peryear %>% 
    #   filter(!is.na(artist_id)) %>% 
    #   left_join(genres) %>% 
    #   mutate(na = is.na(genre)) %>% 
    #   group_by(na) %>% 
    #   summarise(n=sum(n_play))
    # With .4 threshold, 70% of plays; with .3, 79%
    
  } else if(.source == "deezer_maingenre"){
    s3$download_file(Bucket = "scoavoux", 
                     Key = "records_w3/items/artists_data.snappy.parquet",
                     Filename = "data/temp/artists_data.snappy.parquet")
    artists <- read_parquet("data/temp/artists_data.snappy.parquet", col_select = 1:3)
    artists <- artists %>% 
      filter(!is.na(main_genre))
    genres <- artists %>% 
      select(artist_id, genre = "main_genre") %>% 
      mutate(genre = recode_vars(genre, .source))
    # TODO:Collapse
    
  } else if(.source == "senscritique_tags"){
    f <- s3$get_object(Bucket = "scoavoux", Key = "senscritique/albums_tags.csv")
    album_tags <- f$Body %>% rawToChar() %>% read_csv()
    f <- s3$get_object(Bucket = "scoavoux", Key = "senscritique/tags_meaning.csv")
    tags_meaning <- f$Body %>% rawToChar() %>% read_csv()
    f <- s3$get_object(Bucket = "scoavoux", Key = "senscritique/contacts_albums_link.csv")
    contacts_albums_list <- f$Body %>% rawToChar() %>% read_csv()
    rm(f)
    
    genres <- left_join(album_tags, tags_meaning) %>% 
      left_join(select(contacts_albums_list, -contact_subtype_id), relationship = "many-to-many") %>% 
      left_join(select(senscritique_mb_deezer_id, -mbid), relationship = "many-to-many") %>% 
      mutate(genre = recode_vars(genre, .source)) %>% 
      filter(!is.na(artist_id), !is.na(genre)) %>%
      count(artist_id, product_id, genre) %>% 
      mutate(f = n/sum(n), .by = product_id) %>% 
      summarise(f = sum(f), .by = c(artist_id, genre)) %>% 
      mutate(f = f / sum(f), .by = artist_id) %>% 
      arrange(artist_id, desc(f)) %>% 
      slice(1, .by = artist_id) %>% 
      filter(f > .3) %>% 
      select(artist_id, genre)
    # TODO: collapse
    
  } else if(.source == "musicbrainz_tags"){
    # THERE IS SOMETHING WRONG PROBABLY IN MBID. FOR INSTANCE COCTEAU TWINS is
    # "salsa choke" and Death Cab for Cutie is "Non-Music"
    # f <- s3$get_object(Bucket = "scoavoux", Key = "senscritique/mbid_deezerid_pair.csv")
    # mb_dz <- f$Body %>% rawToChar() %>% read_csv()
    # mb_dz <- mb_dz %>% filter(!is.na(mbid))
    # f <- s3$get_object(Bucket = "scoavoux", Key = "musicbrainz/mbid_genre.csv")
    # mb_genres <- f$Body %>% rawToChar() %>% read_csv()
    # mb_genres <- mb_genres %>% filter(!is.na(mbid))
    # mbg <- mb_dz %>% 
    #   inner_join(mb_genres) %>% 
    #   arrange(mbid, desc(count)) %>% 
    #   slice(1, .by = mbid)
  }
  return(genres)
}

# Merge genres ------
## Takes a list of tibbles describing artist genres
## and return a list of unique genre attribution for each artist
merge_genres <- function(...){
  require(tidyverse)  
  # order of arguments is the order of priority of databases
  genres <- bind_rows(...) %>% 
    slice(1, .by = artist_id)
  return(genres)
}

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
  require(tidyverse)
  # Proper join
  l <- list(...)
  artists_raw <- l[[1]]
  for(i in 2:length(l)){
    artists_raw <- artists_raw %>% 
      full_join(l[[i]])
  }
  return(artists_raw)
}

# Filter artists ------
## We define here how to 
filter_artists <- function(artists_raw){
  require(tidyverse)
  
  # Rules of inclusion/exclusion of artists
  artists <- artists_raw %>% 
    filter(!is.na(genre),
           !is.na(senscritique_meanscore), # has score on senscritique
           !is.na(endo_isei_mean_pond),
           n_isei > 5,
           !is.na(total_n_pqnt_texte) # has been searched in the press data
           #parse # has been looked up in press data
    )
  
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
  

  # Add PCA
  x <- compute_pca(artists)
  artists <- artists %>% 
    mutate(sc_exo_pca = center_scale(x$ind$coord[,1]))
  
  return(artists)
}

