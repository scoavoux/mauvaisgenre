
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

#tar_load("user_artist_peryear")

# Make artists popularity data ------
# For each artist, compute their use in control group
# and in respondent group (criteria for selecting artists + assess
# the coverage of our sample)
make_artist_popularity_data <- function(user_artist_peryear){
  library(tidyverse)
  library(tidytable)
  library(arrow)
  s3 <- initialize_s3()

  # compute artist/hashed_id pairs (collapse years)
  pop_raw <- user_artist_peryear %>% 
    filter(!is.na(artist_id)) %>% 
    group_by(artist_id, hashed_id) %>% 
    summarise(l_play = sum(l_play, na.rm=TRUE),
              n_play = sum(n_play, na.rm=TRUE))
  
  # separate between survey respondants and control group
  f <- s3$download_file(Bucket = "scoavoux", Key = "records_w3/RECORDS_hashed_user_group.parquet", Filename = "data/RECORDS_hashed_user_group.parquet")
  us <- read_parquet("data/RECORDS_hashed_user_group.parquet")
  rm(f)
  
  pop_control <- us %>% 
    filter(is_in_control_group) %>% 
    select(hashed_id) %>% 
    inner_join(pop_raw) %>% 
    group_by(artist_id) %>% 
    summarise(l_play = sum(l_play, na.rm=TRUE),
              n_play = sum(n_play, na.rm=TRUE),
              n_users = n()) %>% 
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
              n_users = n()) %>% 
    mutate(f_l_play = l_play / sum(l_play, na.rm=TRUE),
           f_n_play = n_play / sum(n_play, na.rm=TRUE)) %>% 
    rename_with(~paste0("respondent_", .x), -artist_id)
  
  pop <- full_join(pop_control, pop_respondants)
  return(pop)
}


make_endogenous_legitimacy_data <- function(user_artist_peryear, isei){
  library(tidyverse)
  library(tidytable)
  
  isei <- filter(isei, !is.na(isei))
  # Base data: streams
  pop <- user_artist_peryear %>% 
    filter(!is.na(artist_id)) %>% 
    inner_join(isei) %>% 
    group_by(artist_id, hashed_id) %>% 
    summarise(l_play = sum(l_play, na.rm=TRUE),
              n_play = sum(n_play, na.rm=TRUE))
  
  artist_mean_isei <- pop %>% 
    inner_join(isei) %>% 
    group_by(artist_id) %>% 
    mutate(f = l_play / sum(l_play)) %>% 
    summarise(mean_isei = sum(f*isei)) %>% 
    filter(!is.na(mean_isei))
  
  return(artist_mean_isei)
}

# Make senscritique data ------
# Makes pairing of deezer id (artist_id) with musicbrainz and senscritique
# Starts from dump of senscritique SQL database,
# made July, 1st 2024

make_senscritique_pairing_data <- function(){
  require(tidyverse)
  require(tidytable)
  require(WikidataQueryServiceR)
  s3 <- initialize_s3()
  
  # Spotify/deezer id from WIKIDATA
  wikidata_spotify_deezer <- query_wikidata('SELECT DISTINCT ?spotify_id ?deezer_id #?item ?itemLabel 
  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE]". }
    ?item p:P1902 ?statement0.
    ?statement0 (ps:P1902) _:anyValueP1902.
    ?item p:P2722 ?statement1.
    ?statement1 (ps:P2722) _:anyValueP2722.
    ?item wdt:P2722 ?deezer_id.
    ?item wdt:P1902 ?spotify_id.
  }')

  # Spotify/deezer id from WIKIDATA
  f <- s3$get_object(Bucket = "scoavoux", Key = "senscritique/mbid_wikidataid_pair.csv")
  mbz_wikidata <- f$Body %>% rawToChar() %>% read_csv()
  rm(f)
  
  wikidata_deezer <- query_wikidata('SELECT DISTINCT ?item ?deezer_id #?item ?itemLabel 
  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE]". }
    ?item p:P2722 ?statement1.
    ?statement1 (ps:P2722) _:anyValueP2722.
    ?item wdt:P2722 ?deezer_id.
  }')
  wikidata_deezer <- wikidata_deezer %>% 
    mutate(wikidata_id = str_extract(item, "/(Q\\d+)", group = 1)) %>% 
    select(-item)
  
  mbz_wikidata <- mbz_wikidata %>% 
    select(-mbname) %>% 
    inner_join(wikidata_deezer) %>% 
    select(-wikidata_id)
  
  # Musicbrainz id / deezer id pairs from musicbrainz dumps
  f <- s3$get_object(Bucket = "scoavoux", Key = "senscritique/mbid_deezerid_pair.csv")
  mbz_dz <- f$Body %>% rawToChar() %>% read_csv()
  rm(f)
  
  # SensCritique / deezer id from Deezer api search
  f <- s3$get_object(Bucket = "scoavoux", Key = "senscritique/senscritique_id_deezer_id_pairing.csv")
  pairings <- f$Body %>% rawToChar() %>% read_csv()
  rm(f)
  f <- s3$get_object(Bucket = "scoavoux", Key = "senscritique/senscritique_deezer_id_pairing_2.csv")
  pairings2 <- f$Body %>% rawToChar() %>% read_csv()
  pairings <- pairings %>% 
    select(contact_id, artist_id) %>% 
    bind_rows(select(pairings2, contact_id, artist_id = "deezer_id")) %>% 
    distinct()
  
  # TODO: see how many artists we are missing and whether we should add another
  # service with spotify/deezer ids (or push them to wikidata)
  # especially: see maping senscritique id deezer id used on previous
  # project (based on... search in SC database?)
  # Also need to check whether this is a problem with SC spotify links or
  # with wikidata not having spotify/deezer match.
  f <- s3$get_object(Bucket = "scoavoux", Key = "senscritique/contacts.csv")
  co <- f$Body %>% rawToChar() %>% fread()
  rm(f)
  
  co <- distinct(co) %>% 
    mutate(spotify_id = str_remove(spotify_id, "spotify:artist:")) %>% 
    rename(mbid = "mbz_id")
  
  co <- co %>% 
    left_join(select(wikidata_spotify_deezer, spotify_id, deezer_id), by = "spotify_id") %>% 
    left_join(select(mbz_wikidata, mbid, deezer_id_wk = "deezer_id")) %>% 
    left_join(select(mbz_dz, mbid, deezer_id_mb = "deezer_id")) %>% 
    left_join(select(pairings, contact_id, deezer_id_p = "artist_id"))
  
  co <- co %>% 
    mutate(deezer_id = as.numeric(deezer_id),
           artist_id = case_when(!is.na(deezer_id) ~ deezer_id, 
                                 !is.na(deezer_id_wk) ~ deezer_id_wk,
                                 !is.na(deezer_id_mb) ~ deezer_id_mb,
                                 TRUE ~ deezer_id_p),
           id_origin = case_when(!is.na(deezer_id)    ~ "Wikidata spotify/deezer pair", 
                                 !is.na(deezer_id_wk) ~ "Wikidata mbid/deezer pair",
                                 !is.na(deezer_id_mb) ~ "Musicbrainz mbid / deezer pair",
                                 !is.na(deezer_id_p)  ~ "API search"))

  # Manque désormais plus que 126 artists
  # anti_join(artists_filtered, co) %>% select(artist_id, artist_name) %>% 
  #   arrange(artist_id) %>% 
  #   print(n=130)
  
  co <- co %>% 
    filter(!is.na(artist_id)) %>% 
    distinct(contact_id, artist_id, mbid)
  
  return(co)    
}

make_senscritique_ratings_data <- function(senscritique_mb_deezer_id){
  require(tidyverse)
  s3 <- initialize_s3()
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
  
  cora <- ratings %>%
    group_by(product_id) %>% 
    summarize(n = n(),
              mean = mean(rating),
              sd = sd(rating)) %>% 
    filter(n > 10) %>% 
    inner_join(contacts_albums_list) %>% 
    group_by(contact_id) %>% 
    summarise(mean = mean(mean), 
              max = max(mean),
              min = min(mean),
              n_albums = n(),
              mean_sd = mean(sd)) %>% 
    inner_join(select(senscritique_mb_deezer_id, contact_id, artist_id)) %>% 
    select(-contact_id)
  return(cora)
}

# Make genre data ------

make_genres_data <- function(source = "deezer_editorial_playlists", senscritique_mb_deezer_id){
  require(tidyverse)
  s3 <- initialize_s3()
  if(source == "deezer_editorial_playlists"){
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
    # Check coverage of genre definition
    # user_artist_peryear %>% 
    #   filter(!is.na(artist_id)) %>% 
    #   left_join(genres) %>% 
    #   mutate(na = is.na(genre)) %>% 
    #   group_by(na) %>% 
    #   summarise(n=sum(n_play))
    # With .4 threshold, 70% of plays; with .3, 79%
    
  } else if(source == "deezer_maingenre"){
    s3$download_file(Bucket = "scoavoux", 
                     Key = "records_w3/items/artists_data.snappy.parquet",
                     Filename = "data/artists_data.snappy.parquet")
    artists <- read_parquet("data/artists_data.snappy.parquet", col_select = 1:3)
    artists <- artists %>% 
      filter(!is.na(main_genre))
    genres <- artists %>% select(artist_id, genre = "main_genre")
    # TODO:Collapse
    
  } else if(source == "senscritique"){
    f <- s3$get_object(Bucket = "scoavoux", Key = "senscritique/albums_tags.csv")
    album_tags <- f$Body %>% rawToChar() %>% read_csv()
    f <- s3$get_object(Bucket = "scoavoux", Key = "senscritique/tags_meaning.csv")
    tags_meaning <- f$Body %>% rawToChar() %>% read_csv()
    f <- s3$get_object(Bucket = "scoavoux", Key = "senscritique/contacts_albums_link.csv")
    contacts_albums_list <- f$Body %>% rawToChar() %>% read_csv()
    rm(f)
    
    g <- left_join(album_tags, tags_meaning) %>% 
      left_join(select(contacts_albums_list, -contact_subtype_id)) %>% 
      left_join(select(senscritique_mb_deezer_id, -mbid)) %>% 
      filter(!is.na(artist_id)) %>% 
      count(artist_id, genre)
    g
    # TODO: collapse
    
  } else if(source == "musicbrainz_tags"){
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

make_genres_aliases <- function(.file = "data/genres.csv"){
  require(tidyverse)
  genres_aliases <- read_csv(.file)
  genres_aliases_char <- c(genres_aliases$streaminggenre, 
                           genres_aliases$streaminggenre)
  names(genres_aliases_char) <- c(genres_aliases$surveygenre,
                                  genres_aliases$question)
  return(genres_aliases_char)
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

test_join <- function(){
  require(tidyverse)
  require(arrow)
  s3 <- initialize_s3()

  s3$download_file(Bucket = "scoavoux", 
                   Key = "records_w3/items/artists_data.snappy.parquet",
                   Filename = "data/artists_data.snappy.parquet")
  artists <- read_parquet("data/artists_data.snappy.parquet", col_select = 1:3)
  artists <- artists %>% 
    filter(!is.na(main_genre))
  
  full_join()
  tar_load("endo_legitimacy")
  tar_load("exo_radio")
  tar_load("exo_senscritique")
  tar_load("genres")
  tar_load("artists_pop")
  
  artists_pop <- artists_pop %>% 
    filter(respondent_n_users > 20)
  # x <- inner_join(exo_senscritique, exo_radio) %>% 
  #   inner_join(endo_legitimacy) %>% 
  #   inner_join(genres)

  x <- select(exo_senscritique, artist_id) %>% mutate(senscritique = TRUE) %>% 
    full_join(select(exo_radio, artist_id) %>% mutate(radio = TRUE)) %>% 
    full_join(select(endo_legitimacy, artist_id) %>% mutate(endo_legitimacy = TRUE)) %>% 
    full_join(select(artists, artist_id) %>% mutate(genre = TRUE)) %>%
    full_join(select(artists_pop, artist_id) %>% mutate(pop_threshold = TRUE)) %>% 
    mutate(across(everything(), ~ifelse(is.na(.x), FALSE, .x)))
  count(x, senscritique, radio, endo_legitimacy, genre, pop_threshold) %>% 
    arrange(desc(n)) %>% 
    filter(pop_threshold, senscritique)
  
  
  
  # Using deezer genres, setting a low threshold for popularity (20) and
  # accepting artists with 0 radio plays => about 12500 artists
  a <- 
    artists %>% 
    select(artist_id = "deezer_id", name = "mbname", main_genre = "genre") %>% 
    # filter on pop threshold
    inner_join(filter(artists_pop, respondent_n_users >= 20) %>% 
                select(artist_id)) %>% 
    # Add senscritique score
    inner_join(select(exo_senscritique,
                     artist_id, 
                     mean_sc_score = mean,
                     max_sc_score  = max)) %>% 
    # Add endogenous legitimacy
    inner_join(endo_legitimacy) %>% 
    # add radio. Set to zero when not present
    left_join(exo_radio) %>% 
    mutate(across(starts_with("radio"), ~ifelse(is.na(.x), 0, .x)))
  janitor::tabyl(a, main_genre) %>% 
    arrange(n)
}
