# Make artists popularity data ------
# For each artist, compute their use in control group
# and in respondent group (criteria for selecting artists + assess
# the coverage of our sample)
make_artist_popularity_data <- function(user_artist_peryear, to_remove_file){
  library(tidyverse)
  library(tidytable)
  library(arrow)
  s3 <- initialize_s3()
  
  # separate between survey respondants and control group
  f <- s3$download_file(Bucket = "scoavoux", 
                        Key = "records_w3/RECORDS_hashed_user_group.parquet", 
                        Filename = "data/temp/RECORDS_hashed_user_group.parquet")
  us <- read_parquet("data/temp/RECORDS_hashed_user_group.parquet")
  rm(f)
  
  pop_control <- us %>% 
    filter(is_in_control_group) %>% 
    select(hashed_id) %>% 
    inner_join(user_artist_peryear) %>% 
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
    inner_join(user_artist_peryear) %>% 
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

# Make artist language data ------
# In order to debias radio plays
make_artist_language_data <- function(artist_language_file = "records_w3/artists_songs_languages.csv"){
  s3 <- initialize_s3()
  artist_language <- s3$get_object(Bucket = "scoavoux", Key = artist_language_file)$Body %>% 
    rawToChar() %>% 
    read_csv()
  al <- artist_language %>% 
    rename(artist_id = "art_id") %>% 
    arrange(artist_id, desc(nb_songs)) %>% 
    group_by(artist_id) %>% 
    slice(1) %>% 
    ungroup() %>% 
    rename(main_lang = "lang", nb_songs_mainlang = "nb_songs")
  return(al)
}

# Make artist area data ------
make_artist_area_data <- function(area_country_file,
                                  country_rank_file,
                                  mbid_area_file = "musicbrainz/mbid_area.csv",
                                  area_names_file = "musicbrainz/area_names.csv",
                                  area_types_file = "musicbrainz/area_types.csv",
                                  mbid_deezerid_file = "musicbrainz/mbid_deezerid.csv"){
  s3 <- initialize_s3()
  mbid_deezerid <- s3$get_object(Bucket = "scoavoux", Key = mbid_deezerid_file)$Body %>% 
    rawToChar() %>% 
    read_csv()
  mbid_area <- s3$get_object(Bucket = "scoavoux", Key = mbid_area_file)$Body %>% 
    rawToChar() %>% 
    read_csv()
  area_names <- s3$get_object(Bucket = "scoavoux", Key = area_names_file)$Body %>% 
    rawToChar() %>% 
    read_csv()
  area_types <- s3$get_object(Bucket = "scoavoux", Key = area_types_file)$Body %>% 
    rawToChar() %>% 
    read_csv()
  deezerid_area <- mbid_deezerid %>% 
    inner_join(mbid_area, by = c(mbid = "gid")) %>% 
    inner_join(area_names, by = c(area = "id")) %>% 
    inner_join(rename(area_types, type_name = "name"), by = c(type = "id")) %>% 
    select(-mbid, -type, -area) 
  
  # todo! complete this file. Use a LLM?
  area_country <- read_csv(area_country_file) %>% 
    filter(!is.na(country)) %>% 
    select(-n)
  
  deezerid_country <- deezerid_area %>% 
    left_join(area_country) %>% 
    mutate(name = ifelse(!is.na(country), country, name),
           type_name = ifelse(!is.na(country), "Country", type_name)) %>% 
    filter(type_name == "Country") %>% 
    select(artist_id, country = "name") %>% 
    distinct()
  
  ## Finally we need only a single country for each artist. To ease the process
  ## we rank countries, with France first, French speaking countries (Canada,
  ## Switzerland, Belgium) second, culturally dominant English speaking 
  ## countries third (US, UK, Australia, Ireland, New Zealand), the rest
  ## after. The idea is that if an artist has a link to France, they will
  ## be considered French.
  
  country_rank <- read_csv(country_rank_file) %>% 
    rename(country = "Country", rank = "Rank")
  
  deezerid_country <- deezerid_country %>% 
    left_join(country_rank) %>% 
    arrange(artist_id, rank) %>% 
    group_by(artist_id) %>% 
    slice(1) %>% 
    ungroup() %>% 
    select(-rank)
  return(deezerid_country)
}

# Make artists releases by year data ------
make_artist_releases_data <- function(senscritique_mb_deezer_id, genres){
  senscritique_mb_deezer_id <- senscritique_mb_deezer_id %>% 
    filter(!is.na(mbid), mbid != "") %>% 
    distinct(mbid, consolidated_artist_id) %>% 
    rename(artist_id = "consolidated_artist_id") %>% 
    arrange(artist_id) %>% 
    slice(1, .by = mbid)
  
  s3 <- initialize_s3()
  
  # Data about dates of release of albums
  release_group <- s3$get_object(Bucket = "scoavoux", Key = "musicbrainz/mbid_release_group.csv")$Body %>% 
      rawToChar() %>% 
      read_csv()
  release_group <- release_group %>% 
    right_join(senscritique_mb_deezer_id) %>% 
    filter(!is.na(first_release_date_year),
           first_release_date_year < 2026,
           first_release_date_year > 1900)
  
  # Data about dates artists stopped being active
  dates_active <- s3$get_object(Bucket = "scoavoux", Key = "/musicbrainz/mbid_artist_end_date.csv")$Body %>% 
    rawToChar() %>% 
    read_csv()
  
  # Some artists have several mbid but not all have a end date.
  # This leads to the Beatles having a missing end date sometimes
  # and some albums from 2020 considered as original Beatles.
  # so: we look for duplicates, when we find some, we attribute
  # the last know date to duplicated mbid and join this back with dates_active
  dates_active <- senscritique_mb_deezer_id %>% 
    add_count(artist_id) %>% 
    filter(n > 1) %>% 
    left_join(dates_active) %>% 
    arrange(artist_id, desc(end_date_year)) %>% 
    group_by(artist_id) %>% 
    mutate(end_date_year = ifelse(is.na(end_date_year), lag(end_date_year), end_date_year)) %>% 
    ungroup() %>% 
    filter(!is.na(end_date_year)) %>% 
    select(mbid, end_date_year) %>% 
    bind_rows(dates_active) %>% 
    distinct()
  
  # We need genres: remove end dates for classical (because composers die but
  # their music is still played)
  # We remove albums released after the group ceased collaborating
  release_group <- release_group %>% 
    left_join(dates_active) %>% 
    left_join(genres) %>% 
    mutate(end_date_year = ifelse(is.na(end_date_year) | genre == "classical", 9999, end_date_year)) %>% 
    filter(first_release_date_year < end_date_year + 2)
  
  # We remove unrelevant album types
  # We only keep missing secondary_type_name because they are mostly compilations,
  # remixes, etc.
  release_group <- release_group %>% 
    filter(primary_type_name %in% c("Album", "EP", "Single"),
           is.na(secondary_type_name))
  
  # We remove albums where artist is not in first position
  # might be too harsh but 90% of all albums
  release_group <- release_group %>% 
    filter(artist_position == 0)
  
  # Now single and EP count only if there were no album a given year;
  # We give them a weight of respectively .5 and .2 album
  release_group <- release_group %>% 
    group_by(artist_id, first_release_date_year) %>% 
    mutate(keep = ifelse(any(primary_type_name == "Album") & primary_type_name != "Album", FALSE, TRUE)) %>% 
    ungroup() %>% 
    filter(keep) %>% 
    mutate(weight = c("Single" = .2, "EP" = .5, "Album" = 1)[primary_type_name])

  # Now we compute no of albums per year
  rg_artist_year <- group_by(release_group, artist_id, first_release_date_year) %>% 
    summarize(n = sum(weight)) %>% 
    ungroup()

  # Finally, we compute various metrics about this
  # For now:
  
  # Year of last release: last time an artist was active
  year_last_release <- rg_artist_year %>% 
    arrange(artist_id, desc(first_release_date_year)) %>% 
    slice(1, .by = artist_id) %>% 
    select(artist_id, year_last_release = "first_release_date_year")
  
  # No of releases between 2010-2022 (press corpus)
  no_releases_press <- rg_artist_year %>% 
    filter(first_release_date_year > 2009, first_release_date_year < 2023) %>% 
    group_by(artist_id) %>% 
    summarise(n_releases_2010_2022 = sum(n)) %>% 
    ungroup()

  no_releases_radio <- rg_artist_year %>% 
    filter(first_release_date_year >= 2019, first_release_date_year <= 2023) %>% 
    group_by(artist_id) %>% 
    summarise(n_releases_2019_2023 = sum(n)) %>% 
    ungroup()
  
  # Total releases (just because)
  # also barycenter as weighted average of album release date
  # not computing for now, too expensive.
  no_releases_total <- rg_artist_year %>% 
    group_by(artist_id) %>% 
    summarise(n_releases_total = sum(n)) %>% 
#              career_barycenter = sum(n*year_last_release)/n_releases_total) %>% 
    ungroup()
  
  no_releases <- year_last_release %>% 
    full_join(no_releases_press) %>% 
    full_join(no_releases_radio) %>% 
    full_join(no_releases_total) %>% 
    mutate(across(starts_with("n_releases"), ~ifelse(is.na(.x), 0, .x)))
  
  return(no_releases)
}

# Make artists gender data -----
make_artist_gender <- function(){
  # 
  s3 <- initialize_s3()
  gender <- s3$get_object(Bucket = "scoavoux", Key = "musicbrainz/mbz_gender.csv")$Body %>% 
    read_csv() %>% 
    rename(mbid = "gid")
  mbid <- s3$get_object(Bucket = "scoavoux", Key = "musicbrainz/mbid_deezerid.csv")$Body %>% 
    read_csv()
  res <- inner_join(mbid, gender) %>% 
    select(artist_id, gender) %>% 
    slice(1, .by = artist_id)
  return(res)
}


# Make endogenenous legitimacy data ------
# For each artist, compute mean ISEI and share of audience with a 
# high education
make_endogenous_legitimacy_data <- function(user_artist_peryear, isei, survey_raw){
  library(tidyverse)
  library(tidytable)
  
  isei <- filter(isei, !is.na(isei))

  artist_mean_isei <- user_artist_peryear %>% 
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
  
  artist_share_higher_education <- user_artist_peryear %>% 
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
  # x <- bind_rows(albums_ratings, tracks_ratings) %>% 
  #   group_by(contact_id) %>% 
  #   summarise(senscritique_n_ratings = sum(n),
  #             senscritique_n_albums = sum(weight == 1),
  #             senscritique_n_tracks = sum(weight == track_weight)) %>% 
  #   arrange(desc(senscritique_n_ratings)) %>% 
  #   anti_join(ungroup(senscritique_mb_deezer_id) %>% select(contact_id))
  # x %>% write_csv("lots_of_ratings_not_in_deezer.csv")
  
  
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
    
  } else if(.source == "genres_from_deezer_albums"){
    genres <- read_csv("data/genres_from_deezer_albums.csv") %>% 
      mutate(genre = recode_vars(genre, .source))
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

compute_exo_radio <- function(senscritique_mb_deezer_id, radio_leg){
  require(tidyverse)
  require(tidytable)
  s3 <- initialize_s3()
  f <- s3$get_object(Bucket = "scoavoux", Key = "records_w3/radio/radio_plays_with_artist_id.csv")
  radio <- f$Body %>% rawToChar() %>% fread()
  radio <- radio %>% 
    left_join(select(senscritique_mb_deezer_id, artist_id, consolidated_artist_id)) %>% 
    mutate(consolidated_artist_id = ifelse(is.na(consolidated_artist_id), artist_id, consolidated_artist_id))
  radio <- radio %>% 
    select(-artist_id) %>% 
    rename(artist_id = "consolidated_artist_id")
  
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
## We define here what the sample will be
filter_artists <- function(artists_raw, n_isei_min, n_control_users_min){
  require(tidyverse)
  
  # Rules of inclusion/exclusion of artists
  ## We use to be very exclusive: only artists with exo variables 
  ## (have senscritique score, narrowe botteneck, have been searched in press,
  ## have genre, have 5 respondents with ISEI listening)
  ## Now more inclusive: we accept that one variable is missing
  artists <- artists_raw %>% 
    filter(!is.na(genre),
           #!is.na(senscritique_meanscore), # has score on senscritique
           !is.na(endo_isei_mean_pond),
           n_isei >= n_isei_min,
           control_n_users > n_control_users_min
           #!is.na(total_n_pqnt_texte) # has been searched in the press data
           #parse # has been looked up in press data
    )
  
  # correct genre for French rap musician labelled RAP/Hip Hop rather than
  # french rap
  artists <- artists %>% 
    mutate(ml = main_lang == "fr",
           co = country == "France",
           across(ml:co, ~ifelse(is.na(.x), FALSE, .x)),
           genre = ifelse(genre == "raphiphop" & (ml | co), "frenchrap", genre)) %>% 
    select(-co, -ml)

  artists <- artists %>% 
    # turn NA to 0 in radio plays
    # mutate(across(starts_with("radio"), ~if_else(is.na(.x), 0, .x))) %>% 
    # Scaling legitimacy variables
    mutate(sc_endo_isei = center_scale(endo_isei_mean_pond),
           sc_endo_educ = center_scale(endo_share_high_education_pond),
           sc_exo_press = center_scale(log(press_leg_residuals+1)),
           sc_exo_score = center_scale(senscritique_meanscore),
           sc_exo_radio = center_scale(log(radio_leg_resid+1))
    ) %>% 
    rename(leg_endo_isei = "endo_isei_mean_pond",
           leg_endo_educ = "endo_share_high_education_pond",
           leg_exo_press = "total_n_pqnt_texte",
           leg_exo_score = "senscritique_meanscore",
           leg_exo_radio = "radio_leg")

  # Add PCA
  # only on full dataset (with all variables from exo)
  pcad <- filter(artists, !is.na(leg_exo_press), !is.na(leg_exo_score), !is.na(leg_exo_radio))
  pcar <- compute_pca(pcad)
  pcad <- pcad %>% 
    mutate(sc_exo_pca = center_scale(pcar$ind$coord[,1]))
  
  artists <- artists %>% 
    left_join(select(pcad, artist_id, sc_exo_pca))
  
  return(artists)
}

# Residualizing press and radio counts ------

fit_zinflnb_press <- function(exo_press, artists_pop, artists_language, artists_country, artist_releases){
  #todo
  #fit le model avec bonnes variables
  # comparer les rangs variable residualisée vs. variable brute
  require(pscl)
  d <- exo_press %>% 
    left_join(artists_pop) %>% 
    left_join(artists_language) %>% 
    left_join(artists_country) %>% 
    left_join(artist_releases) %>% 
    mutate(lng = case_when(country == "France" | main_lang == "fr" ~ "fr",
                           country %in% c("United States", "United Kingdom", 
                                          "Australia", "Canada", "Ireland")| main_lang == "en" ~ "en",
                           !is.na(country) | !is.na(main_lang) ~ "other",
                           TRUE ~ NA),
           age_last_release = 2023 - year_last_release) %>% 
    select(artist_id, total_n_pqnt_texte, control_n_users, lng, age_last_release, n_releases_2010_2022) %>% 
    na.omit()
  
  press_fit <- zeroinfl(total_n_pqnt_texte ~ lng + control_n_users + age_last_release + n_releases_2010_2022, data = d, dist = "negbin")
  d <- mutate(d, press_leg_residuals = residuals(press_fit, "pearson"))
  res <- list(model = press_fit,
              residuals = select(d, artist_id, press_leg_residuals))
  return(res)
}

fit_zinflnb_radio <- function(exo_radio, artists_pop, artists_language, artist_releases){
  require(pscl)
  d <- exo_radio %>% 
    left_join(select(artists_pop, artist_id, control_n_users)) %>% 
    left_join(artists_language) %>% 
    left_join(artist_releases) %>% 
    na.omit() %>% 
    mutate(lng = ifelse(main_lang == "fr", "fr", "other"),
           age_last_release = 2023 - year_last_release)
  radio_fit <- zeroinfl(radio_leg ~ lng + control_n_users + age_last_release + n_releases_2019_2023, data = d, dist = "negbin")
  d$radio_leg_resid <- 
    residuals(radio_fit, "pearson")
  res <- list(model = radio_fit, 
       residuals = select(d, artist_id, radio_leg_resid))
  return(res)
}
