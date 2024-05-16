# Compute omnivorism by user ------
compute_omnivorourness_from_survey <- function(){
  # Compute from genres liked and consumed.
  # TODO: cultural holes
  require(tidyverse)
  s3 <- initialize_s3()
  f <- s3$get_object(Bucket = "scoavoux", Key = "records_w3/survey/RECORDS_Wave3_apr_june_23_responses_corrected.csv")
  survey <- f$Body %>% rawToChar() %>% data.table::fread() %>% tibble()

  omni_survey_sum_genres_played <- survey %>%
    select(hashed_id, matches("B_genres_\\d+")) %>%
    pivot_longer(-hashed_id) %>%
    filter(value != "") %>%
    mutate(name = 1) %>%
    summarise(omni_survey_sum_genres_played = sum(name), .by = hashed_id)

  # Recoding data on liking genres
  omni_survey_sum_genres_liked <- make_genre_preference_data() %>%
    # only loved and liked genres
    filter(group %in% c("0", "1")) %>%
    summarize(omni_survey_sum_genres_liked = n(), .by = hashed_id)

  omni <- omni_survey_sum_genres_played %>%
    full_join(omni_survey_sum_genres_liked)
  return(omni)
}

compute_latent_classes_from_survey <- function(nclass){
  require(tidyverse)
  require(poLCA)
  require(conflicted)
  conflict_prefer("select", "dplyr")
  conflict_prefer("filter", "dplyr")
  s3 <- initialize_s3()
  f <- s3$get_object(Bucket = "scoavoux", Key = "records_w3/survey/RECORDS_Wave3_apr_june_23_responses_corrected.csv")
  survey <- f$Body %>% rawToChar() %>% data.table::fread() %>% tibble()
  genre_matrix <- survey %>%
    select(hashed_id, matches("B_genres_\\d+")) %>%
    pivot_longer(-hashed_id) %>%
    filter(value != "") %>%
    mutate(name = 2) %>% 
    pivot_wider(names_from = value, values_from = name, values_fill = 1)
  genres <- names(genre_matrix)[-1]
  paste0("`", genres, "`", collapse=",")[1]
  form <- cbind(`Rap français`,`RnB`,`Hip Hop / Rap`,`Pop`,`Variété française`,`Musique de Film`,`Musique Latino`,`Jazz`,`Musique Classique`,`Indé / Alternatif`,`Rock`,`Electro`,`Musiques Africaines`,`Reggae`,`Folk & Acoustique`,`Soul / Funk`,`Musique Brésilienne`,`Musiques Asiatiques`,`Country`,`Métal`,`Musiques Arabes`,`Dance & EDM`,`K-pop`,`Blues`)~1
  mod <- vector("list", length(nclass))
  names(mod) <- paste0("k", nclass)
  for(i in nclass){
    mod[[paste0("k", i)]] <- poLCA(form, genre_matrix, nclass = i)
  }
  return(mod)
}

compute_latent_classes_from_streams <- function(user_artist_peryear, genres, nclass){
  require(mclust)
  require(tidyverse)
  require(tidytable)

  user_artist_peryear <- user_artist_peryear %>% 
    group_by(hashed_id, artist_id) %>% 
    # we use number of play
    summarize(n_play = sum(n_play)) %>% 
    ungroup()
  user_genre_matrix <- user_artist_peryear %>% 
    left_join(genres) %>% 
    filter(!is.na(genre)) %>% 
    group_by(hashed_id, genre) %>% 
    summarize(n_play = sum(n_play)) %>% 
    pivot_wider(names_from = genre, values_from = n_play, values_fill = 0)
  
  mod <- vector("list", length = length(nclass))
  names(mod) <- paste0("k", nclass)
  for(i in nclass){
    mod[[paste0("k", i)]] <- Mclust(select(user_genre_matrix, -hashed_id), G = i)
  }
  return(mod)
}


## Peterson: sum of genres declared
### in survey: sum over genres consumed
### in survey: sum over genres liked


## diversity over mean legitimacy of genres
### let us attribute to each artist the average legitimacy of their genre
### weighted mean and sd of this,
### as highbrow dim (mean) and omnivorous dim (sd)


