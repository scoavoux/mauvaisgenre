# Compute omnivorism by user ------
compute_omnivorourness_from_survey <- function(survey, genres_aliases){
  # Compute from genres liked and consumed.
  # TODO: cultural holes
  require(tidyverse)

  omni_survey_sum_genres_played <- survey %>%
    select(hashed_id, matches("B_genres_\\d+")) %>%
    pivot_longer(-hashed_id) %>%
    # rename genres
    mutate(value = genres_aliases[value]) %>% 
    filter(value != "", !is.na(value)) %>%
    distinct(hashed_id, value) %>% 
    mutate(name = 1) %>%
    summarise(omni_survey_sum_genres_played = sum(name), .by = hashed_id)

  # Recoding data on liking genres
  omni_survey_sum_genres_liked <- make_genre_preference_data() %>%
    # Consolidate genres to agree between survey and streaming
    mutate(genre = genres_aliases[genre]) %>% 
    filter(genre != "", !is.na(genre)) %>%
    # only loved and liked genres
    filter(group %in% c("0", "1")) %>%
    summarize(omni_survey_sum_genres_liked = n(), .by = hashed_id)

  
  
  omni <- omni_survey_sum_genres_played %>%
    full_join(omni_survey_sum_genres_liked)
  return(omni)
}

compute_latent_classes_from_survey <- function(survey, genres_aliases, nclass){
  require(tidyverse)

  genre_matrix <- survey %>%
    select(hashed_id, matches("B_genres_\\d+")) %>%
    pivot_longer(-hashed_id) %>%
    # consolidate genres
    mutate(value = genres_aliases[value]) %>% 
    filter(value != "", !is.na(value)) %>%
    distinct(hashed_id, value) %>% 
    mutate(name = 2) %>% 
    pivot_wider(names_from = value, values_from = name, values_fill = 1)

  # needs manual input when genres change because poLCA formula are really not 
  # standard and don't allow easily passing arguments
  genres <- names(genre_matrix)[-1]

  form <- cbind(frenchrap, pop, rock, electro, jazz, rnb, soulfunk, raphiphop, frenchsongs, classical, reggae, metal, alternative, folk, latino, blues, country, dance, african)~1
  mod <- vector("list", length(nclass))
  names(mod) <- paste0("k", nclass)
  for(i in nclass){
    mod[[paste0("k", i)]] <- poLCA::poLCA(form, genre_matrix, nclass = i)
  }
  return(mod)
}

compute_latent_classes_from_streams <- function(user_artist_peryear, genres, nclass, proportion=FALSE){
  require(mclust)
  require(tidyverse)
  require(tidytable)
  # require(conflicted)
  # conflict_prefer("ungroup", "tidytable")
  # conflict_prefer("group_by", "tidytable")
  # conflict_prefer("summarize", "tidytable")
  # conflict_prefer("filter", "tidytable")
  # conflicts_prefer(tidytable::pivot_wider)
  
  # summarize across years
  user_artist_peryear <- user_artist_peryear %>% 
    group_by(hashed_id, artist_id) %>% 
    summarize(l_play = sum(l_play)) %>% 
    ungroup() %>% 
    filter(l_play > 0)
  
  user_genre <- user_artist_peryear %>% 
    left_join(genres) %>% 
    filter(!is.na(genre)) %>% 
    group_by(hashed_id, genre) %>% 
    summarize(l_play = sum(l_play))

  if(proportion){
    user_genre <- user_genre %>% 
      group_by(hashed_id) %>% 
      mutate(f_play = l_play / sum(l_play)) %>% 
      select(-l_play) %>% 
      rename(l_play = "f_play")
  }
  
  user_genre_matrix <- user_genre %>% 
    ungroup() %>% 
    pivot_wider(names_from = genre, values_from = l_play, values_fill = 0)
  
  mod <- vector("list", length = length(nclass))
  names(mod) <- paste0("k", nclass)
  for(i in nclass){
    mod[[paste0("k", i)]] <- Mclust(select(user_genre_matrix, -hashed_id), G = i)
  }
  return(mod)
}

select_latent_class_model <- function(models_list, nclass){
  return(models_list[[paste0("k", nclass)]])
}

compute_omnivorourness_from_streams <- function(user_artist_peryear, artists_filtered, genres){
  require(tidyverse)
  require(tidytable)
  
  user_artist_peryear <- user_artist_peryear %>% 
    filter(!is.na(artist_id)) %>% 
    group_by(hashed_id, artist_id) %>% 
    # we use number of play
    summarize(l_play = sum(l_play)) %>% 
    ungroup() %>% 
    left_join(genres)
  
  ## diversity over genres actually listened
  ### HHI over stream by genre
  omni <- user_artist_peryear %>% 
    filter(!is.na(genre)) %>% 
    group_by(hashed_id, genre) %>% 
    summarize(l_play = sum(l_play)) %>% 
    group_by(hashed_id) %>% 
    mutate(f_play = l_play / sum(l_play)) %>% 
    summarize(omni_stream_genres_hhi = 1 - sum(f_play^2))
  
  ## diversity over individual legitimacy
  ### weighted mean and sd of each artist's average legitimacy
  ### as highbrow dim (mean) and omnivorous dim (sd)
  omni <- user_artist_peryear %>% 
    group_by(hashed_id) %>% 
    mutate(f_play = l_play / sum(l_play)) %>% 
    left_join(select(artists_filtered, artist_id, sc_exo_pca)) %>% 
    filter(!is.na(sc_exo_pca)) %>% 
    group_by(hashed_id) %>% 
    summarize(mean_exo_pca = sum(sc_exo_pca*f_play), 
              sd_exo_pca   = sqrt(sum((f_play*(sc_exo_pca - mean(sc_exo_pca)))^2))
              ) %>% 
    full_join(omni)
  
  ## Same by genre
  omni <- user_artist_peryear %>% 
    group_by(hashed_id) %>% 
    mutate(f_play = l_play / sum(l_play)) %>% 
    left_join(select(artists_filtered, artist_id, sc_exo_pca)) %>% 
    filter(!is.na(sc_exo_pca), !is.na(genre)) %>% 
    group_by(hashed_id, genre) %>% 
    summarize(n=sum(l_play),
              mean_exo_pca = sum(sc_exo_pca*f_play), 
              sd_exo_pca   = sqrt(sum((f_play*(sc_exo_pca - mean(sc_exo_pca)))^2))
    ) %>% 
    # one needs to actually have listened to that genre
    filter(n>100) %>% 
    pivot_longer(ends_with("exo_pca")) %>% 
    mutate(name = paste(name, genre, sep="_")) %>% 
    select(-genre, -n) %>% 
    pivot_wider(names_from = name, values_from=value) %>% 
    full_join(omni)
  return(omni)
}

## Peterson: sum of genres declared
### in survey: sum over genres consumed
### in survey: sum over genres liked


## diversity over mean legitimacy of genres
### let us attribute to each artist the average legitimacy of their genre
### weighted mean and sd of this,
### as highbrow dim (mean) and omnivorous dim (sd)


