# Compute omnivorism by user ------
compute_omnivorourness_from_survey <- function(survey){
  # Compute from genres liked and consumed.
  # TODO: cultural holes
  require(tidyverse)

  person_genre_survey <- survey %>%
    select(hashed_id, matches("B_genres_\\d+")) %>%
    pivot_longer(-hashed_id) %>% 
    mutate(value = recode_vars(value, "survey_question")) %>% 
    filter(value != "", !is.na(value)) %>% 
    distinct(hashed_id, value) %>% 
    select(hashed_id, genre = "value")
  
  omni_survey_sum_genres_played <- person_genre_survey %>%
    mutate(name = 1) %>%
    summarise(omni_survey_sum_genres_played = sum(name), .by = hashed_id)

  person_genre_liked_survey <- make_genre_preference_data(survey) %>%
    # Consolidate genres to agree between survey and streaming
    mutate(genre = recode_vars(genre, "survey_questionno")) %>% 
    filter(genre != "", !is.na(genre)) %>%
    # only loved and liked genres
    filter(group %in% c("0", "1")) %>% 
    select(hashed_id, genre)
  
  omni_survey_sum_genres_liked <- person_genre_liked_survey %>%
    summarize(omni_survey_sum_genres_liked = n(), .by = hashed_id)

  # Cultural Holes (Lizardo)
  ## Compute genre similarity matrix
  ## => pairwise genre distance.
  #o_{jk} = \frac{c_{jk}}{min(c_{jj}, c_{kk})}
  ## Then for each person-month:
  ## $$OV_i = \sum_j{a_ij}$$
  ## $$EO_i = \sum_{j \in N(i)}(a_{ij} - (\frac{1}{OV_i-1}\sum_{k \in N(i)}^{k \neq j}{o_{jk}}))$$
  
  make_genre_proximity_matrix <- function(person_genre_survey){
    genre_proximity_matrix <- person_genre_survey %>% 
      full_join(rename(person_genre_survey, genre1 = "genre"), 
                by="hashed_id", 
                relationship = "many-to-many") %>% 
      count(genre, genre1)
    
    genre_max <- genre_proximity_matrix %>% 
      filter(genre == genre1) %>% 
      select(-genre1, max=n)
    
    genre_proximity_matrix <- genre_proximity_matrix %>% 
      left_join(rename(genre_max, genre1 = "genre", max1 = "max")) %>% 
      left_join(genre_max) %>% 
      mutate(min = if_else(max < max1, max, max1),
             f = n / min) %>% 
      select(genre, genre1, f)
    
    return(genre_proximity_matrix)
  }
  
  compute_cultural_hole_omnivorousness <- function(genre, genre_proximity_matrix){
    if(length(genre) > 1){
      OV_i <- 1 / (n_distinct(genre)-1)
      r <- tibble(genre = genre) %>% 
        left_join(filter(genre_proximity_matrix, genre1 %in% genre)) %>% 
        filter(genre != genre1) %>% 
        group_by(genre) %>% 
        summarize(d = sum(f)*OV_i) %>% 
        summarise(r = sum(1-d)) %>% 
        pull(r)
    } else {
      r <- 0
    }
    return(r)
  }
  
  compute_cultural_hole_omnivorousness <- function(person_genre_survey, genre_proximity_matrix, 
                                                   .name = "cultural_hole_omnivorousness"){
    univores <- count(person_genre_survey, hashed_id, name = "cho") %>% 
      filter(cho == 1)
    
    pgs <- person_genre_survey %>% 
      filter(!(hashed_id %in% univores$hashed_id)) %>% 
      left_join(genre_proximity_matrix, relationship = "many-to-many") %>% 
      filter(genre1 %in% genre, genre != genre1) %>% 
      group_by(hashed_id, genre) %>% 
      summarize(d = (1/n())*sum(f)) %>% 
      summarize(cho = sum(1-d))
    
    pgs <- bind_rows(pgs, univores)
    names(pgs) <- c("hashed_id", .name)
    return(pgs)
  }
  
  genre_proximity_matrix_played <- make_genre_proximity_matrix(person_genre_survey)
  omni_survey_culturalholes_played <- compute_cultural_hole_omnivorousness(person_genre_survey,
                                                                           genre_proximity_matrix_played,
                                                                    "omni_survey_cultural_holes_played")
  genre_proximity_matrix_liked <- make_genre_proximity_matrix(person_genre_liked_survey)
  omni_survey_culturalholes_liked <- compute_cultural_hole_omnivorousness(person_genre_liked_survey,
                                                                          genre_proximity_matrix_liked,
                                                                    "omni_survey_cultural_holes_liked")
  
  omni <- omni_survey_sum_genres_played %>%
    full_join(omni_survey_sum_genres_liked) %>%
    full_join(omni_survey_culturalholes_played) %>% 
    full_join(omni_survey_culturalholes_liked)
  return(omni)
}

# Make latent classes from survey ------
compute_latent_classes_from_survey <- function(survey, nclass){
  require(tidyverse)

  genre_matrix <- survey %>%
    select(hashed_id, matches("B_genres_\\d+")) %>%
    pivot_longer(-hashed_id) %>%
    # consolidate genres
    mutate(value = recode_vars(value, "survey_question")) %>% 
    filter(value != "", !is.na(value)) %>%
    distinct(hashed_id, value) %>% 
    mutate(name = 2) %>% 
    pivot_wider(names_from = value, values_from = name, values_fill = 1)

  # needs manual input when genres change because poLCA formula are really not 
  # standard and don't allow easily passing arguments
  genres <- names(genre_matrix)[-1]

  form <- cbind(frenchrap, pop, rock, edm, jazz, rnb, soulfunk, raphiphop, frenchsongs, classical, reggae, metal, alternative, folk, latino, blues, country, african)~1
  mod <- vector("list", length(nclass))
  names(mod) <- paste0("k", nclass)
  for(i in nclass){
    mod[[paste0("k", i)]] <- poLCA::poLCA(form, genre_matrix, nclass = i)
    # Pass the ids so that we can pair the data afterwards
    mod[[paste0("k", i)]]$id <- genre_matrix$hashed_id
  }
  return(mod)
}

compute_latent_classes_from_streams <- function(user_artist_peryear_merged_artists, genres, nclass, proportion=FALSE){
  require(mclust)
  require(tidyverse)
  require(tidytable)

  # summarize across years
  user_artist_peryear_merged_artists <- user_artist_peryear_merged_artists %>% 
    group_by(hashed_id, artist_id) %>% 
    summarize(l_play = sum(l_play)) %>% 
    ungroup() %>% 
    filter(l_play > 0)
  
  user_genre <- user_artist_peryear_merged_artists %>% 
    left_join(genres) %>% 
    filter(!is.na(genre)) %>% 
    group_by(hashed_id, genre) %>% 
    summarize(l_play = sum(l_play)) %>% 
    # express time in hours
    mutate(l_play = l_play/3600)

  if(proportion){
    user_genre <- user_genre %>% 
      group_by(hashed_id) %>% 
      mutate(f_play = (l_play / sum(l_play)) * 100) %>% 
      # hard solution to avoid svd errors caused by figures being too close to
      # zero => we remove genres with less than 1% in total consumption.
      filter(f_play > 1) %>% 
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
    # pass ids to be able to pair the values afterwards
    mod[[paste0("k", i)]]$id <- user_genre_matrix$hashed_id
    print(i)
  }
  names(mod) <- paste0("k", nclass)
  
  return(mod)
}

select_latent_class_model <- function(models_list, nclass){
  return(models_list[[paste0("k", nclass)]])
}

compute_omnivorourness_from_streams <- function(user_artist_peryear_merged_artists, artists, genres, rescale_by = "artist"){
  require(tidyverse)
  require(tidytable)
  
  user_artist_peryear_merged_artists <- user_artist_peryear_merged_artists %>% 
    filter(!is.na(artist_id)) %>% 
    group_by(hashed_id, artist_id) %>% 
    # we use number of play
    summarize(l_play = sum(l_play)) %>% 
    ungroup() %>% 
    left_join(genres)
  
  ## diversity over genres actually listened
  ### HHI over stream by genre
  omni_HHI <- user_artist_peryear_merged_artists %>% 
    filter(!is.na(genre)) %>% 
    group_by(hashed_id, genre) %>% 
    summarize(l_play = sum(l_play)) %>% 
    group_by(hashed_id) %>% 
    mutate(f_play = l_play / sum(l_play)) %>% 
    summarize(omni_stream_genres_hhi = 1 - sum(f_play^2))
  
  ## diversity over individual legitimacy
  ### weighted mean and sd of each artist's average legitimacy
  ### as highbrow dim (mean) and omnivorous dim (sd)
  tmp <- user_artist_peryear_merged_artists %>% 
    group_by(hashed_id) %>% 
    mutate(f_play = l_play / sum(l_play)) %>% 
    left_join(select(artists, artist_id, starts_with("sc_exo_")))
  omni_exo <- tmp %>% 
    group_by(hashed_id) %>% 
    summarise(across(starts_with("sc_exo_"), ~sum(.x*f_play, na.rm=TRUE), .names = "mean_{.col}"))

  omni_exo <- tmp %>% 
    left_join(omni_exo) %>% 
    group_by(hashed_id) %>% 
    summarise(sd_sc_exo_radio = sqrt(sum((f_play*(sc_exo_radio - first(mean_sc_exo_radio)))^2, na.rm = TRUE)),
              sd_sc_exo_press = sqrt(sum((f_play*(sc_exo_press - first(mean_sc_exo_press)))^2, na.rm = TRUE)),
              sd_sc_exo_score = sqrt(sum((f_play*(sc_exo_score - first(mean_sc_exo_score)))^2, na.rm = TRUE)),
              sd_sc_exo_pca   = sqrt(sum((f_play*(sc_exo_pca   - first(mean_sc_exo_pca  )))^2, na.rm = TRUE))) %>% 
    full_join(omni_exo)
  
  ## TODO: ADAPT FOR ALL MEASURES AND NOT ONLY PCA + ADAPT TO HAVE PROPER WEIGHTED MEAN SUBSTRACYED IN SD MEASURE 
  ## Same by genre
  if(rescale_by == "artist"){
    ## Start by rescaling legitimacy by genre
    artists <- artists %>%
      group_by(genre) %>%
      mutate(sc_exo_pca_scbygenre = (sc_exo_pca-mean(sc_exo_pca))/sd(sc_exo_pca))
    omni_exo_bygenre <- user_artist_peryear_merged_artists %>%
      group_by(hashed_id) %>%
      mutate(f_play = l_play / sum(l_play)) %>%
      # We must first rescale by genre
      left_join(select(artists, artist_id, sc_exo_pca_scbygenre)) %>%
      filter(!is.na(sc_exo_pca_scbygenre), !is.na(genre)) %>%
      group_by(hashed_id, genre) %>%
      summarize(n=sum(l_play),
                mean_exo_pca = sum(sc_exo_pca_scbygenre*f_play),
                sd_exo_pca   = sqrt(sum((f_play*(sc_exo_pca_scbygenre - mean(sc_exo_pca_scbygenre)))^2))
      ) %>%
      # one needs to actually have listened to that genre
      filter(n>100) %>%
      pivot_longer(ends_with("exo_pca")) %>%
      mutate(name = paste(name, genre, sep="_")) %>%
      select(-genre, -n) %>%
      pivot_wider(names_from = name, values_from=value)
  } else if(rescale_by == "user"){
    # or rescale among individuals  
    omni_exo_bygenre <- user_artist_peryear_merged_artists %>% 
      group_by(hashed_id) %>% 
      mutate(f_play = l_play / sum(l_play)) %>% 
      # We must first rescale by genre
      left_join(select(artists, artist_id, sc_exo_pca)) %>% 
      filter(!is.na(sc_exo_pca), !is.na(genre)) %>% 
      group_by(genre) %>% 
      mutate(sc_exo_pca = (sc_exo_pca-mean(sc_exo_pca))/sd(sc_exo_pca)) %>% 
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
      pivot_wider(names_from = name, values_from=value)
  }

  
    
  omni <- omni_HHI %>% 
    full_join(omni_exo) %>% 
    full_join(omni_exo_bygenre)
  return(omni)
}

## Peterson: sum of genres declared
### in survey: sum over genres consumed
### in survey: sum over genres liked


## diversity over mean legitimacy of genres
### let us attribute to each artist the average legitimacy of their genre
### weighted mean and sd of this,
### as highbrow dim (mean) and omnivorous dim (sd)

# Put everything together
recode_survey_data <- function(survey, 
                               omni_from_survey, 
                               omni_from_streams,
                               latent_classes_from_surveys){
                               # 
                               # latent_classes_from_streams,
                               # latent_classes_from_streams_proportion){
  
  # Recode survey questions  
  survey <- survey %>% 
    mutate(age=2023-E_birth_year,
           gender = factor(E_gender, levels = c("Un homme", "Une femme"), labels = c("Men", "Women")),
           degree = ifelse(E_diploma == "", NA, E_diploma) %>% fct_collapse(low = c(
             "CEP (certificat d'études primaires)",
             "DEUG, BTS, DUT, DEUST, diplôme des professions sociales ou de la santé, d'infirmier.ère",
             "CAP, BEP, brevet de compagnon",       
             "Aucun diplôme",
             "BEPC, brevet élementaire, brevet des collèges"
           ),
           
           middle = c("Bac général, brevet supérieur",
                      "Bac pro ou techno, brevet professionnel ou de technicien, BEA, BEC, BEI, BEH, capacité en droit"
           ),
           high = c(
             "Licence, licence pro, maîtrise, BUT",
             "Master, diplôme d'ingénieur.e, DEA, DESS",
             "Doctorat (y compris médecine, pharmacie, dentaire), HDR"
           )
           ))
  # Let us select only variables of interest...
  
  
  # Extract clusters
  lcs <- tibble(cluster_survey = latent_classes_from_surveys$predclass, 
                hashed_id = latent_classes_from_surveys$id)
  # lcst <- tibble(cluster_streams = latent_classes_from_streams$classification,
  #                hashed_id = latent_classes_from_streams$id)
  # lcstp <- tibble(cluster_streamsprop = latent_classes_from_streams_proportion$classification,
  #                 hashed_id = latent_classes_from_streams_proportion$id)
  
  # Now let's aggregate with other datasets
  survey <- survey %>% 
    left_join(omni_from_survey) %>% 
    left_join(omni_from_streams) %>% 
    left_join(lcs)
    # left_join(lcst) %>% 
    # left_join(lcstp)
  
  return(survey)
}


