make_radio_data <- function(senscritique_mb_deezer_id){
  s3 <- initialize_s3()
  f <- s3$get_object(Bucket = "scoavoux", Key = "records_w3/radio/radio_plays_with_artist_id.csv")
  radio <- f$Body %>% rawToChar() %>% tidytable::fread()
  radio <- radio %>% 
    left_join(select(senscritique_mb_deezer_id, artist_id, consolidated_artist_id)) %>% 
    mutate(consolidated_artist_id = ifelse(is.na(consolidated_artist_id), artist_id, consolidated_artist_id))
  radio <- radio %>% 
    select(-artist_id) %>% 
    rename(artist_id = "consolidated_artist_id")
  radio <- radio %>%
    filter(!is.na(artist_id)) %>% 
    count(radio, artist_id)
  return(radio)
}

plot_robustness_radio_genres <- function(radio_artist, genres){
  require(tidyverse)
  set_ggplot_options()

  genres_by_radio <- radio_artist %>% 
    inner_join(genres) %>% 
    filter(!is.na(genre)) %>% 
    group_by(radio) %>% 
    mutate(f = n / sum(n)) %>% 
    group_by(radio, genre) %>% 
    summarize(m = sum(f))
  genres_order <- radio_artist %>% 
    inner_join(genres) %>% 
    group_by(genre) %>% 
    summarize(n = sum(n)) %>% 
    arrange(desc(n)) %>% 
    pull(genre)
  gg <- genres_by_radio %>% 
    mutate(genre = factor(genre, levels = genres_order)) %>% 
    ggplot(aes(radio, genre, fill = m)) +
      geom_tile() +
      scale_x_discrete(position = "top") +
      scale_fill_distiller(direction = 1) +
      theme(axis.text.x = element_text(angle = 60, hjust = 0)) +
      labs(x = "Radio stations", y = "Genres", fill = "Rate")
  filename <- str_glue("output/omni1/gg_robustness_genre_radio.pdf")
  ggsave(filename, gg, device = "pdf")
  return(filename)
}

plot_robustness_radio_score <- function(radio_artist, exo_senscritique, genres){
  require(tidyverse)
  set_ggplot_options()
  exo_score_by_radio <- radio_artist %>% 
    inner_join(select(exo_senscritique, artist_id, senscritique_meanscore)) %>% 
    group_by(radio) %>% 
    mutate(f = n / sum(n)) %>% 
    summarize(m = sum(senscritique_meanscore * f))
  
  gg <- exo_score_by_radio %>% 
    arrange(desc(m)) %>% 
    mutate(radio = factor(radio, radio)) %>% 
    ggplot(aes(m, radio)) +
    geom_col() +
    labs(x = "Radio stations", y = "Genres", fill = "Rate")
  filename <- str_glue("output/omni1/gg_robustness_radio_score.pdf")
  ggsave(filename, gg, device = "pdf", height = 10)
  return(filename)
}

plot_robustness_radio_score <- function(radio_artist, exo_senscritique){
  require(tidyverse)
  set_ggplot_options()
  exo_score_by_radio <- radio_artist %>% 
    inner_join(select(exo_senscritique, artist_id, senscritique_meanscore)) %>% 
    group_by(radio) %>% 
    mutate(f = n / sum(n)) %>% 
    summarize(m = sum(senscritique_meanscore * f))
  
  gg <- exo_score_by_radio %>% 
    arrange(desc(m)) %>% 
    mutate(radio = factor(radio, radio)) %>% 
    ggplot(aes(m, radio)) +
    geom_col() +
    labs(x = "Average maven score", y = "Radio stations")
  filename <- str_glue("output/omni1/gg_robustness_radio_score.pdf")
  ggsave(filename, gg, device = "pdf", height = 10)
  return(filename)
}

plot_robustness_radio_genre_score <- function(radio_artist, exo_senscritique, genres){
  require(tidyverse)
  set_ggplot_options()
  exo_score_by_radio <- radio_artist %>% 
    inner_join(genres) %>% 
    filter(!is.na(genre)) %>% 
    inner_join(select(exo_senscritique, artist_id, senscritique_meanscore)) %>% 
    group_by(radio, genre) %>% 
    mutate(f = n / sum(n)) %>% 
    summarize(m = sum(senscritique_meanscore * f))
  genres_order <- radio_artist %>% 
    inner_join(genres) %>% 
    group_by(genre) %>% 
    summarize(n = sum(n)) %>% 
    arrange(desc(n)) %>% 
    pull(genre)
  
  gg <- exo_score_by_radio %>% 
    arrange(desc(m)) %>% 
    mutate(radio = factor(radio, unique(radio)),
           genre = factor(genre, genres_order)) %>% 
    ggplot(aes(radio, genre, fill = m)) +
      geom_tile() +
      scale_x_discrete(position = "top") +
      scale_fill_distiller(direction = 1) +
      theme(axis.text.x = element_text(angle = 60, hjust = 0)) +
      labs(x = "Radio stations", y = "Genres", fill = "Maven score")
  filename <- str_glue("output/omni1/gg_robustness_radio_genre_score.pdf")
  ggsave(filename, gg, device = "pdf")
  return(filename)
}

tbl_top_artists_by_radio <- function(radio_artist, artist_names, genres){
  require(tidyverse)
  set_ggplot_options()
  excl <- radio_artist %>% 
    group_by(artist_id) %>% 
    mutate(exclusivity = n / sum(n))
  tb <- radio_artist %>% 
    arrange(radio, desc(n)) %>% 
    slice(1:50, .by = radio) %>% 
    left_join(artist_names) %>% 
    left_join(genres) %>% 
    left_join(excl)
  
  filename <- str_glue("output/omni1/tb_robustness_radio_artist_genre.csv")
  write_csv(tb, filename)
  return(filename)
}


tbl_robustness_top_artist_by_radioleg <- function(radio_artist, artist_names, genres){
  leg_radio_lists <- list(
    fip = c("Fip"),
    public = c("France Musique", "France Inter", "Fip"),
    public_mouv = c("France Musique", "France Inter", "Fip", "Mouv'"),
    public_plus_nova = c("France Musique", "France Inter", "Fip", "Radio Nova"),
    public_plus_classique = c("France Musique", "Radio Classique", 
                              "France Inter", "Fip"),
    public_plus_classique_nova = c("France Musique", "Radio Classique", 
                              "France Inter", "Fip", "Radio Nova"),
    public_plus_classique_jazz = c("France Musique", "Radio Classique", 
                                   "France Inter", "Fip", 
                                   "Jazz Radio", "TSF Jazz"),
    public_plus_classique_jazz_nova = c("France Musique", "Radio Classique", 
                                        "Jazz Radio", "TSF Jazz", "France Inter", 
                                        "Fip", "Radio Nova"),
    extensif = c("France Musique", "Radio Classique", "Jazz Radio", 
                 "ABC Lounge Jazz", "TSF Jazz", "France Inter", 
                 "Fip", "Radio Nova", "Radio Meuh", "Djam Radio"))
  res <- vector("list", length = length(leg_radio_lists))
  for(i in 1:length(res)){
    leg <- radio_artist %>% 
      mutate(leg = ifelse(radio %in% leg_radio_lists[[i]], "gatekeeper", "commercial")) %>% 
      group_by(artist_id, leg) %>% 
      summarize(n = sum(n)) %>% 
      mutate(f_gtkp = n / sum(n)) %>% 
      rename(n_gtkp = "n") %>% 
      filter(leg == "gatekeeper") %>% 
      select(-leg) %>% 
      left_join(genres) %>% 
      left_join(artist_names) %>% 
      ungroup()
    res[[i]] <- leg %>% 
      arrange(desc(n_gtkp)) %>% 
      slice(1:100)
  }
  names(res) <- names(leg_radio_lists)
  for(i in 1:length(res)){
    res[[i]]$def <- names(res)[[i]]
  }
  full <- bind_rows(res)
  filename <- str_glue("output/omni1/tb_robustness_top_artist_leg_definition.csv")
  write_csv(full, filename)
  return(filename)
  
}


trial_pond_radio <- function(radio_artist, artist_names, genres, artists_pop){
  # weighting radio by reverse commercial logic (average popularity of artists)
  ra <- radio_artist# %>% 
    #filter(!(radio %in% c("Sud radio", "Beur FM", "Party Time", "RTL")))
  pond <- ra %>% 
    left_join(artists_pop) %>% 
    select(radio, artist_id, n, control_n_users) %>% 
    group_by(radio) %>% 
    mutate(f = n/sum(n)) %>% 
    summarize(average_pop = sum(f * n)) %>% 
    mutate(pond = scale(1/average_pop)[,1]) %>% 
    select(radio, pond)
  
  ra %>% 
    left_join(pond) %>% 
    group_by(artist_id) %>% 
    filter(sum(n) > 100) %>% 
    summarize(radio_leg = sum(n/100*pond)) %>% 
    arrange(radio_leg) %>% 
    slice(1:1000) %>% 
    left_join(artist_names) %>% 
    left_join(genres) %>% 
    View()
}