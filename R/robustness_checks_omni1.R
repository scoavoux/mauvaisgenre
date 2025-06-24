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
  tb <- radio_artist %>% 
    arrange(radio, desc(n)) %>% 
    slice(1:50, .by = radio) %>% 
    left_join(artist_names) %>% 
    left_join(genres)
  
  filename <- str_glue("output/omni1/tb_robustness_radio_artist_genre.csv")
  write_csv(tb, filename)
  return(filename)
}