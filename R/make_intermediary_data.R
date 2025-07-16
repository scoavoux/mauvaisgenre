# Deal with survey data ------
# For now we just import it from the last project
make_survey_data <- function(){
  require(tidyverse)
  s3 <- initialize_s3()
  f <- s3$get_object(Bucket = "scoavoux", Key = "records_w3/survey/RECORDS_Wave3_apr_june_23_responses_corrected.csv")
  survey <- f$Body %>% rawToChar() %>% data.table::fread() %>% tibble()
  
  # filter only 
  survey <- survey %>% 
    filter(Progress == 100,
           country == "FR")
  return(survey)
}


# Make genre preferences data. The questions on which genres you like
# are stored in a lot of different variables depending on whether they 
# are loved or liked, and how they are ranked. We simplify this by 
# making a series of dummy variables of whether each genre is either loved
# or liked.

make_genre_preference_data <- function(survey){
  require(tidyverse)
  gc <- survey %>%
    select(hashed_id, matches("B_genres_classif_\\d_GROUP")) %>%
    pivot_longer(-hashed_id, values_to = "genre") %>%
    filter(genre != "") %>%
    mutate(group = str_replace(name, "B_genres_classif_(\\d)_GROUP_\\d+", "\\1"),
           genreno = str_replace(name, "B_genres_classif_\\d_GROUP_(\\d+)", "\\1")) %>%
    select(-name)
  gr <- survey %>%
    select(hashed_id, matches("B_genres_classif_\\d_\\d")) %>%
    pivot_longer(-hashed_id, values_to = "rank") %>%
    filter(rank != "") %>%
    mutate(group = str_replace(name, "B_genres_classif_(\\d)_\\d+_RANK", "\\1"),
           genreno = str_replace(name, "B_genres_classif_\\d_(\\d+)_RANK", "\\1")) %>%
    select(-name)
  full <- full_join(gc, gr)
  return(full)
}


make_items_data <- function(){
  require(tidyverse)
  require(tidytable)

  s3 <- initialize_s3()
  
  items_old <- s3$get_object(Bucket = "scoavoux", Key = "records_w3/items/songs.snappy.parquet")$Body %>% 
    read_parquet(col_select = c("song_id", "artist_id"))
  items_new <- s3$get_object(Bucket = "scoavoux", Key = "records_w3/items/song.snappy.parquet")$Body %>% 
    read_parquet(col_select = c("song_id", "artist_id"))  
  items <- bind_rows(items_old, items_new) %>% 
    distinct()
  return(items)
}

# Make short stream user_artist data ------
make_user_artist_2022 <- function(items, senscritique_mb_deezer_id, to_remove_file){
  require(aws.s3)
  require(lubridate)
  
  # items to take care of duplicated artists 
  # To remove "fake" artists: accounts that compile anonymous music
  to_remove <- to_remove_file %>% 
    select(artist_id)
  
  items <- items %>% 
    anti_join(to_remove) %>% 
    left_join(senscritique_mb_deezer_id) %>% 
    mutate(artist_id = if_else(!is.na(consolidated_artist_id), consolidated_artist_id, artist_id)) %>% 
    select(song_id, artist_id) %>% 
    filter(!is.na(artist_id))
  
  data_cloud <- arrow::open_dataset(
    source =   arrow::s3_bucket(
      "scoavoux",
      endpoint_override = "minio.lab.sspcloud.fr"
    )$path("records_w3/streams/streams_short"),
    partitioning = arrow::schema(REGION = arrow::utf8())
  )
  query <- data_cloud %>% 
    select(hashed_id, is_listened, ts_listen, listening_time, media_type, song_id = "media_id") %>% 
    mutate(year = year(as_datetime(ts_listen)),
           lt = ifelse(listening_time < 0, 0, listening_time)) %>% 
    filter(media_type == "song", is_listened == 1, year == 2022) %>% 
    inner_join(items) %>% 
    group_by(hashed_id, artist_id) %>% 
    summarize(l_play = sum(lt), 
              n_play = n()) %>% 
    ungroup()
  short_streams <- collect(query)
  return(short_streams)
}
