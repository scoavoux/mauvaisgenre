make_survey_data <- function(){
  require(tidyverse)
  s3 <- initialize_s3()
  f <- s3$get_object(Bucket = "scoavoux", Key = "records_w3/survey/RECORDS_Wave3_apr_june_23_responses_corrected.csv")
  survey <- f$Body %>% rawToChar() %>% data.table::fread() %>% tibble()
  return(survey)
}

make_genre_preference_data <- function(){
  require(tidyverse)
  s3 <- initialize_s3()
  f <- s3$get_object(Bucket = "scoavoux", Key = "records_w3/survey/RECORDS_Wave3_apr_june_23_responses_corrected.csv")
  survey <- f$Body %>% rawToChar() %>% data.table::fread() %>% tibble()
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

# Compute user_artist table: number and length of each user listening
# to each artist
list_streaming_data_files <- function(){
  require(tidyverse)
  s3 <- initialize_s3()
  
  stream_data_files <- s3$list_objects_v2(Bucket = "scoavoux", Prefix = "records_w3/streams")$Content %>% map(~.x$Key) %>% 
    unlist()
  stream_data_files <- stream_data_files[str_detect(stream_data_files, "part-")]
  return(stream_data_files)
}

make_user_artist_peryear_table_onefile <- function(file){
  require(tidyverse)
  require(tidytable)
  require(arrow)
  require(lubridate)
  
  s3 <- initialize_s3()
  users <- s3$get_object(Bucket = "scoavoux", Key = "records_w3/RECORDS_hashed_user_group.parquet")$Body %>% 
    read_parquet()
  if(str_detect(file, "long")){
    streams <- s3$get_object(Bucket = "scoavoux", Key = file)$Body %>% 
      read_parquet(col_select = c("hashed_id", "ts_listen", "song_id",
                                  "is_listened", "listening_time"))
  } else if(str_detect(file, "short")) {
    streams <- s3$get_object(Bucket = "scoavoux", Key = file)$Body %>% 
      read_parquet(col_select = c("hashed_id", "ts_listen", "media_id",
                                  "is_listened", "listening_time", "media_type")) %>% 
      filter(media_type == "song") %>% 
      rename(song_id = "media_id") %>% 
      select(-media_type)
  }
  streams <- streams %>% 
    filter(hashed_id %in% filter(users, is_respondent)$hashed_id) %>% 
    mutate(year = year(as_datetime(ts_listen)),
           lt = ifelse(listening_time < 0, 0, listening_time)) %>% 
    filter(year > 2017, 
           is_listened == 1) %>% 
    select(-ts_listen, -listening_time)
  
  items <- s3$get_object(Bucket = "scoavoux", Key = "records_w3/items/songs.snappy.parquet")$Body %>% 
    read_parquet(col_select = c("song_id", "artist_id"))
  
  user_artist_peryear <- streams %>% 
    left_join(items) %>% 
    summarise(l_play = sum(lt), 
              n_play = n(),
              .by = c(hashed_id, year, artist_id))
  return(user_artist_peryear)
}

merge_user_artist_peryear_table <- function(...){
  library(tidyverse)
  library(tidytable)
  streams <- bind_rows(...) %>% 
    summarise(l_play = sum(l_play),
              n_play = sum(n_play),
              .by = c(hashed_id, year, artist_id))
  return(streams)
}

