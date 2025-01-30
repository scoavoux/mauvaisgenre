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

# Making data on consumption ------
## First a function to list available datasets. Returns all streaming data
## whether long or short.
list_streaming_data_files <- function(){
  require(tidyverse)
  s3 <- initialize_s3()
  
  stream_data_files <- s3$list_objects_v2(Bucket = "scoavoux", Prefix = "records_w3/streams")$Content %>% map(~.x$Key) %>% 
    unlist()
  stream_data_files <- stream_data_files[str_detect(stream_data_files, "part-")]
  # the analysis has been done before we got the 2024 data and there has been so
  # much idiosyncratic work done to clean up artists list, etc. that I really 
  # don't want to add the newer data now.
  # So... we exclude it, plain and simple.
  stream_data_files <- stream_data_files[!str_detect(stream_data_files, "stream_with_context")]
  return(stream_data_files)
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


## Because importing them all at once creates memory problems, we divide
## the task. This function loads and preprocesses each streaming data file
## which is then turned to the next function for summary.
make_user_artist_peryear_table_onefile <- function(file, items){
  require(tidyverse)
  require(tidytable)
  require(arrow)
  require(lubridate)
  
  s3 <- initialize_s3()
  # users <- s3$get_object(Bucket = "scoavoux", Key = "records_w3/RECORDS_hashed_user_group.parquet")$Body %>% 
  #   read_parquet()
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
    # filter(hashed_id %in% filter(users, is_respondent)$hashed_id) %>% 
    mutate(year = year(as_datetime(ts_listen)),
           lt = ifelse(listening_time < 0, 0, listening_time)) %>% 
    filter(year > 2017, 
           is_listened == 1) %>% 
    select(-ts_listen, -listening_time)
  
  
  user_artist_peryear <- streams %>% 
    left_join(items) %>% 
    summarise(l_play = sum(lt), 
              n_play = n(),
              .by = c(hashed_id, year, artist_id))
  return(user_artist_peryear)
}

## We bind each of the previous datasets together and compute summary stats.
merge_user_artist_peryear_table <- function(...){
  library(tidyverse)
  library(tidytable)
  streams <- bind_rows(...) %>% 
    summarise(l_play = sum(l_play),
              n_play = sum(n_play),
              .by = c(hashed_id, year, artist_id))
  return(streams)
}

# Merge duplicate artists and remove junk artists
merge_duplicate_artists_in_streams <- function(user_artist_peryear, senscritique_mb_deezer_id, to_remove_file){
  require(tidyverse)
  require(tidytable)
  
  # To remove "fake" artists: accounts that compile anonymous music
  to_remove <- to_remove_file %>% 
    select(artist_id)
  
  senscritique_mb_deezer_id <- distinct(senscritique_mb_deezer_id, artist_id, consolidated_artist_id)
  user_artist_peryear <- user_artist_peryear %>% 
    anti_join(to_remove) %>% 
    left_join(senscritique_mb_deezer_id) %>% 
    mutate(artist_id = if_else(!is.na(consolidated_artist_id), consolidated_artist_id, artist_id)) %>% 
    summarise(l_play = sum(l_play),
              n_play = sum(n_play),
              .by = c(hashed_id, year, artist_id))
  return(user_artist_peryear)
}

