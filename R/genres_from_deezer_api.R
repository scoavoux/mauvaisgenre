# This script has functions that are not part of the pipeline
# they are one-off functions to make/pair more data

pair_more_artists_with_senscritique <- function(){
  library(targets)
  library(tidyverse)
  library(tidytable)
  library(arrow)
  tar_load("senscritique_mb_deezer_id")
  senscritique_mb_deezer_id
  source("R/common_functions.R")
  s3 <- initialize_s3()
  f <- s3$get_object(Bucket = "scoavoux", Key = "senscritique/contacts.csv")
  co <- f$Body %>% rawToChar() %>% fread()
  rm(f)
  f <- s3$get_object(Bucket = "scoavoux", Key = "senscritique/contacts_tracks.csv")
  co_tr <- f$Body %>% rawToChar() %>% fread()
  rm(f)
  
  co <- bind_rows(co, co_tr) %>% distinct()
  
  ## tous les artists non identifiés dans deezer
  co <- anti_join(co, senscritique_mb_deezer_id)
  
  s3$download_file(Bucket = "scoavoux", 
                   Key = "records_w3/items/artists_data.snappy.parquet",
                   Filename = "data/temp/artists_data.snappy.parquet")
  artists <- read_parquet("data/artists_data.snappy.parquet", col_select = 1:3)
  artists <- artists %>% 
    filter(!is.na(main_genre))
  
  tar_load("artists_pop")
  
  artists_pop <- artists_pop %>% 
    filter(respondent_n_users > 10)
  
  artists <- artists %>% 
    # tous les artistes avec plus de 20 usagers
    left_join(select(artists_pop, artist_id)) %>% 
    # et pas déjà identifiés
    anti_join(senscritique_mb_deezer_id)
  artists %>% filter(name == "Ramones")
  co %>% filter(str_detect(contact_name, "Ramones"))
  ju <- inner_join(select(artists, artist_id, name), select(co, contact_id, name = contact_name))
  
  more <- ju %>% 
    distinct() %>% 
    add_count(name)
  
  more %>% 
    arrange(name)
  
  more %>% 
    select("contact_id", "artist_id") %>% 
    fwrite("senscritique_deezer_id_pairing_4.csv")
  
}

get_missing_genres_from_deezer <- function(){
  # For artists with missing genres
  # interrogate the deezer api for a list
  # of albums; get genre of each album
  
  # packages and data
  library(targets)
  library(tidyverse)
  library(httr)
  library(rvest)
  # install.packages("tictoc")
  library(tictoc)
  
  tar_load(artists_raw)
  
  # filter data
  # only missing genres of course
  # for now only artists with at least 5 people with ISEI listening to them
  # because this is those we are going to use in the end
  # divides by 10 the number of calls needed
  artists_raw <- filter(artists_raw, is.na(genre), !is.na(respondent_n_users)) %>% 
    filter(n_isei > 5)
  
  ar <- artists_raw %>% select(artist_id) %>% mutate(scrapped = FALSE)
  
  load(file = "data/temp/scraping_genres.RData")
  
  if(!dir.exists("data/temp/artists_albums")) dir.create("data/temp/artists_albums")
  sum(ar$scrapped)
  ar$scrapped[ar$artist_id %in% (dir("data/temp/artists_albums/") %>% str_remove(".json") %>% as.numeric())] <- TRUE
  t <- now()
  tic()
  for(i in 1:nrow(artists_raw)){
    if(ar$scrapped[i]) next
    p <- RETRY("GET", paste0("https://api.deezer.com/artist/", ar$artist_id[i], "/albums?limit=100"))
    x <- content(p)
    jsonlite::write_json(x, paste0("data/temp/artists_albums/", ar$artist_id[i], ".json"))
    # if(length(x$data) > 0){
    #   al[[i]] <- map(x$data, ~tibble(id = .x$id, record_type = .x$record_type)) %>% 
    #     bind_rows() %>% 
    #     mutate(artist_id = ar$artist_id[i]) %>% 
    #     filter(record_type == "album")
    #     
    # }
    ar$scrapped[i] <- TRUE
    if(i %% 5000 == 0){
      save(ar, file = "data/temp/scraping_genres.RData")
    }
    if(i %% 50 == 0){
      while(now()-t < 5.1) Sys.sleep(.1)
      t <- now()
    }
    if(i %% 1000 == 0){
      toc()
      print(paste0(i, ", ", round(i/nrow(ar)*100, 2), "%"))
      tic()
    }
  }
  # Execute from here
  save(ar, file = "data/temp/scraping_genres.RData")
  toc()
  
  require(jsonlite)
  js <- dir("data/temp/artists_albums", full.names = TRUE)
  dir
  ex <- function(jf){
    d <- fromJSON(jf)$data
    if(length(d) > 0){
      x <- d %>% 
        select(album_id = id, genre_id, record_type) %>% 
        mutate(artist_id = str_extract(jf, "\\d+") %>% as.numeric())
      return(x) 
    }
  }

  al <- map(js, ~ex(.x))
  al <- bind_rows(al)
  al <- al %>% 
    mutate(across(everything(), unlist)) %>% 
    tibble()
  g <- al %>%  left_join(tibble(record_type = c("album", "ep", "single"), w = c(1, .5, .1))) %>% 
    group_by(artist_id, genre_id) %>% 
    summarize(n = sum(w)) %>% 
    ungroup() %>% 
    filter(genre_id > -.1) %>% 
    arrange(artist_id, desc(n)) %>% 
    slice(1, .by = artist_id)
  
  # now pair this with genre list from deezer api
  gl <- vector("list", length = length(unique(g$genre_id)))
  names(gl) <- as.character(unique(g$genre_id))
  for(gnr in unique(g$genre_id)){
    p <- RETRY("GET", paste0("https://api.deezer.com/genre/", gnr)) %>% content()
    gl[[as.character(gnr)]] <- tibble(genre_id = p$id, genre = p$name)
  }
  gl <- bind_rows(gl)
  # BEWARE THIS APPENDS TO EXISTING DATA DO NOT EXECUTE TWICE IN A ROW
  g %>% 
    left_join(gl) %>% 
    select(-n, -genre_id) %>% 
    write_csv("data/genres_from_deezer_albums.csv", append=TRUE)
}

get_missing_names_from_deezer_api <- function(){
  # packages and data
  library(targets)
  library(tidyverse)
  library(httr)
  library(rvest)
  # install.packages("tictoc")
  library(tictoc)
  
  tar_load(artists_raw)
  
}