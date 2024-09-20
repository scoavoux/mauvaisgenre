# For artists with missing genres
# interrogate the deezer api for a list
# of albums; get genre of each album

# packages and data
library(targets)
library(tidyverse)
library(httr)
library(rvest)

tar_load(artists_raw)

# filter data
# only missing genres of course
# for now only artists with at least 5 people with ISEI listening to them
# because this is those we are going to use in the end
# divides by 10 the number of calls needed
artists_raw <- filter(artists_raw, is.na(genre), !is.na(respondent_n_users)) %>% 
  filter(n_isei > 5)

ar <- artists_raw %>% select(artist_id) %>% mutate(scrapped = FALSE)
al <- vector("list", length = nrow(artists_raw))

load(file = "data/temp/scraping_genres.RData")

t <- now()
for(i in 1:nrow(artists_raw)){
  if(ar$scrapped[i]) next
  p <- RETRY("GET", paste0("https://api.deezer.com/artist/", ar$artist_id[i], "/albums"))
  x <- content(p)
  if(length(x$data) > 0){
    al[[i]] <- map(x$data, ~tibble(id = .x$id, type = .x$type)) %>% 
      bind_rows() %>% 
      filter(type == "album") %>% 
      mutate(artist_id = ar$artist_id[i])
  }
  ar$scrapped[i] <- TRUE
  if(i %% 5000 == 0){
    save(ar, al, file = "data/temp/scraping_genres.RData")
  }
  if(i %% 50 == 0){
    while(now()-t < 5.1) Sys.sleep(.1)
    t <- now()
    print(paste0(i, ", ", round(i/nrow(ar)*100, 2), "%"))
  } 
}
# Execute from here
save(ar, al, file = "data/temp/scraping_genres.RData")

album_artist_link <- bind_rows(al) %>% 
  rename(album_id = id)

alb <- album_artist_link %>% distinct(album_id) %>% 
  mutate(scrapped = FALSE)
ge <- vector("list", length = nrow(alb))
ge2 <- ge3 <- ge4 <- ge5 <- vector("list", length = 50000L)



load(file = "data/temp/scraping_genres_albums.RData")

t <- now()
ch <- 151650
it <- 50000
for(i in 1:nrow(alb)){
  if(alb$scrapped[i]) next
  p <- RETRY("GET", paste0("https://api.deezer.com/album/", alb$album_id[i]))
  x <- content(p)
  if(length(x$genres$data) > 0){
    # ge[[i]] <- map(x$genres$data, ~tibble(genre_id = .x$id, genre = .x$name)) %>% 
    #   bind_rows()
    if(i < ch+it){
      ge2[[i-ch]] <- map(x$genres$data, ~tibble(genre_id = .x$id, genre = .x$name)) %>% 
        bind_rows() %>% 
        mutate(album_id = alb$album_id[i])
    } else if(i < ch+2*it){
      ge3[[i-ch-it+1]] <- map(x$genres$data, ~tibble(genre_id = .x$id, genre = .x$name)) %>% 
        bind_rows() %>% 
        mutate(album_id = alb$album_id[i])
    } else if(i < ch+3*it){
      ge4[[i-ch-2*it+1]] <- map(x$genres$data, ~tibble(genre_id = .x$id, genre = .x$name)) %>% 
        bind_rows() %>% 
        mutate(album_id = alb$album_id[i])
      
    } else if(i < ch+4*it){
      ge5[[i-ch-3*it+1]] <- map(x$genres$data, ~tibble(genre_id = .x$id, genre = .x$name)) %>% 
        bind_rows() %>% 
        mutate(album_id = alb$album_id[i])
    }
  }
  alb$scrapped[i] <- TRUE
  if(i %% 10000 == 0){
    save(alb, ge, ge2, ge3, ge4, ge5, file = "data/temp/scraping_genres_albums.RData")
  }
  if(i %% 50 == 0){
    while(now()-t < 5.1) Sys.sleep(.1)
    t <- now()
    print(paste0(i, ", ", round(i/nrow(alb)*100, 2), "%"))
  } 
}
save(alb, ge, ge2, ge3, ge4, ge5, file = "data/temp/scraping_genres_albums.RData")

g <- bind_rows(ge, ge2, ge3, ge4, ge5)

gu <- slice(g, 1, .by = album_id)

album_artist_link %>% 
  left_join(gu) %>% 
  group_by(artist_id) %>% 
  count(genre) %>% 
  arrange(artist_id, n) %>% 
  slice(1)
