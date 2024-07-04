library(targets)
library(tidyverse)
library(tidytable)
tar_load("senscritique_mb_deezer_id")
senscritique_mb_deezer_id

s3 <- initialize_s3()
f <- s3$get_object(Bucket = "scoavoux", Key = "senscritique/contacts.csv")
co <- f$Body %>% rawToChar() %>% fread()
rm(f)

## tous les artists non identifiés dans deezer
co <- anti_join(co, senscritique_mb_deezer_id)

s3$download_file(Bucket = "scoavoux", 
                 Key = "records_w3/items/artists_data.snappy.parquet",
                 Filename = "data/artists_data.snappy.parquet")
artists <- read_parquet("data/artists_data.snappy.parquet", col_select = 1:3)
artists <- artists %>% 
  filter(!is.na(main_genre))
tar_load("artists_pop")

artists_pop <- artists_pop %>% 
  filter(respondent_n_users > 20)

artists <- artists %>% 
  # tous les artistes avec plus de 20 usagers
  left_join(select(artists_pop, artist_id)) %>% 
  # et pas déjà identifiés
  anti_join(senscritique_mb_deezer_id)

ju <- inner_join(select(artists, artist_id, name), select(co, contact_id, name = contact_name))
ju %>% 
  add_count(name) %>% 
  filter(n == 1) %>% 
  left_join(artists_pop) %>% 
  arrange(desc(respondent_n_users))
