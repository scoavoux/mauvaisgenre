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
