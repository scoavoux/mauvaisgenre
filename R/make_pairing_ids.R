# Make senscritique data ------
# Makes pairing of deezer id (artist_id) with musicbrainz and senscritique
# Starts from dump of senscritique SQL database,
# made July, 1st 2024

make_senscritique_pairing_data <- function(){
  require(tidyverse)
  require(tidytable)
  require(WikidataQueryServiceR)
  s3 <- initialize_s3()
  
  ## Import various pairings ------
  
  ### Spotify/deezer id pairings from WIKIDATA ------
  wikidata_spotify_deezer <- query_wikidata('SELECT DISTINCT ?spotify_id ?deezer_id #?item ?itemLabel 
  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE]". }
    ?item p:P1902 ?statement0.
    ?statement0 (ps:P1902) _:anyValueP1902.
    ?item p:P2722 ?statement1.
    ?statement1 (ps:P2722) _:anyValueP2722.
    ?item wdt:P2722 ?deezer_id.
    ?item wdt:P1902 ?spotify_id.
  }')
  
  ### Wikidata/deezer id pairs from WIKIDATA ------
  wikidata_deezer <- query_wikidata('SELECT DISTINCT ?item ?deezer_id #?item ?itemLabel 
  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE]". }
    ?item p:P2722 ?statement1.
    ?statement1 (ps:P2722) _:anyValueP2722.
    ?item wdt:P2722 ?deezer_id.
  }')
  wikidata_deezer <- wikidata_deezer %>% 
    mutate(wikidata_id = str_extract(item, "/(Q\\d+)", group = 1)) %>% 
    select(-item)
  
  f <- s3$get_object(Bucket = "scoavoux", Key = "musicbrainz/mbid_wikidataid_pair.csv")
  mbz_wikidata <- f$Body %>% rawToChar() %>% read_csv()
  rm(f)
  
  mbz_wikidata <- mbz_wikidata %>% 
    select(-mbname) %>% 
    inner_join(wikidata_deezer) %>% 
    select(-wikidata_id)
  
  ### Musicbrainz id / deezer id pairs from musicbrainz dumps ------
  f <- s3$get_object(Bucket = "scoavoux", Key = "musicbrainz/mbid_deezerid_pair.csv")
  mbz_dz <- f$Body %>% rawToChar() %>% read_csv()
  rm(f)
  
  ### Musicbrainz id / spotify id to add spotify id when it lacks from co data ------
  f <- s3$get_object(Bucket = "scoavoux", Key = "musicbrainz/mbid_spotifyid_pair.csv")
  mbid_spotifyid <- f$Body %>% rawToChar() %>% read_csv()
  
  ### SensCritique / deezer id ------
  #### From manual search by me... highly trustworthy ------
  pairings0 <- read_csv("data/manual_search.csv")
  
  #### from Deezer api search (old) ------
  f <- s3$get_object(Bucket = "scoavoux", Key = "senscritique/senscritique_id_deezer_id_pairing.csv")
  pairings1 <- f$Body %>% rawToChar() %>% read_csv()
  
  f <- s3$get_object(Bucket = "scoavoux", Key = "senscritique/senscritique_deezer_id_pairing_2.csv")
  pairings2 <- f$Body %>% rawToChar() %>% read_csv()
  
  #### From exact matches ------
  ## exact match between artists in dz and sc.
  ## Only those with unique match
  ## see script pair_more_artists
  f <- s3$get_object(Bucket = "scoavoux", Key = "senscritique/senscritique_deezer_id_pairing_3.csv")
  pairings3 <- f$Body %>% rawToChar() %>% read_csv()
  rm(f)  
  
  ## Exact match but allow multiple matches -- script will collapse them
  ## together afterwards
  f <- s3$get_object(Bucket = "scoavoux", Key = "senscritique/senscritique_deezer_id_pairing_4.csv")
  pairings4 <- f$Body %>% rawToChar() %>% read_csv()
  rm(f)  
  
  #### Put them together ------
  pairings <- pairings1 %>% 
    select(contact_id, artist_id) %>% 
    bind_rows(pairings0,
              select(pairings2, contact_id, artist_id = "deezer_id"),
              pairings3,
              pairings4) %>% 
    distinct()
  
  pairings <- pairings %>% 
    filter(!is.na(artist_id))
  
  # TODO: see how many artists we are missing and whether we should add another
  # service with spotify/deezer ids (or push them to wikidata)
  # especially: see maping senscritique id deezer id used on previous
  # project (based on... search in SC database?)
  # Also need to check whether this is a problem with SC spotify links or
  # with wikidata not having spotify/deezer match.
  f <- s3$get_object(Bucket = "scoavoux", Key = "senscritique/contacts.csv")
  co <- f$Body %>% rawToChar() %>% fread() %>% distinct()
  rm(f)
  
  f <- s3$get_object(Bucket = "scoavoux", Key = "senscritique/contacts_tracks.csv")
  co_tr <- f$Body %>% rawToChar() %>% fread()
  rm(f)
  co <- bind_rows(co, co_tr) %>% distinct()
  
  co <- distinct(co) %>% 
    mutate(spotify_id = str_remove(spotify_id, "spotify:artist:"),
           spotify_id = ifelse(spotify_id == "", NA, spotify_id)) %>% 
    rename(mbid = "mbz_id")
  
  co <- co %>% 
    left_join(select(mbid_spotifyid, mbid, spotify_id2 = "spotify_id")) %>% 
    mutate(spotify_id = ifelse(is.na(spotify_id), spotify_id2, spotify_id)) %>% 
    select(-spotify_id2) %>% 
    left_join(select(wikidata_spotify_deezer, spotify_id, deezer_id), by = "spotify_id") %>% 
    left_join(select(mbz_wikidata, mbid, deezer_id_wk = "deezer_id")) %>% 
    left_join(select(mbz_dz, mbid, deezer_id_mb = "deezer_id")) %>% 
    left_join(select(pairings, contact_id, deezer_id_p = "artist_id"))
  
  co <- co %>% 
    mutate(deezer_id = as.numeric(deezer_id),
           artist_id = case_when(!is.na(deezer_id) ~ deezer_id, 
                                 !is.na(deezer_id_wk) ~ deezer_id_wk,
                                 !is.na(deezer_id_mb) ~ deezer_id_mb,
                                 TRUE ~ deezer_id_p),
           id_origin = case_when(!is.na(deezer_id)    ~ "Wikidata spotify/deezer pair", 
                                 !is.na(deezer_id_wk) ~ "Wikidata mbid/deezer pair",
                                 !is.na(deezer_id_mb) ~ "Musicbrainz mbid / deezer pair",
                                 !is.na(deezer_id_p)  ~ "API search"))
  
  # Manque désormais plus que 126 artists
  # anti_join(artists_filtered, co) %>% select(artist_id, artist_name) %>% 
  #   arrange(artist_id) %>% 
  #   print(n=130)
  
  co <- co %>% 
    filter(!is.na(artist_id)) %>% 
    distinct(contact_id, artist_id, mbid)
  
  ## Now sometimes one contact_id equivalent to several artist_id
  ## = one artist identified on senscritique has several deezer profile
  ## We consolidate them to the smallest artist_id (somewhat arbitrary
  ## but usually means the oldest deezer profile)
  co <- co %>% 
    arrange(contact_id, artist_id) %>% 
    group_by(contact_id) %>% 
    mutate(inter_artist_id = first(artist_id)) %>% 
    ungroup() %>% 
    group_by(artist_id) %>% 
    mutate(consolidated_artist_id = first(inter_artist_id)) %>% 
    select(-inter_artist_id)
  
  return(co)  
}
