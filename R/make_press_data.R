
# filter corpus 

# Make corpus ------
make_raw_corpus <- function(){
  require(tidyverse)
  
  # Telerama
  telerama <- s3_read("french_media/telerama_raw.csv")
  
  telerama <- mutate(telerama, across(everything(), ~str_trim(.x)))
  
  telerama <- separate(telerama, PublicationName, 
                       into = c("source_name", "issue"), sep = ", ")
  
  telerama <- mutate(telerama, issue = str_remove(issue, "no. "))
  
  telerama <- mutate(telerama, 
                     date = str_replace(DocHeader, 
                                        "^.*?((vendredi|samedi) \\d+ \\w+ \\d{4}).*$", 
                                        "\\1"),
                     article_page = str_extract(DocHeader, "p. .*$"))
  
  telerama <- mutate(telerama,
                     DocHeader = str_remove(DocHeader,
                                            ",?\\s*(vendredi|samedi) \\d+ \\w+ \\d{4} \\w+ mots, p. .*$"))
  
  telerama <- rename(telerama, section = DocHeader)
  
  telerama <- mutate(telerama, 
                     date = str_remove(date, "vendredi|samedi") %>% dmy())
  
  telerama <- mutate(telerama, 
                     Notes = str_remove(Notes, "Note\\(s\\) : ;"),
                     reviewed_score = str_extract(Notes, "\\b\\dF\\b"))
  telerama <- mutate(telerama,
                     reviewed_score = ifelse(is.na(reviewed_score), 
                                             str_extract(Text, "\\bHelas\\b$"), 
                                             reviewed_score))
  telerama <- mutate(telerama,
                     reviewed_score = ifelse(is.na(reviewed_score), 
                                             str_extract(Text, "\\b\\dT\\b"), 
                                             reviewed_score),
                     article_type = ifelse(!is.na(reviewed_score), "review", NA))
  
  telerama <- mutate(telerama,
                     reviewed_artist = str_extract(Notes, "^.*? /") %>% str_remove(" /"),
                     reviewed_title =  str_extract(Notes, "/ .*? >") %>% str_remove_all("[/>]") %>% str_trim())
  
  telerama <- rename(telerama,
                     article_title = "TitreArticle",
                     article_text = "Text",
                     article_annex = "Notes",
                     article_author = "Authors") %>% 
    mutate(source_origin = "Europresse",
           url = NA)
  
  # Le Monde
  lemonde <- vector("list", length = 12L)
  for(i in 1:12){
    lemonde[[i]] <- s3_read(paste0("french_media/lemonde/", paste0("lemonde-20", 10L:22L,".csv"))[i])
    print(i)
  }
  
  lemonde <- lapply(lemonde, function(x) {
    x$is_article <- as.logical(x$is_article)
    x$annee <- as.numeric(x$annee)
    x$publidate <- ymd(x$publidate)
    return(x)
  }
  )
  
  lemonde <- bind_rows(lemonde)
  
  lemonde <- filter(lemonde, str_detect(rubrique, "[Mm]usique"))
  
  # attention, il y a plein de rubriques variées
  lemonde <- filter(lemonde, is_article)
  lemonde <-  rename(lemonde, 
                     date = "publidate",
                     section = "rubrique",
                     article_author = "auteur",
                     article_title = "titre",
                     article_excerpt = "chapo",
                     article_text = "texte") %>% 
    select(-annee, -publiheure, -is_article, -file)
  
  lemonde <- mutate(lemonde, 
                    source_name = "Le Monde",
                    source_origin = "scrapping")
  
  ## Le Figaro
  s3 <- initialize_s3()
  s3$download_file("scoavoux", "french_media/lefigaro-complet-v0.csv", 
                   Filename = "data/temp/lefigaro-complet-v0.csv")
  lefigaro <- read_csv("data/temp/lefigaro-complet-v0.csv")
  file.remove("data/temp/lefigaro-complet-v0.csv")
  lefigaro <- filter(lefigaro, year > 2009)
  
  lefigaro <- filter(lefigaro, rubrique == "Culture")
  lefigaro <- mutate(lefigaro, date = str_remove(date, "T.*") %>% ymd())
  
  lefigaro <- lefigaro %>% 
    select(date, 
           section = "rubrique",
           article_title = "titre",
           article_author = "auteur",
           article_text = "texte"
    ) %>% 
    mutate(source_name = "Le Figaro")
  
  
  ## Libération
  s3$download_file("scoavoux", "french_media/liberation-complet-v2.csv", "data/temp/liberation-complet-v2.csv")
  
  liberation <- read_csv("data/temp/liberation-complet-v2.csv")
  file.remove("data/temp/liberation-complet-v2.csv")
  liberation <- filter(liberation, rubrique %in% c("Culture", "Culture | Musique", "Musique"))
  liberation <- rename(liberation,
                       date = "publidate",
                       section = "rubrique",
                       article_author = "auteur",
                       article_title = "titre",
                       article_excerpt = "chapo",
                       article_text = "texte") %>% 
    select(-publiheure, -file)
  liberation <- mutate(liberation, 
                       source_name = "Libération",
                       date = ymd(date))
  corpus_raw <- bind_rows(telerama, lemonde, liberation, lefigaro)  %>% 
    filter(!is.na(article_text))
  return(corpus_raw)
}

train_bert_model <- function(){}

# Make aliases ------
make_aliases <- function(senscritique_mb_deezer_id){
  require(tidyverse)
  require(tidytable)
  require(arrow)
  s3 <- initialize_s3()
  
  ## Names from deezer
  s3$download_file(Bucket = "scoavoux", 
                   Key = "records_w3/items/artists_data.snappy.parquet",
                   Filename = "data/temp/artists_data.snappy.parquet")
  artists <- read_parquet("data/temp/artists_data.snappy.parquet", col_select = 1:2)
  artists <- mutate(artists, type = "deezername")
  
  artists <- artists %>% 
    left_join(senscritique_mb_deezer_id) %>% 
    filter(!is.na(consolidated_artist_id)) %>% 
    select(consolidated_artist_id, name, type)
  
  ## Names and aliases from musicbrainz
  f <- s3$get_object(Bucket = "scoavoux", Key = "musicbrainz/mbid_name_alias.csv")
  mb_aliases <- f$Body %>% rawToChar() %>% read_csv()
  rm(f)
  
  mb_aliases <- left_join(mb_aliases, senscritique_mb_deezer_id) %>% 
    filter(!is.na(consolidated_artist_id)) %>% 
    select(consolidated_artist_id, name, type) %>% 
    arrange(desc(type))
  
  
  consolidated_id_name_alias <- bind_rows(artists, mb_aliases) %>% 
    # separate names where several artists ;
    tidyr::separate_longer_delim(name, delim = ";") %>% 
    mutate(name = str_trim(name)) %>% 
    distinct(consolidated_artist_id, name, .keep_all = TRUE) %>% 
    rename(artist_id = consolidated_artist_id)
  return(consolidated_id_name_alias)
  
  # require(WikidataQueryServiceR)
  # 
  # # Get aliases from Wikidata
  # wikidata_spotifyid_aliases_en <- query_wikidata('SELECT DISTINCT ?spotify_id ?item ?itemLabel ?itemAltLabel
  # WHERE {
  #   SERVICE wikibase:label { bd:serviceParam wikibase:language "en". }
  #   ?item p:P1902 ?statement0.
  #   ?statement0 (ps:P1902) _:anyValueP1902.
  #   ?item wdt:P1902 ?spotify_id.
  # }')
  # wikidata_spotifyid_aliases_fr <- query_wikidata('SELECT DISTINCT ?spotify_id ?item ?itemLabel ?itemAltLabel
  # WHERE {
  #   SERVICE wikibase:label { bd:serviceParam wikibase:language "fr". }
  #   ?item p:P1902 ?statement0.
  #   ?statement0 (ps:P1902) _:anyValueP1902.
  #   ?item wdt:P1902 ?spotify_id.
  # }')
  # wikidata_spotifyid_aliases <- bind_rows(wikidata_spotifyid_aliases_en, 
  #                                         wikidata_spotifyid_aliases_fr) %>% 
  #   distinct()
  # 
  # 
  # wikidata_deezerid_aliases_en <- query_wikidata('SELECT DISTINCT ?deezer_id ?item ?itemLabel ?itemAltLabel
  # WHERE {
  #   SERVICE wikibase:label { bd:serviceParam wikibase:language "en". }
  #   ?item p:P2722 ?statement1.
  #   ?statement1 (ps:P2722) _:anyValueP2722.
  #   ?item wdt:P2722 ?deezer_id.
  # }')
  # wikidata_deezerid_aliases_fr <- query_wikidata('SELECT DISTINCT ?deezer_id ?item ?itemLabel ?itemAltLabel
  # WHERE {
  #   SERVICE wikibase:label { bd:serviceParam wikibase:language "fr". }
  #   ?item p:P2722 ?statement1.
  #   ?statement1 (ps:P2722) _:anyValueP2722.
  #   ?item wdt:P2722 ?deezer_id.
  # }')
  # wikidata_deezerid_aliases <- bind_rows(wikidata_deezerid_aliases_en, 
  #                                        wikidata_deezerid_aliases_fr) %>% 
  #   distinct()
  # 
  # wikidata_mbid_aliases_fr <- query_wikidata('SELECT DISTINCT ?mbid ?item ?itemLabel ?itemAltLabel
  # WHERE {
  #   SERVICE wikibase:label { bd:serviceParam wikibase:language "fr". }
  #   ?item p:P434 ?statement0.
  #   ?statement0 (ps:P434) _:anyValueP434.
  #   ?item wdt:P434 ?mbid.
  # }')
  # 
  # wikidata_mbid_aliases_en <- query_wikidata('SELECT DISTINCT ?mbid ?item ?itemLabel ?itemAltLabel
  # WHERE {
  #   SERVICE wikibase:label { bd:serviceParam wikibase:language "en". }
  #   ?item p:P434 ?statement0.
  #   ?statement0 (ps:P434) _:anyValueP434.
  #   ?item wdt:P434 ?mbid.
  # }')
  # 
  # wikidata_mbid_aliases <- bind_rows(wikidata_mbid_aliases_en, 
  #                                    wikidata_mbid_aliases_fr) %>% 
  #   distinct()
  
  
}

make_citation_data <- function(corpus_raw, artist_names_and_aliases){
  require(tidyverse)
  require(quanteda)
  require(spacyr)
  corpus(corpus_raw$article_text[1:10]) %>% spacy_tokenize(what = "sentence")
  filter(corpus_raw, str_detect(article_text, consolidated_id_name_alias$name[1]))
}

# Make press data ------
## BEWARE: THIS IS A VARIABLE FROM A PREVIOUS VERSION OF THE DATABASE WITH
## ONLY 7K ARTISTS SEARCHED IN THE PRESS
make_press_data <- function(){
  
  require(tidyverse)
  s3 <- initialize_s3()
  f <- s3$get_object(Bucket = "scoavoux", Key = "records_w3/artists.csv")
  artists <- f$Body %>% rawToChar %>% read_csv()
  artists <- mutate(artists, 
                    total_n_pqnt_texte = n_presse_texte_le_figaro + 
                      n_presse_texte_liberation + 
                      n_presse_texte_le_monde + 
                      n_presse_texte_telerama) %>% 
    select(artist_id, total_n_pqnt_texte)
  return(artists)
}
