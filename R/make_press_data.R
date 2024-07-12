# make corpus
# filter corpus 
make_aliases <- function(){
  require(tidyverse)
  require(tidytable)
  require(WikidataQueryServiceR)
  #s3 <- initialize_s3()
  
  # Get aliases from Wikidata
  wikidata_spotifyid_aliases_en <- query_wikidata('SELECT DISTINCT ?spotify_id ?item ?itemLabel ?itemAltLabel
  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language "en". }
    ?item p:P1902 ?statement0.
    ?statement0 (ps:P1902) _:anyValueP1902.
    ?item wdt:P1902 ?spotify_id.
  }')
  wikidata_spotifyid_aliases_fr <- query_wikidata('SELECT DISTINCT ?spotify_id ?item ?itemLabel ?itemAltLabel
  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language "fr". }
    ?item p:P1902 ?statement0.
    ?statement0 (ps:P1902) _:anyValueP1902.
    ?item wdt:P1902 ?spotify_id.
  }')
  wikidata_spotifyid_aliases <- bind_rows(wikidata_spotifyid_aliases_en, 
                                          wikidata_spotifyid_aliases_fr) %>% 
    distinct()
  
  
  wikidata_deezerid_aliases_en <- query_wikidata('SELECT DISTINCT ?deezer_id ?item ?itemLabel ?itemAltLabel
  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language "en". }
    ?item p:P2722 ?statement1.
    ?statement1 (ps:P2722) _:anyValueP2722.
    ?item wdt:P2722 ?deezer_id.
  }')
  wikidata_deezerid_aliases_fr <- query_wikidata('SELECT DISTINCT ?deezer_id ?item ?itemLabel ?itemAltLabel
  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language "fr". }
    ?item p:P2722 ?statement1.
    ?statement1 (ps:P2722) _:anyValueP2722.
    ?item wdt:P2722 ?deezer_id.
  }')
  wikidata_deezerid_aliases <- bind_rows(wikidata_deezerid_aliases_en, 
                                         wikidata_deezerid_aliases_fr) %>% 
    distinct()
  
  wikidata_mbid_aliases_fr <- query_wikidata('SELECT DISTINCT ?mbid ?item ?itemLabel ?itemAltLabel
  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language "fr". }
    ?item p:P434 ?statement0.
    ?statement0 (ps:P434) _:anyValueP434.
    ?item wdt:P434 ?mbid.
  }')
  
  wikidata_mbid_aliases_en <- query_wikidata('SELECT DISTINCT ?mbid ?item ?itemLabel ?itemAltLabel
  WHERE {
    SERVICE wikibase:label { bd:serviceParam wikibase:language "en". }
    ?item p:P434 ?statement0.
    ?statement0 (ps:P434) _:anyValueP434.
    ?item wdt:P434 ?mbid.
  }')
  
  wikidata_mbid_aliases <- bind_rows(wikidata_mbid_aliases_en, 
                                     wikidata_mbid_aliases_fr) %>% 
    distinct()
  
  
}
