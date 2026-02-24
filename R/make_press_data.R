
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
  
  clean_dates <- function(char){
    str_replace_all(char, c(
      "[Jj]anvier"    = "01",
      "[Ff][ée]vrier" = "02",
      "[Mm]ars"       = "03",
      "[Aa]vril" 		= "04",
      "[Mm]ai" 		= "05",
      "[Jj]uin" 		= "06",
      "[Jj]uillet" 	= "07",
      "[Aa]o[uû]t" 	= "08",
      "[Ss]eptembre" 	= "09",
      "[Oo]ctobre" 	= "10",
      "[Nn]ovembre" 	= "11",
      "[Dd][ée]cembre"= "12"
    ))
  }
  
  telerama <- mutate(telerama, 
                     date = str_remove(date, "vendredi|samedi") %>% clean_dates() %>% dmy())
  
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
  # there are some small errors (bad dates in parsed data); we correct them below
  for(i in 1:12){
    lemonde[[i]] <- s3_read(paste0("french_media/lemonde/", paste0("lemonde-20", 10L:22L,".csv"))[i])
    print(i)
  }

  lemonde <- lapply(lemonde, function(x) {
    x <- mutate(x,
           is_article = as.logical(is_article),
           annee = as.numeric(annee),
           publidate = ymd(publidate)) %>% 
      fill(publidate)
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
  lefigaro <- filter(lefigaro, year > 2009)
  
  lefigaro <- filter(lefigaro, rubrique == "Culture")
  lefigaro <- mutate(lefigaro, date = as.Date(date))
  
  lefigaro <- lefigaro %>% 
    select(date, 
           section = "rubrique",
           article_title = "titre",
           article_author = "auteur",
           article_text = "texte"
    ) %>% 
    mutate(source_name = "Le Figaro")
  file.remove("data/temp/lefigaro-complet-v0.csv")
  
  
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
    filter(!is.na(article_text)) %>% 
    mutate(article_text = paste(article_title, ".\n", article_text))
  corpus_raw <- corpus_raw %>% 
    mutate(id = row_number())
  return(corpus_raw)
}

# This is a part where we break the targets workflow because we need a GPU
# and python. Make_corpus_for_BERT prepares the corpus in R. Then we
# will run BERT in python. Then we export to s3 the results as a csv
# and reimport them in R in the next function
# Since it requires manual work anyway I'm not bothering with export/import to
# and from s3 of intermediate files.
make_corpus_for_BERT <- function(corpus_raw){
  require(tidyverse)
  ma <- s3_read("french_media/music_reviews_manual_annotations.csv") %>% 
    select(date, article_title, article_author, review)
  # merge
  # There is a size limit for BERT. Truncate the corpus
  # a cutoff at 10000 characters keeps intact 98% of the corpus and just cuts 
  # the remaining 2% of all articles
  # mutate(corpus_raw, l = str_length(article_text))$l %>% quantile(c(.25, .40, .5, .75, .80, .90, .95, .97, .98, .99))
  corpus_raw <- corpus_raw %>% 
    mutate(article_text = str_trunc(article_text, 10000))
  # traintest <- corpus_raw %>% 
  #   inner_join(ma) %>% 
  #   select(id, article_text, review) %>% 
  #   # randomiser traintest
  #   mutate(traintest = sample(c("train", "test"), nrow(.), prob = c(.8, .2), replace = TRUE))
  # # écrire le résultat
  # tag <- corpus_raw %>% 
  #   anti_join(ma) %>% 
  #   select(id, article_text)
  # filter(traintest, traintest == "train") %>% 
  #   select(-traintest) %>% 
  #   write_csv("data/temp/bert_train.csv")
  # filter(traintest, traintest == "test") %>% 
  #   select(-traintest) %>% 
  #   write_csv("data/temp/bert_test.csv")
  # tag %>% 
  #   write_csv("data/temp/bert_tag.csv")
  corpus_raw %>% 
    select(id, article_text, article_title, date) %>% 
    left_join(ma) %>% 
    arrange(review) %>% 
    select(id, article_text, review) %>% 
    write_csv("data/temp/bert_tag.csv")
  return("data/temp/bert_tag.csv")
}
# now, make a BERT.

# Importing tags from BERT and only keep music reviews
filter_corpus_raw <- function(corpus_raw, BERT_corpus){
  require(tidyverse)
  tags <- s3_read("french_media/music_review_BERT_tags.csv")
  tags <- tags %>% 
    mutate(review = ifelse(bertpred_music_review > .5, "music_review", "not_music_review")) %>% 
    select(id, review)
  cp <- corpus_raw %>% 
    left_join(tags) %>% 
    filter(review == "music_review") %>% 
    select(-review)
  return(cp)
}

# Make aliases ------
make_aliases <- function(senscritique_mb_deezer_id, artists_pop){
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
    left_join(select(artists_pop, artist_id, control_n_users, respondent_n_users)) %>% 
    filter(!is.na(control_n_users) & is.na(respondent_n_users)) %>% 
    mutate(consolidated_artist_id = ifelse(is.na(consolidated_artist_id), artist_id, consolidated_artist_id)) %>% 
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
  regexify <- function(str){
    str %>% 
      #stringi::stri_trans_general(id = "Latin-ASCII") %>% 
      # escape regex special character
      str_replace_all(fixed("\\"), "\\\\") %>%
      str_replace_all(fixed("."), "\\.") %>%
      str_replace_all(fixed("+"), "\\+") %>%
      str_replace_all(fixed("*"), "\\*") %>%
      str_replace_all(fixed("?"), "\\?") %>%
      str_replace_all(fixed("^"), "\\^") %>%
      str_replace_all(fixed("$"), "\\$") %>%
      str_replace_all(fixed("("), "\\(") %>%
      str_replace_all(fixed(")"), "\\)") %>%
      str_replace_all(fixed("["), "\\[") %>%
      str_replace_all(fixed("]"), "\\]") %>%
      str_replace_all(fixed("{"), "\\{") %>%
      str_replace_all(fixed("}"), "\\}") %>%
      str_replace_all(fixed("|"), "\\|") %>%
      str_replace_all(fixed("-"), "\\-") %>%
      str_replace_all(fixed("\'"), ".") %>% 
      ifelse(str_detect(substr(., 1, 1), "\\w"), paste0("\\b", .), .) %>% 
      ifelse(str_detect(substr(., nchar(.), nchar(.)), "\\w"), paste0(., "\\b"), .) %>% 
      str_replace_all(c("é" = "[ée]",
                        "\\b[Tt]he\\b" = "([Tt]he|[Ll]es)",
                        "^[Tt]he\\b" = "([Tt]he|[Ll]es)",
                        "\\b[Aa]nd\\b|\\b[Ee]t\\b|&" = "([Aa]nd|[Ee]t|&)"))
  }
  
  artist_names_and_aliases <- consolidated_id_name_alias %>% 
    mutate(regex = regexify(name))
  return(artist_names_and_aliases)
  
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

make_artists_names <- function(artist_names_and_aliases){
  require(tidyverse)
  artist_names <- artist_names_and_aliases %>% 
    mutate(type = factor(type, levels = c("deezername", "name", "alias"))) %>% 
    arrange(type) %>% 
    slice(1, .by = artist_id) %>% 
    select(artist_id, name)
  s3 <- initialize_s3()
  
  ## Names from deezer
  s3$download_file(Bucket = "scoavoux", 
                   Key = "records_w3/items/artists_data.snappy.parquet",
                   Filename = "data/temp/artists_data.snappy.parquet")
  
  dz_names <- read_parquet("data/temp/artists_data.snappy.parquet", col_select = 1:2) %>% 
    rename(dz_name = "name")
  artist_names <- left_join(artist_names, dz_names) %>% 
    mutate(name = ifelse(!is.na(dz_name), dz_name, name)) %>% 
    select(-dz_name)
  
  return(artist_names)
}

make_corpus_tokenized_paragraphs <- function(corpus_raw){
  require(tidyverse)
  co <- corpus_raw %>% 
    select(article_text) %>% 
    separate_rows(article_text, sep = "\n") %>% 
    mutate(article_text = str_trim(article_text)) %>% 
    filter(article_text != "")
  return(co)
}

# Make press data ------

make_press_data <- function(corpus_tokenized, artist_names_and_aliases, regex_fixes_file){
  require(tidyverse)
  require(parallel)
  require(foreach)
  require(doParallel)
  
  # # for now we filter by exo_senscritique because this is the bottleneck
  # # and running this function already takes forever
  # artist_names_and_aliases <- artist_names_and_aliases %>% 
  #   inner_join(select(exo_senscritique, artist_id))
  
  # Apply regex fixes
  regex_fixes <- regex_fixes_file %>% 
    select(-total_n_pqnt_texte)
  
  artist_names_and_aliases <- artist_names_and_aliases %>% 
    anti_join(regex_fixes, by = "artist_id")
  
  regex_fixes <- regex_fixes %>% 
    filter(type != "remove")
  
  artist_names_and_aliases <- artist_names_and_aliases %>% 
    bind_rows(regex_fixes)
  
  # We clean up the regexes a bit
  artist_names_and_aliases <- artist_names_and_aliases %>% 
    filter(
      str_length(name)>1,# remove names of length 1
      !str_detect(name, "^\'*[a-zA-Zéèê]{1,2}\'*$"), # remove names of two letters
      !str_detect(name, "^\\d+$"),# and those of just numbers
      !str_detect(name, "^[\u0621-\u064A]+$"), # those only in arabic
      # And those only in non-ascii characters (ie japanese, chinese, 
      # arabic, korean, russian, greek alphabets)
      !str_detect(name, "^[^ -~]+$")
    )
  # Finally we group various aliases together to speed up search
  artist_names_and_aliases <- artist_names_and_aliases %>% 
    group_by(artist_id) %>% 
    summarise(regex = paste(regex, collapse = "|"),
              negative = any(type == "negative"))
  
  # When there is a negative lookbefore/ahead we cant use xan, so good
  # old str_detect
  artist_names_and_aliases_negative <- artist_names_and_aliases %>% 
    filter(negative)
  artist_names_and_aliases <- artist_names_and_aliases %>% 
    filter(!negative)
  results_negative <- vector("list", length = nrow(artist_names_and_aliases_negative))
  for(i in seq_len(nrow(artist_names_and_aliases_negative))) {
    x <- str_detect(corpus_tokenized, artist_names_and_aliases_negative$regex[i]) %>% 
      sum()
    results_negative[[i]] <- tibble(artist_id = artist_names_and_aliases_negative$artist_id[i],
                                    total_n_pqnt_texte = as.numeric(x))
  }
  results_negative <- bind_rows(results_negative)
  
  # Function to search and count (with xan)
  ## Needs rust and xan installed, see init.sh
  xan_count_matches <- function(.pattern, .corpus = "~/work/mauvaisgenre/data/temp/corpus_tokenized.csv"){
    system(stringr::str_glue("~/.cargo/bin/xan search -r '{.pattern}' {.corpus} | ~/.cargo/bin/xan count"),
           intern = TRUE)
  }
  # Tried to speed that up with grep but same performance (checked with microbenchmark) 
  # grep_search <- function(.pattern, .corpus = "~/work/mauvaisgenre/data/temp/corpus_tokenized.csv"){
  #   system(stringr::str_glue("grep -E -i '{.pattern}' {.corpus} | wc -l"),
  #          intern = TRUE)
  # }
  
  # We need the text as a csv file
  tidytable::fwrite(tibble(corpus_tokenized), "data/temp/corpus_tokenized.csv")
  
  
  # Set up parrallel computing
  # To play it safe we leave 20 cores (on a 128 core machine)
  #n.cores <- parallel::detectCores()-20
  n.cores = 40 # ok let's try that
  if(n.cores < 1) n.cores <- 1
  
  my.cluster <- makeCluster(
    n.cores#, type = "FORK"
  )
  
  registerDoParallel(cl = my.cluster)
  
  iterator <- artist_names_and_aliases %>% 
    nrow()
  
  # Perform search
  # library(tictoc)
  # 
  # tic()
  x <- foreach(
    i = seq_len(iterator),
    .combine = "c"
    ) %dopar% {
    xan_count_matches(artist_names_and_aliases$regex[i])
  }
  # toc()
  stopCluster(my.cluster)
  res <- tibble(artist_id = artist_names_and_aliases$artist_id,
                total_n_pqnt_texte = as.numeric(x)) %>% 
    bind_rows(results_negative)
  return(res)
}