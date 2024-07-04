# Make hashed id / ISEI  pairing ------
make_isei_data <- function(survey_raw){
  # Packages and data ------
  require(tidyverse)
  require(tidytable)
  require(janitor)
  require(readxl)
  
  s3 <- initialize_s3()
  s3$download_file(Bucket = "scoavoux", 
                   Key = "PCS2020/L66_Matrice_codification_PCS2020_collecte_2023.xlsx", 
                   Filename = "data/L66_Matrice_codification_PCS2020_collecte_2023.xlsx")
  pcs <- read_excel("data/L66_Matrice_codification_PCS2020_collecte_2023.xlsx", sheet = 2, skip=8)
  
  pcs <- clean_names(pcs) %>% 
    filter(libm != "2023") %>% 
    select(libm:libf) %>% 
    pivot_longer(names_to = "sexe", values_to = "orig_pcs_prof") %>% 
    distinct(orig_pcs_prof)
  
  # Cleaning up data ------
  clean_up <- function(string){
    string %>% 
      stringi::stri_trans_general("Latin-ASCII") %>% 
      tolower() %>% 
      str_replace_all(c("[-,\\.']" = " ")) %>% 
      str_replace_all(c("\\s+" = " "))
  }
  
  pcs <- pcs %>% 
    distinct(orig_pcs_prof) %>% 
    mutate(pcs_prof = clean_up(orig_pcs_prof))
  
  survey_raw <- select(survey_raw, hashed_id, E_FR_prof_femme:E_FR_prof_retr_homme) %>% 
    pivot_longer(-hashed_id, values_to = "orig_survey_prof") %>% 
    filter(orig_survey_prof != "") %>% 
    mutate(orig_survey_prof = str_trim(orig_survey_prof)) %>% 
    select(-name) %>% 
    right_join(survey_raw)
  
  all_profs <- survey_raw %>%  
    count(orig_survey_prof) %>% 
    mutate(survey_prof = clean_up(orig_survey_prof)) %>% 
    filter(!is.na(orig_survey_prof))
  
  # there are 4735 different labels for 10412 individuals
  nrow(all_profs)
  objective <- sum(all_profs$n)
  objective
  
  # Finding matches ------
  ## Very much incomplete
  matches <- inner_join(all_profs, rename(pcs, survey_prof = "pcs_prof")) %>% 
    select(-survey_prof) %>% 
    mutate(niveau = 4, 
           type="exact match")
  
  # exact match finds 2509 for 7330 individuals
  nrow(matches)
  sum(matches$n)
  
  # we export and go through openrefine
  f <- s3$get_object(Bucket = "scoavoux", Key = "records_w3/survey/pcs_openrefine1.csv")
  or1 <- f$Body %>% rawToChar() %>% read_csv()
  
  or1 <- or1 %>% select(-survey_prof)
  or1 <- or1 %>% 
    filter(!(orig_survey_prof %in% matches$orig_survey_prof))
  
  matches <- or1 %>% 
    mutate(niveau=4,
           type = "openrefine_handcoded") %>% 
    bind_rows(matches)
  
  # with first openrefine wave we are at 2787 labels for 7813 individuals
  nrow(matches)
  sum(matches$n)
  sum(matches$n)/objective
  
  # Handcoding by research assistants
  
  
  #TODO: encore 25% de statuts à récupérer
  ## Ongoing
  # all_profs <- filter(all_profs, !(orig_survey_prof %in% matches$orig_survey_prof))
  # 
  # survey_raw %>%
  #   filter(orig_survey_prof %in% all_profs$orig_survey_prof) %>%
  #   select(orig_survey_prof, E_statut_pub_priv:E_encadre) %>%
  #   left_join(all_profs) %>%
  #   select(-orig_survey_prof, -n) %>%
  #   count(survey_prof, E_statut_pub_priv, E_taille_entreprise, E_position_pub, E_position_priv, E_encadre) %>%
  #   arrange(desc(n)) %>%
  #   write_csv("profs.csv")
  # Récupérer les codes
  library(googlesheets4)
  gs4_auth(email = "samuel.coavoux@gmail.com")
  recoprof <- read_sheet("1DcVZkiNRS9XbLFJxl_qRtbF1Um9xKkothWOLuUb28vw") %>% 
    filter(!is.na(PCS4))
  recoprof <- recoprof %>% 
    mutate(
      E_statut_pub_priv = ifelse(is.na(E_statut_pub_priv), "", E_statut_pub_priv),
      E_taille_entreprise = ifelse(is.na(E_taille_entreprise), "", E_taille_entreprise),
      E_position_pub = ifelse(is.na(E_position_pub), "", E_position_pub),
      E_position_priv = ifelse(is.na(E_position_priv), "", E_position_priv),
      E_encadre = ifelse(is.na(E_encadre), "", E_encadre)
    )
  recoprof <- recoprof %>% 
    select(-n, -PCS3, -PCS2, -Inclassable)
  
  pppcs <- read_excel("data/L66_Matrice_codification_PCS2020_collecte_2023.xlsx", sheet = 2, skip=8)
  pppcs <- pppcs %>% 
    slice(-1) %>% 
    select(-libf, -liste, -natlib) %>% 
    pivot_longer(-libm) %>% 
    filter(!is.na(value)) %>% 
    group_by(value) %>% 
    slice(1) %>% 
    select(clean_pcs_prof = "libm", PCS4 = "value")
  handcoded <- survey_raw %>% 
    select(hashed_id, orig_survey_prof, E_statut_pub_priv, E_taille_entreprise,
           E_position_pub, E_position_priv, E_encadre) %>% 
    left_join(select(all_profs, -n)) %>% 
    inner_join(recoprof) %>% 
    select(hashed_id, PCS4) %>% 
    left_join(pppcs) %>% 
    select(-PCS4)
  
  (sum(matches$n) + nrow(handcoded))/objective
  
  # Match databases ------
  survey_raw <-  matches %>% 
    select(orig_survey_prof, clean_pcs_prof = orig_pcs_prof, pcs_niveau = niveau) %>% 
    right_join(survey_raw) %>% 
    left_join(rename(handcoded, clean_pcs_prof2 = "clean_pcs_prof")) %>% 
    mutate(clean_pcs_prof = ifelse(is.na(clean_pcs_prof), clean_pcs_prof2, clean_pcs_prof)) %>% 
    select(-clean_pcs_prof2)
  
  ## Prepare PCS encoding table
  pcs_cod <- read_xlsx("data/L66_Matrice_codification_PCS2020_collecte_2023.xlsx", sheet = 2, skip = 8) %>% filter(libm != "2023") %>% 
    select(-liste, -natlib, -codeu) %>% 
    pivot_longer(libm:libf, values_to = "clean_pcs_prof") %>% 
    select(-name) %>% 
    pivot_longer(-clean_pcs_prof, names_to = "condition_pcs", values_to = "pcs4") %>% 
    distinct()
  
  ## Prepare ISCO encoding table
  s3$download_file(Bucket = "scoavoux", 
                   Key = "PCS2020/L72_Matrice_codification_ISCO_collecte_2023.xlsx", 
                   Filename = "data/L72_Matrice_codification_ISCO_collecte_2023.xlsx")
  
  isco_cod <- read_xlsx("data/L72_Matrice_codification_ISCO_collecte_2023.xlsx", sheet = 2, skip = 7) %>% 
    filter(libm != "2023") %>% 
    select(-id, -liste, -codeu) %>% 
    pivot_longer(libm:libf, values_to = "clean_pcs_prof") %>% 
    select(-name) %>% 
    pivot_longer(-clean_pcs_prof, names_to = "condition_isco", values_to = "isco4") %>% 
    distinct()
  
  profs <- select(survey_raw, hashed_id, clean_pcs_prof, E_statut_pub_priv:E_position_priv, E_encadre) %>% 
    filter(!is.na(clean_pcs_prof)) %>% 
    mutate(across(everything(), ~ifelse(.x == "", NA, .x))) %>% 
    mutate(condition_pcs = case_when(E_statut_pub_priv == "Vous travaill(i)ez, sans être rémunéré(e), avec un membre de votre famille" ~ "aid_fam", 
                                     E_statut_pub_priv == "À votre compte (y compris gérant de société ou chef d’entreprise salarié)" &
                                       E_taille_entreprise %in% c("Une seule personne : vous travaillez seul(e) / vous travailliez seul(e)",
                                                                  "Entre 2 et 10 personnes") ~ "inde_0_9", 
                                     E_statut_pub_priv == "À votre compte (y compris gérant de société ou chef d’entreprise salarié)" &
                                       E_taille_entreprise == "Entre 11 et 49 personnes" ~ "inde_10_49", 
                                     E_statut_pub_priv == "À votre compte (y compris gérant de société ou chef d’entreprise salarié)" &
                                       E_taille_entreprise == "50 personnes ou plus" ~ "inde_sup49", 
                                     E_statut_pub_priv == "À votre compte (y compris gérant de société ou chef d’entreprise salarié)" ~ "inde_nr", 
                                     E_statut_pub_priv %in% c("Salarié(e) d’une entreprise (y compris d’une association ou de la Sécurité sociale)", "Salarié(e) d’un particulier") &
                                       E_position_priv == "Ingénieur(e), cadre d’entreprise" ~ "priv_cad", 
                                     E_statut_pub_priv %in% c("Salarié(e) d’une entreprise (y compris d’une association ou de la Sécurité sociale)", "Salarié(e) d’un particulier") &
                                       E_position_priv == "Technicien(ne)" ~ "priv_tec", 
                                     E_statut_pub_priv %in% c("Salarié(e) d’une entreprise (y compris d’une association ou de la Sécurité sociale)", "Salarié(e) d’un particulier") &
                                       E_position_priv == "Agent de maîtrise (y compris administrative ou commerciale)" ~ "priv_am", 
                                     E_statut_pub_priv %in% c("Salarié(e) d’une entreprise (y compris d’une association ou de la Sécurité sociale)", "Salarié(e) d’un particulier") &
                                       E_position_priv == "Employé(e) de bureau, de commerce, de services" ~ "priv_emp", 
                                     E_statut_pub_priv %in% c("Salarié(e) d’une entreprise (y compris d’une association ou de la Sécurité sociale)", "Salarié(e) d’un particulier") & 
                                       E_position_priv == "Ouvrier (ouvrière) qualifié(e), technicien(ne) d'atelier"~ "priv_oq", 
                                     E_statut_pub_priv %in% c("Salarié(e) d’une entreprise (y compris d’une association ou de la Sécurité sociale)", "Salarié(e) d’un particulier") & 
                                       E_position_priv == "Manœuvre, ouvrier (ouvrière) spécialisé(e)"~ "priv_opq", 
                                     E_statut_pub_priv %in% c("Salarié(e) d’une entreprise (y compris d’une association ou de la Sécurité sociale)", "Salarié(e) d’un particulier") ~ "priv_nr", 
                                     E_statut_pub_priv == "Salarié(e) de la fonction publique (État, territoriale ou hospitalière)" &
                                       E_position_pub == "Agent de catégorie A de la fonction publique" ~ "pub_catA", 
                                     E_statut_pub_priv == "Salarié(e) de la fonction publique (État, territoriale ou hospitalière)" &
                                       E_position_pub %in% c("Agent de catégorie B de la fonction publique", "Technicien(ne)") ~ "pub_catB", 
                                     E_statut_pub_priv == "Salarié(e) de la fonction publique (État, territoriale ou hospitalière)" &
                                       E_position_pub %in% c("Agent de catégorie C de la fonction publique",
                                                             "Ouvrier (ouvrière) qualifié(e)",
                                                             "Manœuvre, ouvrier (ouvrière) spécialisé(e)") ~ "pub_catC", 
                                     E_statut_pub_priv == "Salarié(e) de la fonction publique (État, territoriale ou hospitalière)" ~ "pub_nr", 
                                     TRUE ~ "ssvaran"),
           condition_isco = case_when(E_position_priv == "Ingénieur(e), cadre d’entreprise" | E_position_pub == "Agent de catégorie A de la fonction publique" & E_encadre %in% c("Oui, mais ce n’est pas (ce n’était pas) ma tâche principale", "Oui, et c’est (c’était) ma tâche principale") ~ "ingcad_supv", 
                                      E_position_priv == "Ingénieur(e), cadre d’entreprise" | E_position_pub == "Agent de catégorie A de la fonction publique" & E_encadre == "Non" ~ "ingcad_nsupv", 
                                      E_position_priv %in% c("Technicien(ne)", "Agent de maîtrise (y compris administrative ou commerciale)") | E_position_pub %in% c("Agent de catégorie B de la fonction publique", "Technicien(ne)") & E_encadre %in% c("Oui, mais ce n’est pas (ce n’était pas) ma tâche principale", "Oui, et c’est (c’était) ma tâche principale") ~ "tecam_supv", 
                                      E_position_priv %in% c("Technicien(ne)", "Agent de maîtrise (y compris administrative ou commerciale)") | E_position_pub %in% c("Agent de catégorie B de la fonction publique", "Technicien(ne)") & E_encadre == "Non" ~ "tecam_nsupv", 
                                      E_position_priv %in% c("Employé(e) de bureau, de commerce, de services", "Ouvrier (ouvrière) qualifié(e), technicien(ne) d'atelier", "Manœuvre, ouvrier (ouvrière) spécialisé(e)") | E_position_pub %in% c("Agent de catégorie C de la fonction publique", "Ouvrier (ouvrière) qualifié(e)", "Manœuvre, ouvrier (ouvrière) spécialisé(e)") & E_encadre %in% c("Oui, mais ce n’est pas (ce n’était pas) ma tâche principale", "Oui, et c’est (c’était) ma tâche principale") ~ "ouvemp_supv", 
                                      E_position_priv %in% c("Employé(e) de bureau, de commerce, de services", "Ouvrier (ouvrière) qualifié(e), technicien(ne) d'atelier", "Manœuvre, ouvrier (ouvrière) spécialisé(e)") | E_position_pub %in% c("Agent de catégorie C de la fonction publique", "Ouvrier (ouvrière) qualifié(e)", "Manœuvre, ouvrier (ouvrière) spécialisé(e)") & E_encadre == "Non" ~ "ouvemp_nsupv", 
                                      E_encadre %in% c("Oui, mais ce n’est pas (ce n’était pas) ma tâche principale", "Oui, et c’est (c’était) ma tâche principale") ~ "nr_supv", 
                                      E_encadre == "Non" ~ "nr_nsupv", 
                                      E_statut_pub_priv == "À votre compte (y compris gérant de société ou chef d’entreprise salarié)" &
                                        E_taille_entreprise %in% c("Une seule personne : vous travaillez seul(e) / vous travailliez seul(e)",
                                                                   "Entre 2 et 10 personnes") ~ "inde_0_9", 
                                      E_statut_pub_priv == "À votre compte (y compris gérant de société ou chef d’entreprise salarié)" &
                                        E_taille_entreprise %in% c("Entre 11 et 49 personnes", "50 personnes ou plus") ~ "inde_sup10", 
                                      E_statut_pub_priv == "À votre compte (y compris gérant de société ou chef d’entreprise salarié)" ~ "inde_nr", 
                                      TRUE ~ "ssvaran")
    ) %>% 
    select(hashed_id, clean_pcs_prof, starts_with("condition_"))
  
  f <- s3$get_object(Bucket = "scoavoux", Key = "PCS2020/isco_isei.csv")
  isco_isei <- f$Body %>% rawToChar() %>% read_csv() %>% 
    rename(isco4 = "isco")
  
  result <- profs %>% 
    left_join(pcs_cod, by = c("clean_pcs_prof", "condition_pcs")) %>% 
    left_join(isco_cod, by = c("clean_pcs_prof", "condition_isco")) %>% 
    left_join(isco_isei, by = "isco4") %>% 
    mutate(pcs1 = str_extract(pcs4, "^\\w"),
           pcs2 = str_extract(pcs4, "^\\w{2}"),
           pcs3 = str_extract(pcs4, "^\\w{3}"),
           isco1 = str_extract(isco4, "^\\w"),
           isco2 = str_extract(isco4, "^\\w{2}"),
           isco3 = str_extract(isco4, "^\\w{3}")) %>% 
    select(hashed_id, isei)
  
  return(result)
}