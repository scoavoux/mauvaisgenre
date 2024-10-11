make_tbl_coverage <- function(artists_pop, artists){
  require(tidyverse)
  require(tidytable)
  require(kableExtra)
  
  tb <- artists_pop %>% 
    left_join(select(artists, artist_id, genre) %>% mutate(included = TRUE)) %>% 
    mutate(included_in_study = ifelse(is.na(included), FALSE, included)) %>% 
    summarize(n_artists = n(),
              control_f_l_play = sum(control_f_l_play, na.rm = TRUE),
              control_f_n_play = sum(control_f_n_play, na.rm = TRUE), 
              respondent_f_l_play = sum(respondent_f_l_play, na.rm = TRUE),
              respondent_f_n_play = sum(respondent_f_n_play, na.rm = TRUE), 
              .by = included_in_study)

  tb <- artists %>% 
    mutate(has_press = !is.na(leg_exo_press),
           has_score = !is.na(leg_exo_score),
           has_radio = !is.na(leg_exo_radio),
           has_press_score = has_press & has_score,
           has_press_radio = has_press & has_radio,
           has_score_radio = has_score & has_radio,
           has_all = has_press & has_score & has_radio) %>%  
    select(artist_id, genre, starts_with("has")) %>% 
    pivot_longer(starts_with("has")) %>% 
    mutate(name = factor(name, levels = unique(name))) %>% 
    filter(value) %>% 
    inner_join(artists_pop) %>% 
    group_by(name) %>% 
    summarise(n_artists = n(),
              control_f_l_play = sum(control_f_l_play, na.rm = TRUE),
              respondent_f_l_play = sum(respondent_f_l_play, na.rm = TRUE))
  
  kbl(tb, format = "latex", digits = 2, booktabs = TRUE) %>% 
    save_kable(file = "output/omni1/tb_coverage.tex")
  return("output/omni1/tb_coverage.tex")
}

plot_endoleg_bygenre <- function(artists){
  require(tidyverse)
  set_ggplot_options()
  g <- artists %>% 
    mutate(genre = recode_vars(genre, "cleangenres"), 
           genre = fct_reorder(genre, leg_endo_isei, mean)) %>% 
    select(genre, leg_endo_isei, leg_endo_educ) %>% 
    pivot_longer(-genre) %>% 
    mutate(name = recode_vars(name, "cleanlegitimacy"),
           name = str_replace_all(name, "\\\\n", "\n")) %>% 
    ggplot(aes(x= genre, y = value)) +
    geom_boxplot() +
    facet_wrap(~name, scales = "free_x") +
    coord_flip() +
    labs(y = "", x = "")
  ggsave("gg_endoleg_bygenre.pdf", g, path = "output/omni1", device = "pdf")
  return("output/omni1/gg_endoleg_bygenre.pdf")
}

plot_exoleg_bygenre <- function(artists){
  require(tidyverse)
  set_ggplot_options()
  g <- artists %>% 
    mutate(leg_exo_press = log(leg_exo_press+1),
           leg_exo_radio = log(leg_exo_radio+1)) %>%
    mutate(genre = recode_vars(genre, "cleangenres"),
           genre = factor(genre) %>% fct_reorder(leg_exo_score, median)) %>% 
    select(genre, leg_exo_press, leg_exo_score, leg_exo_radio, sc_exo_pca) %>% 
    pivot_longer(-genre) %>% 
    mutate(name = recode_vars(name, "cleanlegitimacy"),
           name = str_replace_all(name, "\\\\n", "\n")) %>% 
    ggplot(aes(genre, value)) +
    geom_boxplot() +
    facet_wrap(~name, scales = "free_x") +
    coord_flip() +
    labs(y="", x="")
  ggsave("gg_exoleg_bygenre.pdf", g, path = "output/omni1", device = "pdf", height = 8)
  return("output/omni1/gg_exoleg_bygenre.pdf")
}

plot_endoexoleg_bygenre <- function(artists, type="density"){
  set_ggplot_options()
  require(tidyverse)
  require(gghalves)
  d <- artists %>% 
    select(genre, starts_with("sc_e")) %>% 
    mutate(genre = recode_vars(genre, "cleangenres"))
  
  if(type == "density") {
    g <- d %>% 
      mutate(genre = factor(genre) %>% fct_reorder(sc_endo_isei, median)) %>% 
      pivot_longer(-genre) %>% 
      mutate(name = recode_vars(name, "cleanlegitimacy"),
             name = str_replace_all(name, "\\\\n", "\n")) %>% 
      ggplot(aes(x= genre, y = value)) +
      geom_violin()
    # geom_half_point(side = "l", size=.7)
  } else if(type == "estimate"){
    d <- d %>% 
      mutate(genre = factor(genre) %>% fct_reorder(sc_endo_isei, mean)) %>% 
      pivot_longer(-genre) %>% 
      mutate(name = recode_vars(name, "cleanlegitimacy"),
             name = str_replace_all(name, "\\\\n", "\n")) %>% 
      group_by(genre, name) %>% 
      summarize(n = n(),
                m = mean(value),
                se = 1.96*sd(value)/sqrt(n()))
    g <- ggplot(d, aes(x = genre, y = m, ymin = m-se, ymax=m+se)) +
      geom_point() +
      geom_errorbar(width = 0)
  }
  
  g <- g + 
    facet_wrap(~name, scales = "free_x") +
    coord_flip() +
    labs(y="", x="")
  ggsave(paste0("gg_endoexoleg_bygenre_", type, ".pdf"), g, path = "output/omni1", device = "pdf", height = 10)
  return(paste0("output/omni1/gg_endoexoleg_bygenre_", type, ".pdf"))
}

plot_endoexoleg_genrerank <- function(artists){
  require(tidyverse)
  set_ggplot_options()
  x <- artists %>% 
    select(genre, starts_with("sc"), -sc_exo_pca) %>% 
    pivot_longer(-genre) %>% 
    filter(!is.na(value)) %>% 
    group_by(genre, name) %>% 
    summarize(m = mean(value)) %>% 
    arrange(name, desc(m)) %>% 
    group_by(name) %>% 
    mutate(rank = row_number()) %>% 
    select(-m) %>% 
    pivot_wider(names_from = name, values_from = rank)
  
  labs <- recode_vars(names(x)[-1], "cleanlegitimacy") %>% 
    str_replace("\\\\n", "\n")
  g <- x %>% 
    #select(genre, sc_endo_isei, sc_exo_pca) %>% 
    pivot_longer(-genre) %>% 
    mutate(genre = recode_vars(genre, "cleangenres")) %>% 
    ggplot(aes(x=name, y=value, group=genre)) +
    geom_point() +
    geom_line() +
    geom_text(aes(x = 1, label=genre), data = ~ filter(.x, name == "sc_endo_educ"), hjust=1.2) +
    geom_text(aes(x = 5, label=genre), data = ~ filter(.x, name == "sc_exo_score"), hjust=-0.2) +
    scale_y_reverse(breaks=1:18, minor_breaks = NULL) +
    scale_x_discrete(labels = labs) +
    labs(y = "Rank", x = "Legitimacy scale")
  ggsave("gg_endoexoleg_genrerank.pdf", g, path = "output/omni1", device = "pdf", width = 10)
  return("output/omni1/gg_endoexoleg_genrerank.pdf")
}

plot_endoexoleg_correlation <- function(artists, genremean=FALSE){
  require(tidyverse)
  require(ggrepel)
  set_ggplot_options()
  x <- select(artists, starts_with("sc_e"), genre) %>% 
    select(-sc_exo_pca) %>% 
    pivot_longer(starts_with("sc_endo"), names_to = "endo", values_to = "endo_value") %>%
    pivot_longer(starts_with("sc_exo"),  names_to = "exo",  values_to = "exo_value") %>% 
    # manual adjustments
    filter(!(exo == "sc_exo_score" & exo_value < -2.5),
           !(exo == "sc_exo_press" & exo_value > 3.5),
           !(exo == "sc_exo_radio" & exo_value > 3)) %>% 
    # correct names
    mutate(exo = recode_vars(exo, "cleanlegitimacy") %>% 
             str_replace("\\\\n", "\n"),
           endo = recode_vars(endo, "cleanlegitimacy") %>% 
             str_replace("\\\\n", "\n"))
  
  g <- ggplot(x, aes(exo_value, endo_value)) +
    geom_point(shape = ".", alpha = .2) +
    #geom_smooth(method = "lm") +
    facet_grid(endo~exo, scales = "free") +
    labs(x = "Gatekeeper legitimacy", y = "Audience status")
  g
  if(genremean){
    gm <- x %>% 
      group_by(endo, exo, genre) %>% 
      summarise(endo_value = mean(endo_value), exo_value = mean(exo_value)) %>% 
      ungroup() %>% 
      mutate(genre = recode_vars(genre, "cleangenres"))
    g <- g +
      geom_point(data = gm, shape = "x") +
      geom_text_repel(aes(label = genre), data = gm, max.overlaps = 50)
    ggsave("gg_endoexoleg_correlation_genremean.pdf", g, path = "output/omni1", device = "pdf", width = 9)
    return("output/omni1/gg_endoexoleg_correlation_genremean.pdf")
  } else {
    ggsave("gg_endoexoleg_correlation.pdf", g, path = "output/omni1", device = "pdf", width = 9)
    return("output/omni1/gg_endoexoleg_correlation.pdf")
  }
}

tbl_endoexoleg_correlation <- function(artists, genremean=FALSE){
  require(tidyverse)
  require(ggrepel)
  require(kableExtra)
  set_ggplot_options()
  x <- select(artists, starts_with("sc_e"), genre) %>% 
    select(-sc_exo_pca) %>% 
    pivot_longer(starts_with("sc_endo"), names_to = "endo", values_to = "endo_value") %>%
    pivot_longer(starts_with("sc_exo"),  names_to = "exo",  values_to = "exo_value") %>% 
    # correct names
    mutate(exo = recode_vars(exo, "cleanlegitimacy") %>% 
             str_replace("\\\\n", " "),
           endo = recode_vars(endo, "cleanlegitimacy") %>% 
             str_replace("\\\\n", " "),
           genre = recode_vars(genre, "cleangenres"))
  if(genremean){
    x <- x %>% 
      group_by(endo, exo, genre) %>% 
      summarise(endo_value = mean(endo_value), exo_value = mean(exo_value)) %>% 
      ungroup()
  }
  al <- x %>% 
    group_by(endo, exo) %>% 
    summarise(cor = cor(endo_value, exo_value)) %>% 
    ungroup() %>% 
    rename(`Gatekeeper legitimacy` = "exo")
  
  caption <- paste0("Pearson correlation coefficient (", ifelse(genremean, "genre", "artist"), " level)")
  
  al %>% mutate(cor = round(cor, 2)) %>% 
    pivot_wider(names_from = endo, values_from = cor) %>% 
    kbl(format = "latex", digits = 2, booktabs = TRUE, caption = caption) %>% 
    save_kable(file = paste0("output/omni1/tb_endo_exo_cor_", ifelse(genremean, "genre", "artist"),".tex"))
  return(paste0("output/omni1/tb_endo_exo_cor_", ifelse(genremean, "genre", "artist"),".tex"))
}

# plot_endoexoleg_correlation2 <- function(artists, 
#                                         genrefacets=FALSE, 
#                                         genremean=FALSE, 
#                                         ncol=3, 
#                                         output="gg_endoexoleg_correlation.pdf",
#                                         .width = 7){
#   require(tidyverse)
#   require(ggrepel)
#   set_ggplot_options()
#   if(genrefacets & genremean) error("genrefacets and genremean cannot both be TRUE")
#   get_r2 <- function(.x, level = "artist"){
#     y <- lm(sc_endo_isei~sc_exo_pca, .x) %>% 
#       summary() %>% 
#       .$r.squared %>% 
#       round(2)
#     if(level == "artist") {
#       res <- paste0("{italic(R)^2} ==", y)
#     } else if(level != "artist"){
#       res <- paste0("{italic(R)^2} (", level, "~level) ==", y)
#     }
#     return(res)
#   }
#   artists <- artists %>% 
#     mutate(genre = recode_vars(genre, "cleangenres")) 
# 
#   labs <- recode_vars(c("sc_endo_isei", "sc_exo_pca"), "cleanlegitimacy") %>% 
#     str_replace("\\\\n", " ")
#   
#   g <- artists %>% 
#     ggplot(aes(sc_exo_pca, sc_endo_isei)) +
#     geom_smooth(se = FALSE, method="lm") +
#     labs(x = labs[2], y = labs[1])
#   if(genremean){
#     gm <- artists %>% 
#       group_by(genre) %>% 
#       summarize(mean_exo_pca = mean(sc_exo_pca), 
#                 mean_endo_isei= mean(sc_endo_isei))
#     g <- g + 
#       geom_point(alpha=.2, color="grey") +
#       geom_point(data = gm, mapping = aes(mean_exo_pca, mean_endo_isei), shape = "x", size=5) +
#       geom_text_repel(data = gm, 
#                       mapping = aes(mean_exo_pca, mean_endo_isei, label = genre), 
#                       max.overlaps=18)
#   } else {
#     g <- g + geom_point()
#   }
#   if(genrefacets){
#     l <- vector("list", length = length(unique(artists$genre)))
#     names(l) <- unique(artists$genre)
#     for(ge in unique(artists$genre)){
#       l[[ge]] <- artists %>% 
#         filter(genre == ge) %>%  
#         get_r2()
#     }
#     lab <- tibble(genre = names(l), r2 = unlist(l))
#     lab <- artists %>% 
#       #     group_by(genre) %>% 
#       summarize(x = max(sc_exo_pca)-.85, y = min(sc_endo_isei)+1) %>% 
#       #      right_join(lab)
#       bind_cols(lab)
#   } else if(!genremean){
#     lab <- tibble(r2 = get_r2(artists),
#                   x = max(artists$sc_exo_pca)-.85, 
#                   y = min(artists$sc_endo_isei)+1)
#   } else if(genremean){
#     r2_genre <- artists %>% 
#       group_by(genre) %>% 
#       summarize(sc_endo_isei = mean(sc_endo_isei),
#                 sc_exo_pca = mean(sc_exo_pca)) %>% 
#       get_r2(level = "genre")
#     lab <- tibble(r2 = r2_genre,
#                   x = max(artists$sc_exo_pca)-.85, 
#                   y = min(artists$sc_endo_isei)+1)
#   }
#   g <- g + 
#     geom_text(data = lab, mapping = aes(x=x, y=y, label = r2), 
#               parse = TRUE, 
#               hjust=.75)
#   if(genrefacets) {
#     g <- g + facet_wrap(~genre, ncol=ncol)
#   }
#   ggsave(output, g, path = "output/omni1", device = "pdf", width = .width)
#   return(paste0("output/omni1/", output))
# }

plot_genre_overlap <- function(artists){
  require(tidyverse)
  set_ggplot_options()
  require(overlapping)
  # We use the measure of overlap proposed by 
  # https://www.frontiersin.org/journals/psychology/articles/10.3389/fpsyg.2019.01089/full
  
  lendo_isei <- lendo_educ <- lexo_score <- lexo_press <- lexo_radio <- lexo_pca <- vector("list", length = length(unique(artists$genre)))
  
  names(lendo_isei) <- names(lendo_educ) <- names(lexo_score) <- names(lexo_press) <- names(lexo_radio) <- names(lexo_pca) <- unique(artists$genre)
  
  for(ge in unique(artists$genre)){
    lendo_isei[[ge]] <- filter(artists, genre == ge, !is.na(sc_endo_isei)) %>% 
      pull(sc_endo_isei)
  }
  
  for(ge in unique(artists$genre)){
    lendo_educ[[ge]] <- filter(artists, genre == ge, !is.na(sc_endo_educ)) %>% 
      pull(sc_endo_educ)
  }
  
  for(ge in unique(artists$genre)){
    lexo_pca[[ge]] <- filter(artists, genre == ge, !is.na(sc_exo_pca)) %>% 
      pull(sc_exo_pca)
  }
  
  for(ge in unique(artists$genre)){
    lexo_score[[ge]] <- filter(artists, genre == ge, !is.na(sc_exo_score)) %>% 
      pull(sc_exo_score)
  }
  
  for(ge in unique(artists$genre)){
    lexo_press[[ge]] <- filter(artists, genre == ge, !is.na(sc_exo_press)) %>% 
      pull(sc_exo_press)
  }
  
  for(ge in unique(artists$genre)){
    lexo_radio[[ge]] <- filter(artists, genre == ge, !is.na(sc_exo_radio)) %>% 
      pull(sc_exo_radio)
  }
  
  # order by mean ENDOGENOUS legitimacy
  # beware... maybe we should use exo
  # or order both list independently
  o <- artists %>% 
    group_by(genre) %>% 
    summarise(m = mean(sc_endo_isei)) %>% 
    arrange(m) 
  
  lendo_isei <- lendo_isei[o$genre]
  lendo_educ <- lendo_educ[o$genre]
  lexo_pca   <- lexo_pca[o$genre]
  lexo_score <- lexo_score[o$genre]
  lexo_press <- lexo_press[o$genre]
  lexo_radio <- lexo_radio[o$genre]
  
  ol_endo_isei <- overlap(lendo_isei, plot = FALSE)
  ol_endo_educ <- overlap(lendo_educ, plot = FALSE)
  ol_exo_pca   <- overlap(lexo_pca, plot = FALSE)
  ol_exo_score <- overlap(lexo_score, plot = FALSE)
  ol_exo_press <- overlap(lexo_press, plot = FALSE)
  ol_exo_radio <- overlap(lexo_radio, plot = FALSE)
  
  dp <- tibble(cp= names(ol_endo_isei$OVPairs),
               ol = ol_endo_isei$OVPairs,
               leg = "sc_endo_isei") %>% 
    bind_rows(tibble(cp= names(ol_endo_educ$OVPairs),
                     ol = ol_endo_educ$OVPairs,
                     leg = "sc_endo_educ")) %>% 
    bind_rows(tibble(cp= names(ol_exo_pca$OVPairs),
                     ol = ol_exo_pca$OVPairs,
                     leg = "sc_exo_pca")) %>% 
    bind_rows(tibble(cp= names(ol_exo_score$OVPairs),
                     ol = ol_exo_score$OVPairs,
                     leg = "sc_exo_score")) %>% 
    bind_rows(tibble(cp= names(ol_exo_press$OVPairs),
                     ol = ol_exo_press$OVPairs,
                     leg = "sc_exo_press")) %>% 
    bind_rows(tibble(cp= names(ol_exo_radio$OVPairs),
                     ol = ol_exo_radio$OVPairs,
                     leg = "sc_exo_radio")) %>% 
    separate(cp, into = c("g1", "g2"), sep = "-") %>% 
    add_count(g1, name = "n1") %>% 
    add_count(g2, name = "n2") %>% 
    arrange(n1, n2) %>% 
    mutate(g1 = recode_vars(g1, "cleangenres") %>% factor(levels = unique(.)),
           g2 = recode_vars(g2, "cleangenres") %>% factor(levels = unique(.)),
           leg = recode_vars(leg, "cleanlegitimacy") %>% 
             str_replace("\\\\n", " "))
  g <- ggplot(dp, aes(g1, g2, fill = ol)) +
    geom_tile() +
    geom_text(aes(label=round(ol, 2) %>% str_replace("0.", "."))) +
    labs(x="", y="") +
    scale_y_discrete(position="right") +
    facet_wrap(~leg) +
    theme(axis.text.x = element_text(angle=45, hjust = 1), line = element_blank(),
          legend.position = "top") +
    guides(fill = "none") +
    scale_fill_distiller(type = "seq",
                         direction = 1,
                         palette = "Greys")
  ggsave("gg_genre_overlap.pdf", g, path = "output/omni1", device="pdf", width = 12, height=9)
  return("output/omni1/gg_genre_overlap.pdf")
}

plot_example_genre_overlap <- function(artists){
  require(tidyverse)
  set_ggplot_options()
  labs <- recode_vars(c("sc_endo_isei"), "cleanlegitimacy") %>% 
    str_replace("\\\\n", " ")
  g <- artists %>% 
    filter(genre %in% c("frenchrap", "rock", "classical")) %>% 
    mutate(genre = recode_vars(genre, "cleangenres")) %>% 
    ggplot(aes(sc_endo_isei, fill = genre)) +
    geom_density(alpha=.5) +
    labs(y = "Density", x = labs, fill = "")
  ggsave("example_overlap.pdf", g, path = "output/omni1", device = "pdf", height = 5)
  return("output/omni1/example_overlap.pdf")
}

table_leg_variance <- function(artists){
  require(knitr)
  require(kableExtra)
  # R-squared from regression of leg ~ genre
  rs <- artists %>% 
    select(starts_with("sc_"), genre) %>% 
    summarise(across(starts_with("sc_"), function(.x){
      y <- lm(.x~genre, data = .) %>% summary()
      y$r.squared
    }))
  rs <- rs %>% 
    pivot_longer(everything(), 
                 names_to = "Variable", 
                 values_to = "R squared") %>% 
    mutate(Variable = recode_vars(Variable, "cleanlegitimacy") %>% 
             str_replace("\\\\n", " "))
  kbl(rs, format = "latex", digits = 2, booktabs = TRUE, linesep = "") %>% 
    save_kable(file = "output/omni1/tb_leg_variance.tex")
  return("output/omni1/tb_leg_variance.tex")
}

plot_endoleg_pca <- function(artists){
  require(tidyverse)
  require(FactoMineR)
  require(factoextra)
  pcad <- filter(artists, !is.na(exo_press), !is.na(exo_score), !is.na(exo_radio))
  pca_mod <- compute_pca(pcad)
  g <- fviz_pca_var(pca_mod)
  ggsave("pca_var.pdf", g, path = "output/omni1", device="pdf")
  return("output/omni1/pca_var.pdf")
}

table_mean_sd_leg <- function(artists){
  require(tidyverse)
  require(kableExtra)
  artists %>% 
    mutate(genre = recode_vars(genre, "cleangenres"), 
           genre = fct_reorder(genre, leg_endo_isei, mean)) %>% 
    select(genre, starts_with("sc_e")) %>% 
    pivot_longer(-genre) %>% 
    mutate(name = recode_vars(name, "cleanlegitimacy"),
           name = str_replace_all(name, "\\\\n", " ")) %>% 
    group_by(genre, name) %>% 
    summarize(m = mean(value),
              sd = sd(value)) %>% 
    arrange(name, m) %>% 
    mutate(genre = factor(genre, levels = unique(genre)),
           v = paste0(round(m, 2), " (", round(sd, 2), ")")) %>% 
    select(-m, -sd) %>% 
    pivot_wider(names_from = name, values_from = v) %>% 
    rename(Genre = "genre") %>% 
    kbl(format = "latex", booktabs = TRUE) %>% 
    save_kable("output/omni1/tb_mean_sd_leg.tex")
  return("output/omni1/tb_mean_sd_leg.tex")
}

plot_senscritique_users_from_quentin_survey <- function(){
  require(tidyverse)
  set_ggplot_options()
  require(readxl)
  s3 <- initialize_s3()
  f <- s3$download_file(Bucket = "scoavoux", 
                        Key = "enquete_quentin/DTBExpCult pour Samuel 160824.xlsx", 
                        Filename = "data/temp/DTBExpCult pour Samuel 160824.xlsx")
  qu <- read_excel("data/temp/DTBExpCult pour Samuel 160824.xlsx")
  rm(f, s3)
  # Make a variable that is TRUE if R declares writing reviews on allocine,
  # senscritique, etc., FALSE if not
  q <- qu %>% 
    mutate(sc = str_detect(Q55tot, "2"),
           # nb of different ways to discover culture
           nb_discover = (ifelse(Q42tot == "13", "", Q42tot) %>% nchar())/2,
           # score cultural outings (cinema, theater, etc.) frequency
           across(starts_with("repQ27"), ~ifelse(.x == 4, 0, .x)),
           score_sortie = repQ27l1 + repQ27l2 + repQ27l3 + repQ27l4 + repQ27l5,
           # score cultural passion (says they are passionante (3), amateur (2), occasional (1), non consumer (0))
           # on five items: films, series, music, video games, books
           across(starts_with("Q39repL"), ~-(.x-4)),
           score_passion_culturelle = Q39repL1 + Q39repL2 + Q39repL3 + Q39repL4 + Q39repL5
    )
  
  ggplot(q, aes(nb_discover, sc)) +
    geom_boxplot()
  ggplot(q, aes(score_sortie, sc)) +
    geom_boxplot()
  ggplot(q, aes(score_passion_culturelle, sc)) + 
    geom_boxplot()
  
  q %>% 
    select(sc, sexe, age9, professionact, recodeCSP9, Q57, Q39repL3) %>% 
    count(sc, Q39repL3) %>% 
    group_by(sc) %>% 
    mutate(f = n / sum(n)) %>% 
    select(-n) %>% 
    pivot_wider(names_from = sc, values_from = f)
  
}
