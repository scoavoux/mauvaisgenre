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
  kbl(tb, format = "latex", digits = 2, booktabs = TRUE) %>% 
    save_kable(file = "output/omni1/tb_coverage.tex")
  return("output/omni1/tb_coverage.tex")
}

plot_endoleg_bygenre <- function(artists){
  set_ggplot_options()
  g <- artists %>% 
    mutate(genre = recode_vars(genre, "cleangenres"), 
           genre = fct_reorder(genre, endo_isei_mean_pond, mean)) %>% 
    select(genre, endo_isei_mean_pond, endo_share_high_education_pond) %>% 
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
  set_ggplot_options()
  g <- artists %>% 
    mutate(total_n_pqnt_texte = log(total_n_pqnt_texte+1),
           radio_leg = log(radio_leg+1)) %>%
    mutate(genre = recode_vars(genre, "cleangenres"),
           genre = factor(genre) %>% fct_reorder(senscritique_meanscore, median)) %>% 
    select(genre, total_n_pqnt_texte, senscritique_meanscore, radio_leg, sc_exo_pca) %>% 
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
  require(gghalves)
  d <- artists %>% 
    select(genre, sc_endo_isei, sc_exo_pca) %>% 
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
  if(type == "density"){
    ggsave("gg_endoexoleg_bygenre_density.pdf", g, path = "output/omni1", device = "pdf")
    return("output/omni1/gg_endoexoleg_bygenre_density.pdf")
  } else if(type == "estimate"){
    ggsave("gg_endoexoleg_bygenre_estimate.pdf", g, path = "output/omni1", device = "pdf")
    return("output/omni1/gg_endoexoleg_bygenre_estimate.pdf")
  }
}

plot_endoexoleg_genrerank <- function(artists){
  require(tidyverse)
  set_ggplot_options()
  x <- artists %>% 
    select(genre, starts_with("sc")) %>% 
    pivot_longer(-genre) %>% 
    filter(!is.na(value)) %>% 
    group_by(genre, name) %>% 
    summarize(m = mean(value)) %>% 
    arrange(name, desc(m)) %>% 
    group_by(name) %>% 
    mutate(rank = row_number()) %>% 
    select(-m) %>% 
    pivot_wider(names_from = name, values_from = rank)
  
  g <- x %>% 
    select(genre, sc_endo_isei, sc_exo_pca) %>% 
    pivot_longer(-genre) %>% 
    mutate(genre = recode_vars(genre, "cleangenres")) %>% 
    ggplot(aes(x=name, y=value, group=genre)) +
    geom_point() +
    geom_line() +
    geom_text(aes(x = 1, label=genre), data = ~ filter(.x, name == "sc_endo_isei"), hjust=1.2) +
    geom_text(aes(x = 2, label=genre), data = ~ filter(.x, name == "sc_exo_pca"), hjust=-0.2) +
    scale_y_reverse(breaks=1:18, minor_breaks = NULL) +
    scale_x_discrete(labels = c("sc_endo_isei"="Endogenous (ISEI)", "sc_exo_pca" = "Exogenous (PCA)")) +
    labs(y = "Rank", x = "Legitimacy scale")
  ggsave("gg_endoexoleg_genrerank.pdf", g, path = "output/omni1", device = "pdf")
  return("output/omni1/gg_endoexoleg_genrerank.pdf")
}

plot_endoexoleg_correlation <- function(artists, 
                                        genrefacets=FALSE, 
                                        genremean=FALSE, 
                                        ncol=3, 
                                        output="gg_endoexoleg_correlation.pdf",
                                        .width = 7){
  require(ggrepel)
  set_ggplot_options()
  if(genrefacets & genremean) error("genrefacets and genremean cannot both be TRUE")
  get_r2 <- function(.x, level = "artist"){
    y <- lm(sc_endo_isei~sc_exo_pca, .x) %>% 
      summary() %>% 
      .$r.squared %>% 
      round(2)
    if(level == "artist") {
      res <- paste0("{italic(R)^2} ==", y)
    } else if(level != "artist"){
      res <- paste0("{italic(R)^2} (", level, "~level) ==", y)
    }
    return(res)
  }
  artists <- artists %>% 
    mutate(genre = recode_vars(genre, "cleangenres")) 
  
  g <- artists %>% 
    ggplot(aes(sc_exo_pca, sc_endo_isei)) +
    geom_smooth(se = FALSE, method="lm") +
    labs(x="Exogenous legitimacy (PCA)", y = "Endogenous legitimacy (ISEI)")
  if(genremean){
    gm <- artists %>% 
      group_by(genre) %>% 
      summarize(mean_exo_pca = mean(sc_exo_pca), 
                mean_endo_isei= mean(sc_endo_isei))
    g <- g + 
      geom_point(alpha=.2, color="grey") +
      geom_point(data = gm, mapping = aes(mean_exo_pca, mean_endo_isei), shape = "x", size=5) +
      geom_text_repel(data = gm, 
                      mapping = aes(mean_exo_pca, mean_endo_isei, label = genre), 
                      max.overlaps=18)
  } else {
    g <- g + geom_point()
  }
  if(genrefacets){
    l <- vector("list", length = length(unique(artists$genre)))
    names(l) <- unique(artists$genre)
    for(ge in unique(artists$genre)){
      l[[ge]] <- artists %>% 
        filter(genre == ge) %>%  
        get_r2()
    }
    lab <- tibble(genre = names(l), r2 = unlist(l))
    lab <- artists %>% 
      #     group_by(genre) %>% 
      summarize(x = max(sc_exo_pca)-.85, y = min(sc_endo_isei)+1) %>% 
      #      right_join(lab)
      bind_cols(lab)
  } else if(!genremean){
    lab <- tibble(r2 = get_r2(artists),
                  x = max(artists$sc_exo_pca)-.85, 
                  y = min(artists$sc_endo_isei)+1)
  } else if(genremean){
    r2_genre <- artists %>% 
      group_by(genre) %>% 
      summarize(sc_endo_isei = mean(sc_endo_isei),
                sc_exo_pca = mean(sc_exo_pca)) %>% 
      get_r2(level = "genre")
    lab <- tibble(r2 = r2_genre,
                  x = max(artists$sc_exo_pca)-.85, 
                  y = min(artists$sc_endo_isei)+1)
  }
  g <- g + 
    geom_text(data = lab, mapping = aes(x=x, y=y, label = r2), 
              parse = TRUE, 
              hjust=.75)
  if(genrefacets) {
    g <- g + facet_wrap(~genre, ncol=ncol)
  }
  ggsave(output, g, path = "output/omni1", device = "pdf", width = .width)
  return(paste0("output/omni1/", output))
}

plot_genre_overlap <- function(artists){
  require(tidyverse)
  set_ggplot_options()
  require(overlapping)
  # We use the measure of overlap proposed by 
  # https://www.frontiersin.org/journals/psychology/articles/10.3389/fpsyg.2019.01089/full
  
  lendo <- lexo <- vector("list", length = length(unique(artists$genre)))
  
  names(lendo) <- unique(artists$genre)
  for(ge in unique(artists$genre)){
    lendo[[ge]] <- filter(artists, genre == ge) %>% pull(sc_endo_isei)
  }
  
  
  lexo <- vector("list", length = length(unique(artists$genre)))
  names(lexo) <- unique(artists$genre)
  for(ge in unique(artists$genre)){
    lexo[[ge]] <- filter(artists, genre == ge) %>% pull(sc_exo_pca)
  }
  
  # order by mean ENDOGENOUS legitimacy
  # beware... maybe we should use exo
  # or order both list independently
  o <- artists %>% 
    group_by(genre) %>% 
    summarise(m = mean(sc_endo_isei)) %>% 
    arrange(m) 
  
  lendo <- lendo[o$genre]
  lexo  <- lexo[o$genre]
  
  ol_endo <- overlap(lendo, plot = FALSE)
  ol_exo  <- overlap(lexo, plot = FALSE)
  
  dp <- tibble(cp= names(ol_endo$OVPairs),
               ol = ol_endo$OVPairs,
               leg = "sc_endo_isei") %>% 
    bind_rows(tibble(cp= names(ol_exo$OVPairs),
                     ol = ol_exo$OVPairs,
                     leg = "sc_exo_pca")) %>% 
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
  ggsave("gg_genre_overlap.pdf", g, path = "output/omni1", device="pdf", width = 11)
  return("output/omni1/gg_genre_overlap.pdf")
}

plot_example_genre_overlap <- function(artists){
  require(tidyverse)
  set_ggplot_options()
  g <- artists %>% 
    filter(genre %in% c("frenchrap", "rock", "classical")) %>% 
    mutate(genre = recode_vars(genre, "cleangenres")) %>% 
    ggplot(aes(sc_endo_isei, fill = genre)) +
      geom_density(alpha=.5) +
      labs(y = "Density", x = "Endogenous legitimacy (ISEI)", fill = "")
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
  pca_mod <- compute_pca(artists)
  g <- fviz_pca_var(pca_mod)
  ggsave("pca_var.pdf", g, path = "output/omni1", device="pdf")
  return("output/omni1/pca_var.pdf")
}

table_mean_sd_leg <- function(artists){
  require(tidyverse)
  require(kableExtra)
  artists %>% 
    mutate(genre = recode_vars(genre, "cleangenres"), 
           genre = fct_reorder(genre, endo_isei_mean_pond, mean)) %>% 
    select(genre, sc_endo_isei, sc_exo_pca) %>% 
    pivot_longer(-genre) %>% 
    mutate(name = recode_vars(name, "cleanlegitimacy"),
           name = str_replace_all(name, "\\\\n", " ")) %>% 
    group_by(genre, name) %>% 
    summarize(v = paste0(round(mean(value), 2), " (", round(sd(value), 2), ")")) %>% 
    pivot_wider(names_from = name, values_from = v) %>% 
    mutate(l = as.numeric(str_extract(`Endogenous legitimacy (ISEI)`, "^-?[\\d\\.]+"))) %>% 
    arrange(l) %>% 
    select(-l) %>% 
    rename(Genre = "genre") %>% 
    kbl(format = "latex", booktabs = TRUE) %>% 
    save_kable("output/omni1/tb_mean_sd_leg.tex")
  return("output/omni1/tb_mean_sd_leg.tex")
}
