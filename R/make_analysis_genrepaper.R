plot_endoleg_bygenre <- function(artists){
  set_ggplot_options()
  g <- artists %>% 
    mutate(genre = recode_vars(genre, "genres"), 
           genre = fct_reorder(genre, endo_isei_mean_pond, mean)) %>% 
    select(genre, endo_isei_mean_pond, endo_share_high_education_pond) %>% 
    pivot_longer(-genre) %>% 
    mutate(name = recode_vars(name, "legitimacy")) %>% 
    ggplot(aes(x= genre, y = value)) +
      geom_boxplot() +
      facet_wrap(~name, scales = "free_x") +
      coord_flip() +
      labs(y="", x="")
  return(g)
}

plot_exoleg_bygenre <- function(artists){
  set_ggplot_options()
  g <- artists %>% 
    mutate(total_n_pqnt_texte = log(total_n_pqnt_texte+1),
           radio_leg = log(radio_leg+1)) %>%
    mutate(genre = recode_vars(genre, "genres"),
           genre = factor(genre) %>% fct_reorder(senscritique_meanscore, median)) %>% 
    select(genre, total_n_pqnt_texte, senscritique_meanscore, radio_leg, sc_exo_pca) %>% 
    pivot_longer(-genre) %>% 
    mutate(name = recode_vars(name, "legitimacy")) %>% 
    ggplot(aes(genre, value)) +
      geom_boxplot() +
      facet_wrap(~name, scales = "free_x") +
      coord_flip() +
      lims(y = c(0, 10)) +
      labs(y="", x="")
  return(g)
}

plot_endoexoleg_bygenre <- function(artists, type="density"){
  set_ggplot_options()
  require(gghalves)
  d <- artists %>% 
    select(genre, sc_endo_isei, sc_exo_pca) %>% 
    mutate(genre = recode_vars(genre, "genres"),
           genre = factor(genre) %>% fct_reorder(sc_endo_isei, median)) %>% 
    pivot_longer(-genre) %>% 
    mutate(name = recode_vars(name, "legitimacy"))
  
  if(type == "density") {
    g <- ggplot(d, aes(x= genre, y = value)) +
      geom_half_violin(side="t")
      # geom_half_point(side = "l", size=.7)
  } else if(type == "estimate"){
    d <- d %>% 
      group_by(genre, name) %>% 
      summarize(m = mean(value),
                se = 1.96*sd(value))
    g <- ggplot(d, aes(x = genre, y = m, ymin = m-se, ymax=m+se)) +
      geom_point() +
      geom_errorbar(width = 0)
  }
  
  g <- g + 
    facet_wrap(~name) +
    coord_flip() +
    labs(y="", x="")
  return(g)
}

plot_endoexoleg_genrerank <- function(artists){
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
    mutate(genre = recode_vars(genre, "genres")) %>% 
    ggplot(aes(x=name, y=value, group=genre)) +
      geom_point() +
      geom_line() +
      geom_text(aes(x = 1, label=genre), data = ~ filter(.x, name == "sc_endo_isei"), hjust=1.2) +
      geom_text(aes(x = 2, label=genre), data = ~ filter(.x, name == "sc_exo_pca"), hjust=-0.2) +
      scale_y_reverse(breaks=1:18, minor_breaks = NULL) +
      scale_x_discrete(labels = c("sc_endo_isei"="Endogenous (ISEI)", "sc_exo_pca" = "Exogenous (PCA)")) +
      labs(y = "Rank", x = "Legitimacy scale")
  return(g)
}

plot_endoexoleg_correlation <- function(artists, genrefacets=FALSE, genremean=FALSE, ncol=3){
  require(ggrepel)
  set_ggplot_options()
  if(genrefacets & genremean) error("genrefacets and genremean cannot both be TRUE")
  get_r2 <- function(.x){
    y <- lm(sc_endo_isei~sc_exo_pca, .x) %>% 
      summary() %>% 
      .$r.squared %>% 
      round(2)
    paste0("italic(R)^2 ==", y)
  }
  artists <- artists %>% 
    mutate(genre = recode_vars(genre, "genres")) 

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
      geom_text_repel(data = gm, mapping = aes(mean_exo_pca, mean_endo_isei, label = genre))
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
      summarize(x = max(sc_exo_pca)-.5, y = min(sc_endo_isei)+.5) %>% 
#      right_join(lab)
      bind_cols(lab)
  } else {
    lab <- tibble(r2 = get_r2(artists),
                  x = max(artists$sc_exo_pca)-.5, 
                  y = min(artists$sc_endo_isei)+.5)
  }
  g <- g + 
    geom_text(data = lab, mapping = aes(x=x, y=y, label = r2), parse = TRUE, hjust=.75)
  if(genrefacets) {
    g <- g + facet_wrap(~genre, ncol=ncol)
    }
  return(g)
}

plot_genre_overlap <- function(artists){
  require(overlapping)
  
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
    mutate(g1 = recode_vars(g1, "genres") %>% factor(levels = unique(.)),
           g2 = recode_vars(g2, "genres") %>% factor(levels = unique(.)),
           leg = recode_vars(leg, "legitimacy"))
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
  return(g)
}

table_leg_variance <- function(artists){
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
    mutate(Variable = recode_vars(Variable, "legitimacy") %>% 
             str_replace("\\n", " "))
  return(rs)
}
