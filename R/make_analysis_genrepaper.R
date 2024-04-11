plot_endoleg_bygenre <- function(artists){
  set_ggplot_options()
  g <- artists %>% 
    mutate(genre = fct_reorder(genre, endo_isei_mean_pond, mean)) %>% 
    select(genre, endo_isei_mean_pond, endo_share_high_education_pond) %>% 
    pivot_longer(-genre) %>% 
    ggplot(aes(x= genre, y = value)) +
      geom_boxplot() +
      facet_wrap(~name, scales = "free") +
      coord_flip()
  return(g)
}

plot_exoleg_bygenre <- function(artists){
  set_ggplot_options()
  g <- artists %>% 
    mutate(total_n_pqnt_texte = log(total_n_pqnt_texte+1),
           radio_leg = log(radio_leg+1)) %>%
    mutate(genre = factor(genre) %>% fct_reorder(senscritique_meanscore, median)) %>% 
    select(genre, total_n_pqnt_texte, senscritique_meanscore, radio_leg, sc_exo_pca) %>% 
    pivot_longer(-genre) %>% 
    ggplot(aes(genre, value)) +
    geom_boxplot() +
    facet_wrap(~name, scales = "free") +
    coord_flip() +
    lims(y = c(0, 10))
  return(g)
}

plot_endoexoleg_bygenre <- function(artists){
  set_ggplot_options()
  g <- artists %>% 
    select(genre, sc_endo_isei, sc_exo_pca) %>% 
    mutate(genre = factor(genre) %>% fct_reorder(sc_endo_isei, median)) %>% 
    pivot_longer(-genre) %>% 
    ggplot(aes(x= genre, y = value)) +
    geom_boxplot() +
    facet_wrap(~name, scales = "free") +
    coord_flip()
  return(g)
}

plot_endoexoleg_correlation <- function(artists, genrefacets=FALSE, genremean=FALSE){
  set_ggplot_options()
  if(genrefacets & genremean) error("genrefacets and genremean cannot both be TRUE")
  get_r2 <- function(.x){
    y <- lm(sc_endo_isei~sc_exo_pca, .x) %>% 
      summary() %>% 
      .$r.squared %>% 
      round(2)
    paste0("italic(R)^2 ==", y)
  }
  g <- artists %>% 
    ggplot(aes(sc_exo_pca, sc_endo_isei)) +
     geom_smooth(method="lm") +
     labs(x="Exogenous legitimacy (pca)", y = "Endogenous legitimacy (ISEI)")
  if(genremean){
    gm <- artists %>% 
      group_by(genre) %>% 
      summarize(mean_exo_pca = mean(sc_exo_pca), 
                mean_endo_isei= mean(sc_endo_isei))
    g <- g + 
      geom_point(alpha=.2, color="grey") +
      geom_text(data= gm, mapping = aes(mean_exo_pca, mean_endo_isei, label = genre))
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
      group_by(genre) %>% 
      summarize(x = max(sc_exo_pca)-.5, y = min(sc_endo_isei)+.5) %>% 
      right_join(lab)
  } else {
    lab <- tibble(r2 = get_r2(artists),
                  x = max(artists$sc_exo_pca)-.5, 
                  y = min(artists$sc_endo_isei)+.5)
  }
  g <- g + 
    geom_text(data = lab, mapping = aes(x=x, y=y, label = r2), parse = TRUE, hjust=.75)
  if(genrefacets) {
    g <- g + facet_wrap(~genre, scales="free")
    }
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
                 values_to = "R squared")
  return(rs)
}
