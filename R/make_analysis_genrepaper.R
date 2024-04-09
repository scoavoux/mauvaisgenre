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
    mutate(total_n_pqnt_texte = log(total_n_pqnt_texte)) %>%
    mutate(genre = factor(genre) %>% fct_reorder(senscritique_meanscore, median)) %>% 
    select(genre, total_n_pqnt_texte, senscritique_meanscore) %>% 
    pivot_longer(-genre) %>% 
    ggplot(aes(genre, value)) +
    geom_boxplot() +
    facet_wrap(~name, scales = "free") +
    coord_flip() +
    lims(y = c(0, 10))
  return(g)
}

plot_endoexo_bygenre_scatterplot <- function(artists){
  set_ggplot_options()
  require(GGally)
  g <- artists %>% 
    select(starts_with("sc_")) %>% 
    ggpairs()
  return(g)
}

plot_endoexo_corrplot <- function(artists){
  set_ggplot_options()
  g <- artists %>% 
    ggplot(aes(x = sc_endo_isei, y = sc_exo_score)) +
      geom_point() +
      geom_smooth(method = "lm") +
     facet_wrap(~genre)
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
