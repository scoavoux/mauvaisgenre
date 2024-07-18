plot_lca_diag <- function(latent_classes_from_surveys_multiple){
  require(tidyverse)
  set_ggplot_options()
  diag_lca <- tibble(nclass = names(latent_classes_from_surveys_multiple) %>% 
                       str_remove("k") %>% 
                       as.numeric(),
                     bic = map(latent_classes_from_surveys_multiple, ~.x$bic) %>%  unlist())
  ggplot(diag_lca, aes(nclass, bic)) +
    geom_point() +
    geom_line() +
    labs(x = "Number of clusters", y = "BIC")
}

make_lca_class_interpretation <- function(){
  c("Omnivore" = "class1", 
    "Highbrow omnivore" = "class2", 
    "Popular" = "class3", 
    "Rap" = "class4")
}

plot_lca_profile <- function(latent_classes_from_surveys, 
                             lca_class_interpretation){
  set_ggplot_options()
  res <- vector("list", length(latent_classes_from_surveys$probs))
  for(i in 1:length(latent_classes_from_surveys$probs)){
    res[[i]] <- latent_classes_from_surveys$probs[[i]] %>% 
      as.data.frame() %>% 
      rownames_to_column("class") 
    res[[i]]$genre <- names(latent_classes_from_surveys$probs)[i]
  }
  res <- bind_rows(res) %>% 
    select(-`Pr(1)`) %>% 
    mutate(class = str_remove_all(class, "[ :]")) %>% 
    pivot_wider(names_from = class, values_from = `Pr(2)`) %>% 
    mutate(genre = recode_vars(genre, "cleangenres"))
  genre_prevalence <- res %>% 
    pivot_longer(-genre) %>% 
    summarise(value = mean(value), .by = genre)
  genre_order <- genre_prevalence %>% 
    arrange(value) %>% 
    pull(genre)
  if(!is.null(lca_class_interpretation)){
    res <- res %>% 
      rename(lca_class_interpretation)
  }
  res %>% 
    pivot_longer(-genre) %>% 
    mutate(genre = factor(genre, levels = genre_order)) %>% 
    ggplot(aes(genre, value)) +
      geom_col() +
      coord_flip() +
      facet_wrap(~name) +
      labs(x = "Genres", y = "Prevalence") 
}

table_lca_socdem <- function(survey, lca_class_interpretation){
  require(tidyverse)
  require(cowplot)
  set_ggplot_options()
  s <- survey %>%
    select(cluster_survey, gender, degree, age) %>% 
    filter(!is.na(cluster_survey)) %>% 
    mutate(cluster_survey = fct_recode(paste0("class", cluster_survey), !!!lca_class_interpretation))
  gg_gender <- s %>% 
    filter(!is.na(gender)) %>% 
    ggplot(aes(cluster_survey, fill=gender)) +
      geom_bar(position="fill") +
      coord_flip() +
      labs(x="LCA cluster", y="", fill="") +
      theme(legend.position = "bottom")
  gg_gender <- s %>% 
    filter(!is.na(gender)) %>% 
    ggplot(aes(cluster_survey, fill=gender)) +
      geom_bar(position="fill") +
      coord_flip() +
      labs(x="", y="", fill="Gender") +
      theme(legend.position = "bottom")
  gg_degree <- s %>% 
    filter(!is.na(degree)) %>% 
    ggplot(aes(cluster_survey, fill=degree)) +
      geom_bar(position="fill") +
      scale_fill_brewer() +
      coord_flip() +
      labs(x="", y="", fill="Education") +
      theme(legend.position = "bottom")
  gg_age <- s %>% 
    filter(!is.na(age)) %>% 
    ggplot(aes(age, group = cluster_survey, color=cluster_survey)) +
      geom_density() +
      scale_fill_brewer() +
  #    coord_flip() +
      labs(x="Age", y="", color = "LCA cluster") +
      scale_x_continuous(limits=c(10, 90), breaks = seq(10, 90, 10)) +
      theme(legend.position = "bottom", base.size=12)
  
  ggdraw() +
    draw_plot(gg_gender, x = 0 , y = .5, width = .5, height = .5) +
    draw_plot(gg_degree, x = .5, y = .5, width = .5, height = .5) +
    draw_plot(gg_age,    x = 0 , y = 0 , width =  1, height = .5)
  
  s %>% 
    mutate(age = cut(age, breaks = c(0, 25, 35, 55, 100), labels = c("<25yo", "26-35yo", "36-55yo", ">55yo"))) %>% 
    filter(!is.na(age), !is.na(degree), !is.na(cluster_survey)) %>% 
    count(cluster_survey, age, degree) %>% 
    mutate(degree = factor(degree, levels = rev(c("high", "middle", "low")))) %>% 
    group_by(cluster_survey, age) %>% 
    mutate(f = n/sum(n)) %>% 
    ggplot(aes(age, f, fill = degree)) +
      geom_col() +
      coord_flip() +
      labs(x = "Age") +
      #scale_fill_discrete(palette = "Set1") +
      facet_wrap(~cluster_survey)
}

plot_lca_omni <- function(survey, lca_class_interpretation, format="paper"){
  require(tidyverse)
  set_ggplot_options()
  s <- survey %>% 
    filter(!is.na(cluster_survey)) %>% 
    select(cluster_survey, 
           mean_exo_pca,
           omni_survey_cultural_holes_played,
           omni_stream_genres_hhi, 
           sd_exo_pca) %>% 
    pivot_longer(-cluster_survey) %>% 
    mutate(name = recode_vars(name, "cleanlegitimacy"),
           cluster_survey = paste0("class", cluster_survey),
           cluster_survey = fct_recode(cluster_survey, !!!lca_class_interpretation)) %>% 
    filter(!is.na(value)) %>% 
    group_by(cluster_survey, name) %>% 
    summarize(m = mean(value, na.rm=TRUE),
              sqrt = sqrt(n()),
              se = 1.96*sd(value, na.rm=TRUE)/sqrt(n())) %>% 
    ungroup()
  
  if(format == "paper"){
    gg <- s %>% 
      # Version for the paper: all together
      ggplot(aes(x=cluster_survey, y=m, ymin=m-se, ymax=m+se)) +
        geom_point() +
        geom_pointrange() +
        #geom_col() +
        coord_flip() +
        facet_wrap(~name, scales="free_x") +
        labs(x = "", y = "")
  } else if(format == "presentation1"){
    gg <- s %>% 
      filter(!str_detect(name, "leg.")) %>% 
      ggplot(aes(x=cluster_survey, y=m, ymin=m-se, ymax=m+se)) +
        geom_point() +
        geom_pointrange() +
        coord_flip() +
        facet_wrap(~name, scales="free_x") +
        labs(x = "", y = "")
  } else if(format == "presentation2"){
    # Version for the presentation: separate
    gg <- s %>% 
      filter(str_detect(name, "leg.")) %>% 
      ggplot(aes(x=cluster_survey, y=m, ymin=m-se, ymax=m+se)) +
        geom_point() +
        geom_pointrange() +
        coord_flip() +
        facet_wrap(~name, scales="free_x") +
        labs(x = "", y = "")
  }
  return(gg)
}

plot_exoomni_by_otheromni <- function(survey){
  require(tidyverse)
  require(cowplot)
  set_ggplot_options()
  s <- survey %>% 
      select(hashed_id, 
             omni_survey_cultural_holes_played,
             omni_stream_genres_hhi,
             sd_exo_pca) %>% 
    pivot_longer(-hashed_id) %>% 
    mutate(name = recode_vars(name, "cleanlegitimacy")) %>% 
    pivot_wider(names_from = name, values_from = value)
  l <- vector("list", 2L)
  l[[1]] <- ggplot(s, aes(`Cultural holes played`, `SD exo. leg.`)) +
    #geom_point(shape = ".")
    geom_density_2d_filled() +
    scale_y_continuous(limits = c(0, .3)) +
    scale_x_continuous(limits = c(0, 7))
  l[[2]] <- ggplot(s, aes(`HHI genres streamed`, `SD exo. leg.`)) +
    #geom_point(shape = ".")
    geom_density_2d_filled() +
    scale_y_continuous(limits = c(0, .3)) +
    scale_x_continuous(limits = c(.4, 1))
  plot_grid(l[[1]], l[[2]])
}

plot_omni_socdem <- function(survey){
  require(tidyverse)
  require(cowplot)
  set_ggplot_options()
  s <- survey %>% 
    select(omni_survey_cultural_holes_played,
           omni_stream_genres_hhi,
           mean_exo_pca, 
           sd_exo_pca,
           gender,
           degree,
           age) %>% 
    pivot_longer(omni_survey_cultural_holes_played:sd_exo_pca) %>% 
    mutate(name = recode_vars(name, "cleanlegitimacy"))
  s %>% 
    filter(!is.na(degree), !is.na(value)) %>% 
    group_by(name) %>% 
    filter(value < quantile(value, .95),
           value > quantile(value, .01)) %>% 
    ggplot(aes(degree, value)) +
      geom_boxplot(notch = TRUE) +
      coord_flip() +
      labs(x = "Education", y="") +
      facet_wrap(~name, scales="free_x")
}