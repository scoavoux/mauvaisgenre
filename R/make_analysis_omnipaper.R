plot_lca_diag <- function(latent_classes_from_surveys_multiple){
  require(tidyverse)
  set_ggplot_options()
  diag_lca <- tibble(nclass = names(latent_classes_from_surveys_multiple) %>% 
                       str_remove("k") %>% 
                       as.numeric(),
                     bic = map(latent_classes_from_surveys_multiple, ~.x$bic) %>%  unlist())
  g <- ggplot(diag_lca, aes(nclass, bic)) +
    geom_point() +
    geom_line() +
    labs(x = "Number of clusters", y = "BIC")
  ggsave("gg_lca_diag.pdf", g, path = "output/omni2", device = "pdf", height = 5)
  return("output/omni2/gg_lca_diag.pdf")
}

make_lca_class_interpretation <- function(latent_classes_from_surveys){
  require(tidyverse)
  require(janitor)
  cluster_survey <- latent_classes_from_surveys$predclass
  tb <- tabyl(cluster_survey) %>% 
    filter(!is.na(cluster_survey)) %>% 
    select(cluster_survey, n)
  tb$interp <- c("Omnivore",
                 "Highbrow omnivore", 
                 "Popular", 
                 "Rap")
  tb <- mutate(tb, 
               interp = paste0(interp, "\n(n=", n, ")"),
               cluster_survey = paste0("class", cluster_survey))
  res <- tb$cluster_survey
  names(res) <- tb$interp
  return(res)
}

plot_lca_profile <- function(latent_classes_from_surveys, 
                             lca_class_interpretation){
  require(tidyverse)
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
      rename(all_of(lca_class_interpretation))
  }
  g <- res %>% 
    pivot_longer(-genre) %>% 
    mutate(genre = factor(genre, levels = genre_order)) %>% 
    ggplot(aes(genre, value)) +
      geom_col() +
      coord_flip() +
      facet_wrap(~name) +
      labs(x = "Genres", y = "Prevalence") 
  ggsave("gg_lca_profile.pdf", g, path = "output/omni2", device = "pdf", height = 6)
  return("output/omni2/gg_lca_profile.pdf")
}

make_user_genre_summary_data <- function(user_artist_peryear_merged_artists,
                                         genres,
                                         proportion=TRUE){
  require(tidyverse)
  res <- user_artist_peryear_merged_artists %>% 
    left_join(genres) %>% 
    filter(!is.na(genre)) %>% 
    group_by(hashed_id, genre) %>% 
    summarize(l_play = sum(l_play, na.rm=TRUE))
  if(proportion){
    res <- res %>% 
      group_by(hashed_id) %>% 
      mutate(l_play = l_play / sum(l_play)) 
  }
  res <- res %>% 
    ungroup() %>% 
    pivot_wider(names_from = genre, values_from = l_play, values_fill = 0)
  return(res)
}

plot_lca_profile_played <- function(latent_classes_from_surveys, 
                                    lca_class_interpretation,
                                    user_genre_summary_data_prop,
                                    user_genre_summary_data_raw){

  require(tidyverse)
  set_ggplot_options()
  ci <- names(lca_class_interpretation)
  names(ci) <- lca_class_interpretation

  res <- tibble(hashed_id = latent_classes_from_surveys$id, 
         class = latent_classes_from_surveys$predclass) %>% 
    left_join(pivot_longer(user_genre_summary_data_prop, -hashed_id, names_to = "genre", values_to = "prop")) %>% 
    left_join(pivot_longer(user_genre_summary_data_raw, -hashed_id, names_to = "genre", values_to = "raw")) %>% 
    group_by(class, genre) %>% 
    summarise(prop = mean(prop),
              raw = mean(raw)) %>% 
    ungroup() %>% 
    mutate(genre = recode_vars(genre, "cleangenres"),
           class = ci[paste0("class", class)])

  g1 <- res %>% 
    ggplot(aes(genre, prop)) +
      geom_col() +
      coord_flip() +
      facet_wrap(~class) +
      labs(x = "Genres", y = "Proportion") 
  # so we compute the same plot with raw numbers rather than proportions
  # but it shows exactly the same 
  # g2 <- res %>% 
  #   ggplot(aes(genre, raw)) +
  #   geom_col() +
  #   coord_flip() +
  #   facet_wrap(~class) +
  #   labs(x = "Genres", y = "Prevalence") 
  # g2
  ggsave("gg_lca_profile_played_prop.pdf", g1, path = "output/omni2", device = "pdf", height = 6)
  return("output/omni2/gg_lca_profile_played_prop.pdf")
}

plot_lca_socdem <- function(survey, lca_class_interpretation){
  require(tidyverse)
  require(cowplot)
  set_ggplot_options()
  s <- survey %>%
    select(cluster_survey, gender, degree, age) %>% 
    filter(!is.na(cluster_survey)) %>% 
    mutate(cluster_survey = fct_recode(paste0("class", cluster_survey), !!!lca_class_interpretation))

  g <- s %>% 
    mutate(age = cut(age, breaks = c(0, 25, 35, 55, 100), labels = c("<25yo", "26-35yo", "36-55yo", ">55yo"))) %>% 
    filter(!is.na(age), !is.na(degree), !is.na(cluster_survey)) %>% 
    count(cluster_survey, age, degree) %>% 
    mutate(degree = factor(degree, 
                           levels = rev(c("high", "middle", "low")),
                           labels = rev(c("High", "Middle", "Low")))) %>% 
    group_by(cluster_survey, age) %>% 
    mutate(f = n/sum(n)) %>% 
    ggplot(aes(age, f, fill = degree)) +
      geom_col() +
      coord_flip() +
      scale_fill_brewer() + 
      labs(x = "Age", fill = "Education level", y ="") +
      #scale_fill_discrete(palette = "Set1") +
      facet_wrap(~cluster_survey)
  ggsave("gg_lca_socdem.pdf", g, path = "output/omni2", device = "pdf", height = 5)
  return("output/omni2/gg_lca_socdem.pdf")
}

plot_lca_omni <- function(survey, lca_class_interpretation, format="paper"){
  require(tidyverse)
  set_ggplot_options()
  s <- survey %>% 
    filter(!is.na(cluster_survey)) %>% 
    select(cluster_survey, 
           starts_with("mean_sc_exo"),
           starts_with("sd_sc_exo"),
           omni_survey_cultural_holes_played,
           omni_stream_genres_hhi) %>% 
    pivot_longer(-cluster_survey) %>% 
    mutate(name = recode_vars(name, "cleanlegitimacy"),
           name = str_replace(name, "\\\\n", "\n"),
           cluster_survey = paste0("class", cluster_survey),
           cluster_survey = fct_recode(cluster_survey, !!!lca_class_interpretation)) %>% 
    filter(!is.na(value)) %>% 
    group_by(cluster_survey, name) %>% 
    summarize(m = mean(value, na.rm=TRUE),
              sqrt = sqrt(n()),
              se = 1.96*sd(value, na.rm=TRUE)/sqrt(n())) %>% 
    ungroup() %>% 
    mutate(name = factor(name, levels = rev(unique(name))))
  
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
      filter(str_detect(name, "Mean")) %>% 
      ggplot(aes(x=cluster_survey, y=m, ymin=m-se, ymax=m+se)) +
        geom_point() +
        geom_pointrange() +
        coord_flip() +
        facet_wrap(~name, scales="free_x") +
        labs(x = "", y = "")
  } else if(format == "presentation3"){
    # Version for the presentation: separate
    gg <- s %>% 
      filter(str_detect(name, "SD")) %>% 
      ggplot(aes(x=cluster_survey, y=m, ymin=m-se, ymax=m+se)) +
      geom_point() +
      geom_pointrange() +
      coord_flip() +
      facet_wrap(~name, scales="free_x") +
      labs(x = "", y = "")
  }
  if(format == "paper"){
    height <- 6
  } else if(format != "paper"){
    height <- 4
  }
  ggsave(paste0("gg_lca_omni_", format, ".pdf"), gg, path = "output/omni2", device = "pdf", height = height)
  return(paste0("output/omni2/gg_lca_omni_", format, ".pdf"))
}

plot_lca_omni_bygenre <- function(survey, lca_class_interpretation){
  require(tidyverse)
  g <- survey %>% 
    select(cluster_survey, starts_with("mean_exo_pca_")) %>% 
    pivot_longer(-cluster_survey) %>% 
    filter(!is.na(cluster_survey), !is.na(value)) %>% 
    mutate(name = str_remove(name, "mean_exo_pca_"),
           name = recode_vars(name, "cleangenres"),
           cluster_survey = paste0("class", cluster_survey),
           cluster_survey = fct_recode(cluster_survey, !!!lca_class_interpretation)) %>% 
    # rescale by genre
    group_by(name) %>% 
    mutate(value = center_scale(value)) %>% 
    group_by(cluster_survey, name) %>% 
    summarize(m = mean(value), se = 1.96 * sd(value) / sqrt(n())) %>% 
    ggplot(aes(y=m, x=factor(cluster_survey), ymin = m-se, ymax=m+se)) +
      geom_point() +
      geom_linerange() +
      coord_flip() +
      facet_wrap(~name, scales = "free_x", ncol = 6) +
      labs(y = "Mean gatkp. leg.", x = "Cluster")
  ggsave("gg_lca_omni_bygenre.pdf", g, path = "output/omni2", device = "pdf", width = 9, height = 6)
  return("output/omni2/gg_lca_omni_bygenre.pdf")
}

plot_exoomni_by_otheromni <- function(survey){
  require(tidyverse, warn.conflicts = FALSE)
  require(cowplot)
  set_ggplot_options()
  s <- survey %>% 
      select(hashed_id, 
             omni_survey_cultural_holes_played,
             omni_stream_genres_hhi,
             starts_with("sd_sc")) %>% 
    pivot_longer(-hashed_id) %>% 
    mutate(name = recode_vars(name, "cleanlegitimacy"),
           name = str_replace(name, "\\\\n", "\n")) %>% 
    pivot_wider(names_from = name, values_from = value)
  l <- vector("list", 2L)
  xlim <- c(0, 7)
  ylim <- c(0, .3)
  l[[1]] <- s %>% 
    select(`Cultural holes played`, starts_with("SD gatkp")) %>% 
    pivot_longer(starts_with("SD gatkp")) %>% 
    ggplot(aes(`Cultural holes played`, value)) +
      #geom_point(shape = ".")
      geom_density_2d_filled() +
      guides(fill = "none") +
      facet_wrap(~name) +
      labs(y = "Gatkp. legitimacy") +
      scale_y_continuous(limits = ylim) +
      scale_x_continuous(limits = xlim) 
  xlim <- c(.4, 1)
  ylim <- c(0, .3)
  l[[2]] <- s %>% 
    select(`HHI genres streamed`, starts_with("SD gatkp")) %>% 
    pivot_longer(starts_with("SD gatkp")) %>% 
    ggplot(aes(`HHI genres streamed`, value)) +
      #geom_point(shape = ".")
      geom_density_2d_filled() +
      guides(fill = "none") +
      facet_wrap(~name) +
      labs(y = "Gatkp. legitimacy") +
      scale_x_continuous(limits = xlim) +
      scale_y_continuous(limits = ylim)
  g <- plot_grid(l[[1]], l[[2]])
  ggsave("gg_exoomni_by_otheromni.pdf", g, path = "output/omni2", device = "pdf", width = 11, height = 5)
  return("output/omni2/gg_exoomni_by_otheromni.pdf")
}

plot_omni_socdem_educ <- function(survey){
  require(tidyverse)
  set_ggplot_options()
  s <- survey %>% 
    select(omni_survey_cultural_holes_played,
           omni_stream_genres_hhi,
           starts_with("mean_sc_exo"),
           starts_with("sd_sc_exo"  ),
           degree) %>% 
    pivot_longer(-degree) %>% 
    mutate(name = recode_vars(name, "cleanlegitimacy"),
           name = str_replace(name, "\\\\n", "\n"),
           name = factor(name, levels = rev(unique(name))))
  g <- s %>% 
    filter(!is.na(degree), !is.na(value)) %>% 
    group_by(name) %>% 
    filter(value < quantile(value, .95),
           value > quantile(value, .01)) %>% 
    ggplot(aes(degree, value)) +
      geom_boxplot(notch = TRUE) +
      coord_flip() +
      labs(x = "Education", y="") +
      facet_wrap(~name, scales="free_x")
  ggsave("gg_omni_socdem_educ.pdf", g, path = "output/omni2", device = "pdf", height = 5)
  return("output/omni2/gg_omni_socdem_educ.pdf")
}

plot_omni_socdem_isei <- function(survey, isei){
  require(tidyverse)
  set_ggplot_options()
  s <- survey %>% 
    left_join(isei) %>% 
    select(omni_survey_cultural_holes_played,
           omni_stream_genres_hhi,
           #starts_with("mean_sc_exo"),
           starts_with("sd_sc_exo"  ),
           isei) %>% 
    pivot_longer(-isei) %>% 
    filter(!is.na(isei)) %>% 
    mutate(isei = cut(isei, breaks = quantile(isei, c(0, .25, .50, .75, 1)), labels = paste0("Q", 1:4)),
           name = recode_vars(name, "cleanlegitimacy"),
           name = str_replace(name, "\\\\n", "\n"),
           name = factor(name, levels = rev(unique(name))))
  g <- s %>% 
    filter(!is.na(isei), !is.na(value)) %>% 
    group_by(name) %>% 
    filter(value < quantile(value, .95),
           value > quantile(value, .01)) %>% 
    ggplot(aes(isei, value)) +
      geom_boxplot(notch = TRUE) +
      labs(x = "ISEI", y="Omnivorousness") +
      coord_flip() +
      facet_wrap(~name, scales="free_x")
  ggsave("gg_omni_socdem_isei.pdf", g, path = "output/omni2", device = "pdf", height = 5)
  return("output/omni2/gg_omni_socdem_isei.pdf")
}