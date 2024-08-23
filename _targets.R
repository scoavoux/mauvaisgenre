# Preparation ------
library(targets)
library(tarchetypes)

tar_option_set(
  packages = c("paws", "tidyverse", "arrow"),
  repository = "aws",
  resources = tar_resources(
    aws = tar_resources_aws(
      endpoint = Sys.getenv("S3_ENDPOINT"),
      bucket = "scoavoux",
      prefix = "omnivorism"
    )
  )
)

tar_source("R")

# List of targets ------
list(

  ## Prepare streaming data ------
  tar_target(streaming_data_files,               list_streaming_data_files()),
  tar_target(user_artist_peryear_onefile,        make_user_artist_peryear_table_onefile(streaming_data_files), pattern = streaming_data_files),
  tar_target(user_artist_peryear,                merge_user_artist_peryear_table(user_artist_peryear_onefile)),
  tar_target(to_remove_file,                     read_csv("data/artists_to_remove.csv")),
  tar_target(user_artist_peryear_merged_artists, merge_duplicate_artists_in_streams(user_artist_peryear, senscritique_mb_deezer_id, to_remove_file)),
  tar_target(user_genre_summary_data_prop,       make_user_genre_summary_data(user_artist_peryear_merged_artists, genres, proportion=TRUE)),
  tar_target(user_genre_summary_data_raw ,       make_user_genre_summary_data(user_artist_peryear_merged_artists, genres, proportion=FALSE)),
  ## Prepare artists data ------
  
  ### Artists names, aliases, ids ------
  tar_target(manual_search_file,           read_csv("data/manual_search.csv")),
  tar_target(senscritique_mb_deezer_id,    make_senscritique_pairing_data(manual_search_file)),
  tar_target(artist_names_and_aliases,     make_aliases(senscritique_mb_deezer_id)),
  tar_target(artist_names,                 make_artists_names(artist_names_and_aliases)),
  
  ### Artists genres ------
  tar_target(genres_deezer_editorial_playlists, make_genres_data(.source = "deezer_editorial_playlists", senscritique_mb_deezer_id)),
  tar_target(genres_deezer_maingenre,           make_genres_data(.source = "deezer_maingenre", senscritique_mb_deezer_id)),
  tar_target(genres_senscritique_tags,          make_genres_data(.source = "senscritique_tags", senscritique_mb_deezer_id)),
  tar_target(genres,                            merge_genres(genres_deezer_editorial_playlists, genres_deezer_maingenre, genres_senscritique_tags)),
  
  ### Artists popularity ------
  tar_target(artists_pop, make_artist_popularity_data(user_artist_peryear_merged_artists )),
  
  ### Press data ------
  tar_target(corpus_raw,              make_raw_corpus()),
  tar_target(corpus_raw_tokenized,    make_corpus_tokenized_sentences(corpus_raw)),

  ### Legitimacy data ------
  tar_target(exo_press,        make_press_data(corpus_raw_tokenized, artist_names_and_aliases)),
  tar_target(radio_leg,        c("France Inter", "France Musique", "Fip", "Radio Nova")),
  tar_target(exo_radio,        compute_exo_radio(senscritique_mb_deezer_id, radio_leg)),
  tar_target(exo_senscritique, make_senscritique_ratings_data(senscritique_mb_deezer_id)),
  tar_target(isei,             make_isei_data(survey_raw)),
  tar_target(endo_legitimacy,  make_endogenous_legitimacy_data(user_artist_peryear_merged_artists, isei, survey_raw)),

  ### Put all artists data together and filter ------
  tar_target(artists_raw, join_artist(artist_names,
                                      genres, 
                                      artists_pop, 
                                      exo_radio, 
                                      exo_senscritique, 
                                      endo_legitimacy,
                                      exo_press)), #artists_name
  tar_target(artists,     filter_artists(artists_raw)),
  ## Prepare user data ------
  tar_target(survey_raw,  make_survey_data()),
  
  ### Omnivorousness ------
  tar_target(omni_from_survey,    compute_omnivorourness_from_survey(survey_raw)),
  tar_target(omni_from_streams,   compute_omnivorourness_from_streams(user_artist_peryear_merged_artists, artists, genres, rescale_by = "user")),
  
  ### Latent classes ------
  ## Make many models
  tar_target(latent_classes_from_surveys_multiple, 
             compute_latent_classes_from_survey(survey_raw, nclass = 1L:15L)),
  tar_target(latent_classes_from_streams_multiple, 
             compute_latent_classes_from_streams(user_artist_peryear_merged_artists , genres, nclass = 1L:15L)),
  tar_target(latent_classes_from_streams_multiple_proportion, 
             compute_latent_classes_from_streams(user_artist_peryear_merged_artists , genres, nclass = 1L:15L, proportion = TRUE)),
  
  ## Extract one
  tar_target(latent_classes_from_surveys, 
             select_latent_class_model(latent_classes_from_surveys_multiple, 4)),
  tar_target(latent_classes_from_streams, 
             select_latent_class_model(latent_classes_from_streams_multiple, 5)),
  tar_target(latent_classes_from_streams_proportion, 
             select_latent_class_model(latent_classes_from_streams_multiple_proportion, 5)),
  
  ### Put all user data together ------
  tar_target(survey, recode_survey_data(survey_raw, 
                                        omni_from_survey, 
                                        omni_from_streams,
                                        latent_classes_from_surveys,
                                        latent_classes_from_streams,
                                        latent_classes_from_streams_proportion)),
  
  ## Analysis Omni 1 ------
  tar_target(gg_pca_endoleg,                    plot_endoleg_pca(artists), format = "file", repository = "local"),
  tar_target(gg_endoleg_bygenre,                plot_endoleg_bygenre(artists), format = "file", repository = "local"),
  tar_target(gg_exoleg_bygenre,                 plot_exoleg_bygenre(artists), format = "file", repository = "local"),
  tar_target(gg_endoexoleg_bygenre_density,     plot_endoexoleg_bygenre(artists, type = "density"), format = "file", repository = "local"),
  tar_target(gg_endoexoleg_bygenre_estimate,    plot_endoexoleg_bygenre(artists, type = "estimate"), format = "file", repository = "local"),
  tar_target(gg_endoexoleg_genrerank,           plot_endoexoleg_genrerank(artists), format = "file", repository = "local"),
  tar_target(gg_endoexoleg_correlation,         plot_endoexoleg_correlation(artists)),
  tar_target(gg_endoexoleg_correlation_bygenre, plot_endoexoleg_correlation(artists, 
                                                                            genrefacets = TRUE, 
                                                                            output = "gg_endoexoleg_correlation_bygenre.pdf"),
             format = "file", repository = "local"),
  tar_target(gg_endoexoleg_correlation_bygenre_pres, plot_endoexoleg_correlation(artists, 
                                                                            genrefacets = TRUE,
                                                                            ncol=6,
                                                                            .width=9,
                                                                            output = "gg_endoexoleg_correlation_bygenre_pres.pdf"),
             format = "file", repository = "local"),
  tar_target(gg_endoexoleg_correlation_genremean,    plot_endoexoleg_correlation(artists, 
                                                                              genremean = TRUE,
                                                                              output = "gg_endoexoleg_correlation_genremean.pdf"), 
             format = "file", repository = "local"),
  tar_target(tb_mean_sd_leg,               table_mean_sd_leg(artists), format = "file", repository = "local"),
  tar_target(gg_genre_overlap,             plot_genre_overlap(artists), format = "file", repository = "local"),
  tar_target(gg_example_genre_overlap,     plot_example_genre_overlap(artists), format = "file", repository = "local"),
  tar_target(tb_leg_variance,              table_leg_variance(artists), format = "file", repository = "local"),
  
  
  # Report Omni1
  # we remove it for now because it's the longest target to compile
  # (about 30 seconds) and it basically removes the incentive to do cache/targets
  # altogether
  #tar_quarto(mauvais_genre_report, "mauvais_genre.qmd")
  
  
  ## Analysis Omni 2 ------
  tar_target(lca_class_interpretation, make_lca_class_interpretation(latent_classes_from_surveys)),
  tar_target(gg_lca_omni_paper, 
             plot_lca_omni(survey, lca_class_interpretation, format="paper"), 
             format = "file", repository = "local"),
  tar_target(gg_lca_omni_presentation1, 
             plot_lca_omni(survey, lca_class_interpretation, format="presentation1"), 
             format = "file", repository = "local"),
  tar_target(gg_lca_omni_presentation2, 
             plot_lca_omni(survey, lca_class_interpretation, format="presentation2"), 
             format = "file", repository = "local"),
  tar_target(gg_lca_diag, 			       plot_lca_diag(latent_classes_from_surveys_multiple), format = "file", repository = "local"),
  tar_target(gg_lca_profile, 			     plot_lca_profile(latent_classes_from_surveys, lca_class_interpretation), format = "file", repository = "local"),
  tar_target(gg_lca_profile_played,    plot_lca_profile_played(latent_classes_from_surveys, 
                                                               lca_class_interpretation,
                                                               user_genre_summary_data_prop,
                                                               user_genre_summary_data_raw), format = "file", repository = "local"),
  tar_target(gg_lca_omni_bygenre, plot_lca_omni_bygenre(survey, lca_class_interpretation), format = "file", repository = "local"),
  tar_target(gg_exoomni_by_otheromni,	 plot_exoomni_by_otheromni(survey), format = "file", repository = "local"),
  tar_target(gg_omni_socdem, 			     plot_omni_socdem(survey), format = "file", repository = "local"),
  tar_target(gg_lca_socdem,	 		       plot_lca_socdem(survey, lca_class_interpretation), format = "file", repository = "local"),
  #tar_quarto(middlebrow_omnivore_report, "middlebrow_omnivore.qmd")  
  
  ## Supplementary analyses ------  
  tar_target(tbl_coverage, make_tbl_coverage(artists_pop, artists), format = "file", repository = "local")
)
