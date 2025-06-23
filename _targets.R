# Preparation ------
library(targets)
library(tarchetypes)

tar_option_set(
  packages = c("paws", "tidyverse", "arrow"),
  repository = "aws", 
  repository_meta = "aws",
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
  tar_target(items,                              make_items_data()),
  tar_target(user_artist_peryear_onefile,        make_user_artist_peryear_table_onefile(streaming_data_files, items), pattern = streaming_data_files),
  tar_target(user_artist_peryear,                merge_user_artist_peryear_table(user_artist_peryear_onefile)),
  tar_target(to_remove_file_path,                "data/artists_to_remove.csv", format = "file"),
  tar_target(to_remove_file,                     read_csv(to_remove_file_path)),
  tar_target(user_artist_peryear_merged_artists, merge_duplicate_artists_in_streams(user_artist_peryear, senscritique_mb_deezer_id, to_remove_file)),
  tar_target(user_genre_summary_data_prop,       make_user_genre_summary_data(user_artist_peryear_merged_artists, genres, proportion=TRUE)),
  tar_target(user_genre_summary_data_raw ,       make_user_genre_summary_data(user_artist_peryear_merged_artists, genres, proportion=FALSE)),
  ## Prepare artists data ------
  
  ### Artists names, aliases, ids ------
  tar_target(manual_search_file_path,            "data/manual_search.csv", format = "file"),
  tar_target(manual_search_file,                 read_csv(manual_search_file_path)),
  tar_target(senscritique_mb_deezer_id,          make_senscritique_pairing_data(manual_search_file)),
  tar_target(artist_names_and_aliases,           make_aliases(senscritique_mb_deezer_id)),
  tar_target(artist_names,                       make_artists_names(artist_names_and_aliases)),
  
  ### Artists genres ------
  tar_target(genres_deezer_editorial_playlists,  make_genres_data(.source = "deezer_editorial_playlists", senscritique_mb_deezer_id)),
  tar_target(genres_deezer_maingenre,            make_genres_data(.source = "deezer_maingenre", senscritique_mb_deezer_id)),
  tar_target(genres_senscritique_tags,           make_genres_data(.source = "senscritique_tags", senscritique_mb_deezer_id)),
  tar_target(genres_deezer_albums,               make_genres_data(.source = "genres_from_deezer_albums", senscritique_mb_deezer_id)),
  tar_target(genres,                             merge_genres(genres_deezer_editorial_playlists, genres_deezer_maingenre, genres_senscritique_tags, genres_deezer_albums)),
  
  ### Artists popularity ------
  tar_target(artists_pop,                        make_artist_popularity_data(user_artist_peryear_merged_artists )),
  tar_target(artists_language,                   make_artist_language_data()),
  tar_target(area_country_file,                  "data/area_country.csv", format = "file"),
  tar_target(country_rank_file,                  "data/country_rank.csv", format = "file"),
  tar_target(artists_country,                    make_artist_area_data(area_country_file, country_rank_file)),
  
  ### Press data ------
  tar_target(corpus_raw,                         make_raw_corpus()),
  tar_target(BERT_corpus,                        make_corpus_for_BERT(corpus_raw), format = "file", repository = "local"),
  tar_target(corpus_filtered,                    filter_corpus_raw(corpus_raw, BERT_corpus)),
  tar_target(corpus_tokenized,                   make_corpus_tokenized_paragraphs(corpus_filtered)),

  ### Legitimacy data ------
  tar_target(regex_fixes_file_path,              "data/regex_fixes.csv", format = "file"),
  tar_target(regex_fixes_file,                   read_csv(regex_fixes_file_path, col_types = "nnccc")),
  tar_target(exo_press,                          make_press_data(corpus_tokenized, artist_names_and_aliases, regex_fixes_file)),
  tar_target(radio_leg,                          c("France Musique", "Radio Classique",
                                                   "TSF Jazz", "France Inter", 
                                                   "Fip", "Radio Nova")),
  tar_target(exo_radio,                          compute_exo_radio(senscritique_mb_deezer_id, radio_leg)),
  tar_target(exo_senscritique,                   make_senscritique_ratings_data(senscritique_mb_deezer_id)),
  tar_target(isei,                               make_isei_data(survey_raw)),
  tar_target(endo_legitimacy,                    make_endogenous_legitimacy_data(user_artist_peryear_merged_artists, isei, survey_raw)),

  ### Put all artists data together and filter ------
  tar_target(artists_raw,                        join_artist(artist_names,
                                                             genres, 
                                                             artists_pop, 
                                                             artists_language,
                                                             artists_country,
                                                             exo_radio, 
                                                             exo_senscritique, 
                                                             endo_legitimacy,
                                                             exo_press)),
  tar_target(artists,                            filter_artists(artists_raw)),
  ## Prepare user data ------
  tar_target(survey_raw,                         make_survey_data()),
  
  ### Omnivorousness ------
  tar_target(omni_from_survey,                   compute_omnivorourness_from_survey(survey_raw)),
  tar_target(omni_from_streams,                  compute_omnivorourness_from_streams(user_artist_peryear_merged_artists, artists, genres, rescale_by = "user")),
  
  ### Latent classes ------
  ## Make many models and then select one
  #### LCA from survey
  tar_target(latent_classes_from_surveys_multiple, 
             compute_latent_classes_from_survey(survey_raw, nclass = 1L:15L)),
  tar_target(latent_classes_from_surveys,        select_latent_class_model(latent_classes_from_surveys_multiple, 4)),
  
  #### LPA from streams
  # I don't get what's not working because the function runs outside of target. For now
  # we are not using this
  # when adding this again, edit recode_survey_data() to merge all user level variables.
  # tar_target(latent_classes_from_streams_multiple, 
  #            compute_latent_classes_from_streams(user_artist_peryear_merged_artists, genres, nclass = 1L:15L)),
  # tar_target(latent_classes_from_streams, 
  #            select_latent_class_model(latent_classes_from_streams_multiple, 5)),
  
  #### LPA from streams, with proportions
  # I don't get what's not working because the function runs outside of target. For now
  # we are not using this
  # tar_target(latent_classes_from_streams_multiple_proportion,
  #            compute_latent_classes_from_streams(user_artist_peryear_merged_artists, genres, nclass = 1L:15L, proportion = TRUE)),
  # tar_target(latent_classes_from_streams_proportion,
  #          select_latent_class_model(latent_classes_from_streams_multiple_proportion, 5)),
  
  ### Put all user data together ------
  tar_target(survey,                             recode_survey_data(survey_raw, 
                                                                    omni_from_survey, 
                                                                    omni_from_streams,
                                                                    latent_classes_from_surveys
                                                                    #latent_classes_from_streams,
                                                                    #latent_classes_from_streams_proportion
                                                                    )),
  
  ## Analysis Omni 1 ------
  tar_target(gg_pca_endoleg,                     plot_endoleg_pca(artists), format = "file", repository = "local"),
  tar_target(gg_endoleg_bygenre,                 plot_endoleg_bygenre(artists), format = "file", repository = "local"),
  tar_target(gg_exoleg_bygenre,                  plot_exoleg_bygenre(artists), format = "file", repository = "local"),
  tar_target(gg_endoexoleg_bygenre_density,      plot_endoexoleg_bygenre(artists, type = "density"), format = "file", repository = "local"),
  tar_target(gg_endoexoleg_bygenre_density_pres, plot_endoexoleg_bygenre(artists, type = "density", format = "presentation"), format = "file", repository = "local"),
  tar_target(gg_endoexoleg_bygenre_estimate,     plot_endoexoleg_bygenre(artists, type = "estimate"), format = "file", repository = "local"),
  tar_target(gg_endoexoleg_bygenre_estimate_pres,plot_endoexoleg_bygenre(artists, type = "estimate", format = "presentation"), format = "file", repository = "local"),
  tar_target(gg_endoexoleg_genrerank,            plot_endoexoleg_genrerank(artists), format = "file", repository = "local"),
  tar_target(gg_endoexoleg_correlation,          plot_endoexoleg_correlation(artists), format = "file", repository = "local"),
  tar_target(gg_leg_correlation_coefficient,     plot_leg_correlation_coefficient(artists), format = "file", repository = "local"),
  
  # tar_target(gg_endoexoleg_correlation_bygenre, plot_endoexoleg_correlation(artists, 
  #                                                                           genrefacets = TRUE, 
  #                                                                           output = "gg_endoexoleg_correlation_bygenre.pdf"),
  #            format = "file", repository = "local"),
  # tar_target(gg_endoexoleg_correlation_bygenre_pres, plot_endoexoleg_correlation(artists, 
  #                                                                           genrefacets = TRUE,
  #                                                                           ncol=6,
  #                                                                           .width=9,
  #                                                                           output = "gg_endoexoleg_correlation_bygenre_pres.pdf"),
  #            format = "file", repository = "local"),
  tar_target(gg_endoexoleg_correlation_genremean,    plot_endoexoleg_correlation(artists, genremean = TRUE), 
             format = "file", repository = "local"),
  tar_target(tb_endoexoleg_correlation_artist, tbl_endoexoleg_correlation(artists, genremean = FALSE), format = "file", repository = "local"),
  tar_target(tb_endoexoleg_correlation_genre,  tbl_endoexoleg_correlation(artists, genremean = TRUE), format = "file", repository = "local"),
  tar_target(tb_mean_sd_leg,                   table_mean_sd_leg(artists), format = "file", repository = "local"),
  tar_target(gg_genre_overlap,                 plot_genre_overlap(artists), format = "file", repository = "local"),
  tar_target(gg_example_genre_overlap,         plot_example_genre_overlap(artists), format = "file", repository = "local"),
  tar_target(tb_leg_variance,                  table_leg_variance(artists), format = "file", repository = "local"),
  tar_target(gg_leg_variance,                  plot_sd_leg(artists), format = "file", repository = "local"),
  
  
  # Omni 1 robustness check ------
  tar_target(gg_robustness_radio_genres,       plot_robustness_radio_genres(senscritique_mb_deezer_id, exo_senscritique, genres), format = "file", repository = "local"),
  tar_target(gg_robustness_radio_score ,       plot_robustness_radio_score(senscritique_mb_deezer_id, exo_senscritique, genres), format = "file", repository = "local"),
  
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
  tar_target(gg_lca_omni_presentation3, 
             plot_lca_omni(survey, lca_class_interpretation, format="presentation3"), 
             format = "file", repository = "local"),
  tar_target(gg_lca_diag, 			       plot_lca_diag(latent_classes_from_surveys_multiple), format = "file", repository = "local"),
  tar_target(gg_lca_profile, 			     plot_lca_profile(latent_classes_from_surveys, lca_class_interpretation), format = "file", repository = "local"),
  tar_target(gg_lca_profile_played,    plot_lca_profile_played(latent_classes_from_surveys, 
                                                               lca_class_interpretation,
                                                               user_genre_summary_data_prop,
                                                               user_genre_summary_data_raw), format = "file", repository = "local"),
  tar_target(gg_exoomni_by_otheromni_point, plot_exoomni_by_otheromni(survey, plot_type = "point_smooth"), format = "file", repository = "local"),
  tar_target(gg_exoomni_by_otheromni_dens,  plot_exoomni_by_otheromni(survey, plot_type = "density_2d"), format = "file", repository = "local"),
  tar_target(gg_omni_socdem_educ, 		      plot_omni_socdem_educ(survey), format = "file", repository = "local"),
  tar_target(gg_omni_socdem_isei, 		      plot_omni_socdem_isei(survey, isei), format = "file", repository = "local"),
  tar_target(gg_lca_socdem,	 		            plot_lca_socdem(survey, lca_class_interpretation), format = "file", repository = "local"),
  #tar_quarto(middlebrow_omnivore_report, "middlebrow_omnivore.qmd")  
  
  ## Supplementary analyses ------  
  tar_target(tbl_coverage, make_tbl_coverage(artists_pop, artists), format = "file", repository = "local"),
  
  ## Export for analyses in other languages (Philippe on STATA)
  tar_target(artists_csv,              export_to_csv(artists), format = "file"),
  tar_target(users_csv,                export_to_csv(survey), format = "file")
)
