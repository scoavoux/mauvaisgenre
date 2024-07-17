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

    ## Prepare data about artists pop ------
  tar_target(streaming_data_files, list_streaming_data_files()),
  tar_target(user_artist_peryear_onefile, make_user_artist_peryear_table_onefile(streaming_data_files), pattern = streaming_data_files),
  tar_target(user_artist_peryear, merge_user_artist_peryear_table(user_artist_peryear_onefile)),
  tar_target(user_artist_peryear_merged_artists, merge_duplicate_artists_in_streams(user_artist_peryear, senscritique_mb_deezer_id)),
  
  ## Prepare artists data ------
  tar_target(genres_deezer_editorial_playlists, make_genres_data(.source = "deezer_editorial_playlists", senscritique_mb_deezer_id)),
  tar_target(genres_deezer_maingenre, make_genres_data(.source = "deezer_maingenre", senscritique_mb_deezer_id)),
  tar_target(genres_senscritique_tags, make_genres_data(.source = "senscritique_tags", senscritique_mb_deezer_id)),
  tar_target(genres, merge_genres(genres_deezer_editorial_playlists, genres_deezer_maingenre, genres_senscritique_tags)),
  tar_target(artists_pop, make_artist_popularity_data(user_artist_peryear_merged_artists )),
  tar_target(senscritique_mb_deezer_id, make_senscritique_pairing_data()),
  
  tar_target(corpus_raw, make_raw_corpus()),
  tar_target(corpus_raw_tokenized, make_corpus_tokenized_sentences(corpus_raw)),
  tar_target(artist_names_and_aliases, make_aliases(senscritique_mb_deezer_id)),
  tar_target(artist_names, make_artists_names(artist_names_and_aliases)),
  
  tar_target(exo_press, make_press_data(corpus_raw_tokenized, artist_names_and_aliases, exo_senscritique)),
  tar_target(exo_radio, compute_exo_radio()),
  tar_target(exo_senscritique, make_senscritique_ratings_data(senscritique_mb_deezer_id)),
  tar_target(isei, make_isei_data(survey_raw)),
  tar_target(endo_legitimacy, make_endogenous_legitimacy_data(user_artist_peryear_merged_artists, isei, survey_raw)),

  # tar_target(exo_pca, compute_exo_pca()),
  tar_target(artists_raw, join_artist(artist_names,
                                      genres, 
                                      artists_pop, 
                                      exo_radio, 
                                      exo_senscritique, 
                                      endo_legitimacy,
                                      exo_press)), #artists_name
  tar_target(artists, filter_artists(artists_raw)),

  ## Analysis Omni 1 ------
  tar_target(gg_endoleg_bygenre, plot_endoleg_bygenre(artists)),
  tar_target(gg_exoleg_bygenre, plot_exoleg_bygenre(artists)),
  tar_target(gg_endoexoleg_bygenre, plot_endoexoleg_bygenre(artists)),
  tar_target(gg_endoexoleg_bygenre_mean_errorbar, plot_endoexoleg_bygenre(artists, type = "estimate")),
  tar_target(gg_endoexoleg_genrerank, plot_endoexoleg_genrerank(artists)),
  tar_target(gg_endoexoleg_correlation, plot_endoexoleg_correlation(artists)),
  tar_target(gg_endoexoleg_correlation_bygenre, plot_endoexoleg_correlation(artists, genrefacets = TRUE)),
  tar_target(gg_endoexoleg_correlation_bygenre_pres, plot_endoexoleg_correlation(artists, 
                                                                            genrefacets = TRUE,
                                                                            ncol=6)),
  tar_target(gg_endoexoleg_correlation_genremean, plot_endoexoleg_correlation(artists, genremean = TRUE)),
  tar_target(gg_genre_overlap, plot_genre_overlap(artists)),
  tar_target(tb_leg_variance, table_leg_variance(artists)),
  
  # Report Omni1
  # we remove it for now because it's the longest target to compile
  # (about 30 seconds) and it basically removes the incentive to do cache/targets
  # altogether
  #tar_quarto(mauvais_genre_report, "mauvais_genre.qmd")
  
  ## Prepare user data ------
  tar_target(survey_raw, make_survey_data()),
  
  # Omnivorousness
  tar_target(omni_from_survey, compute_omnivorourness_from_survey(survey_raw)),
  tar_target(omni_from_streams, compute_omnivorourness_from_streams(user_artist_peryear_merged_artists, artists, genres, rescale_by = "user")),
  
  # Latent classes
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
  
  # Put everything together
  tar_target(survey, recode_survey_data(survey_raw, 
                                        omni_from_survey, 
                                        omni_from_streams,
                                        latent_classes_from_surveys,
                                        latent_classes_from_streams,
                                        latent_classes_from_streams_proportion)),
  
  ## Analysis Omni 2 ------
  tar_target(lca_class_interpretation, make_lca_class_interpretation()),
  tar_target(gg_lca_omni_paper, 
             plot_lca_omni(survey, lca_class_interpretation, format="paper")),
  tar_target(gg_lca_omni_presentation1, 
             plot_lca_omni(survey, lca_class_interpretation, format="presentation1")),
  tar_target(gg_lca_omni_presentation2, 
             plot_lca_omni(survey, lca_class_interpretation, format="presentation2")),
  #tar_quarto(middlebrow_omnivore_report, "middlebrow_omnivore.qmd")  
  
  ## Supplementary analyses ------  
  tar_target(tbl_coverage, make_tbl_coverage(artists_pop, artists))
)
