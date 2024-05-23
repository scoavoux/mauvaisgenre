# Example _targets.R file:
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

source("R/common_functions.R")
source("R/make_intermediary_data.R")
source("R/make_artists_data.R")
source("R/make_analysis_genrepaper.R")
source("R/make_user_data.R")


list(
  # Prepare artists data ------
  # tar_target(artists_list),
  # 
  # tar_target(endo_leg, compute_endo_leg()),
  # tar_target(exo_press, compute_exo_press()),
  tar_target(exo_radio, compute_exo_radio()),
  # tar_target(exo_sc, read_csv("")),
  # 
  # tar_target(exo_pca, compute_exo_pca()),
  
  tar_target(artists, join_artist(exo_radio)), #endo_leg, exo_press, exo_sc
  tar_target(artists_filtered, filter_artists(artists)),
  tar_target(genres, make_genres_data()),
  tar_target(genres_aliases, make_genre_aliases()),

  # Analysis Omni 1 ------
  tar_target(gg_endoleg_bygenre, plot_endoleg_bygenre(artists_filtered)),
  tar_target(gg_exoleg_bygenre, plot_exoleg_bygenre(artists_filtered)),
  tar_target(gg_endoexoleg_bygenre, plot_endoexoleg_bygenre(artists_filtered)),
  tar_target(gg_endoexoleg_bygenre_mean_errorbar, plot_endoexoleg_bygenre(artists_filtered, type = "estimate")),
  tar_target(gg_endoexoleg_genrerank, plot_endoexoleg_genrerank(artists_filtered)),
  tar_target(gg_endoexoleg_correlation, plot_endoexoleg_correlation(artists_filtered)),
  tar_target(gg_endoexoleg_correlation_bygenre, plot_endoexoleg_correlation(artists_filtered, genrefacets = TRUE)),
  tar_target(gg_endoexoleg_correlation_bygenre_pres, plot_endoexoleg_correlation(artists_filtered, 
                                                                            genrefacets = TRUE,
                                                                            ncol=6)),
  tar_target(gg_endoexoleg_correlation_genremean, plot_endoexoleg_correlation(artists_filtered, genremean = TRUE)),
  tar_target(gg_genre_overlap, plot_genre_overlap(artists_filtered)),
  tar_target(tb_leg_variance, table_leg_variance(artists_filtered)),
  
  # Report Omni1
  # we remove it for now because it's the longest target to compile
  # (about 30 seconds) and it basically removes the incentive to do cache/targets
  # altogether
  #tar_quarto(mauvais_genre_report, "mauvais_genre.qmd")
  
  # Prepare user data ------
  tar_target(survey, make_survey_data()),
  tar_target(streaming_data_files, list_streaming_data_files()),
  tar_target(user_artist_peryear_onefile, make_user_artist_peryear_table_onefile(streaming_data_files), pattern = streaming_data_files),
  tar_target(user_artist_peryear, merge_user_artist_peryear_table(user_artist_peryear_onefile)),
  
  tar_target(omni_from_survey, compute_omnivorourness_from_survey(survey, genre_aliases)),
  tar_target(omni_from_streams, compute_omnivorourness_from_streams(user_artist_peryear, artists_filtered, genres)),
  tar_target(latent_classes_from_surveys_multiple, compute_latent_classes_from_survey(survey, genre_aliases, nclass = 3L:10L)),
  tar_target(latent_classes_from_streams_multiple, compute_latent_classes_from_streams(user_artist_peryear, genres, nclass = 3L:10L)),
  tar_target(latent_classes_from_streams_multiple_proportion, compute_latent_classes_from_streams(user_artist_peryear, genres, nclass = 3L:8L, proportion = TRUE))
  
  # Analysis Omni 2 ------
  #tar_quarto(middlebrow_omnivore_report, "middlebrow_omnivore.qmd")  
)
