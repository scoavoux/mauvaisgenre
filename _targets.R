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
source("R/make_artists_data.R")
source("R/make_analysis_genrepaper.R")


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

  # Analysis Omni 1 ------
  tar_target(gg_endoleg_bygenre, plot_endoleg_bygenre(artists_filtered)),
  tar_target(gg_exoleg_bygenre, plot_exoleg_bygenre(artists_filtered)),
  tar_target(gg_endoexoleg_bygenre, plot_endoexoleg_bygenre(artists_filtered)),
  tar_target(gg_endoexoleg_bygenre_mean_errorbar, plot_endoexoleg_bygenre(artists_filtered, type = "estimate")),
  tar_target(gg_endoexoleg_genrerank, plot_endoexoleg_genrerank(artists_filtered)),
  tar_target(gg_endoexoleg_correlation, plot_endoexoleg_correlation(artists_filtered)),
  tar_target(gg_endoexoleg_correlation_bygenre, plot_endoexoleg_correlation(artists_filtered, genrefacets = TRUE)),
  tar_target(gg_endoexoleg_correlation_genremean, plot_endoexoleg_correlation(artists_filtered, genremean = TRUE)),
  tar_target(gg_genre_overlap, plot_genre_overlap(artists_filtered)),
  tar_target(tb_leg_variance, table_leg_variance(artists_filtered)),
  
  # Report Omni1
  tar_quarto(mauvais_genre_report, "mauvais_genre.qmd"),
  
  # Prepare user data ------  
  
  # Analysis Omni 2 ------
  tar_quarto(middlebrow_omnivore_report, "middlebrow_omnivore.qmd")  
)
