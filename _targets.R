# Example _targets.R file:
library(targets)

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
  # tar_target(exo_radio, compute_exo_radio()),
  # tar_target(exo_sc, read_csv("")),
  # 
  # tar_target(exo_pca, compute_exo_pca()),
  
  tar_target(artists, join_artist(artists_list, endo_leg, exo_press, exo_radio, exo_sc)),
  tar_target(artists_filtered, filter_artists(artists)),
  
  # Prepare user data ------
  
  # Analysis Omni 1 ------
  tar_target(gg_endoleg_bygenre, plot_endoleg_bygenre(artists_filtered)),
  tar_target(gg_exoleg_bygenre, plot_exoleg_bygenre(artists_filtered)),
  tar_target(gg_endoexo_bygenre_scatterplot, plot_endoexo_bygenre_scatterplot(artists_filtered)),
  tar_target(gg_endoexo_corrplot, plot_endoexo_corrplot(artists_filtered)),
  tar_target(tb_leg_variance, table_leg_variance(artists_filtered))
  
  # Analysis Omni 2 ------
)
