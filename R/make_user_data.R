# Compute omnivorism by user ------
compute_omnivorourness_from_survey <- function(){
  # Compute from genres liked and consumed.
  # TODO: cultural holes
  require(tidyverse)
  s3 <- initialize_s3()
  f <- s3$get_object(Bucket = "scoavoux", Key = "records_w3/survey/RECORDS_Wave3_apr_june_23_responses_corrected.csv")
  survey <- f$Body %>% rawToChar() %>% data.table::fread() %>% tibble()

  omni_survey_sum_genres_played <- survey %>%
    select(hashed_id, matches("B_genres_\\d+")) %>%
    pivot_longer(-hashed_id) %>%
    filter(value != "") %>%
    mutate(name = 1) %>%
    summarise(omni_survey_sum_genres_played = sum(name), .by = hashed_id)

  # Recoding data on liking genres
  omni_survey_sum_genres_liked <- make_genre_preference_data() %>%
    # only loved and liked genres
    filter(group %in% c("0", "1")) %>%
    summarize(omni_survey_sum_genres_liked = n(), .by = hashed_id)

  omni <- omni_survey_sum_genres_played %>%
    full_join(omni_survey_sum_genres_liked)
  return(omni)
}


## Peterson: sum of genres declared
### in survey: sum over genres consumed
### in survey: sum over genres liked

## diversity over genres actually listened
### HHI over stream by genre

## diversity over mean legitimacy of genres
### let us attribute to each artist the average legitimacy of their genre
### weighted mean and sd of this,
### as highbrow dim (mean) and omnivorous dim (sd)

## diversity over individual legitimacy
### weighted mean and sd of each artist's average legitimacy
### as highbrow dim (mean) and omnivorous dim (sd)
