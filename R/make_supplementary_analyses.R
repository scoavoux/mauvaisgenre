make_tbl_coverage <- function(artists_pop, artists){
  require(tidyverse)
  require(tidytable)
  
  tb <- artists_pop %>% 
    left_join(select(artists, artist_id, genre) %>% mutate(included = TRUE)) %>% 
    mutate(included = ifelse(is.na(included), FALSE, included)) %>% 
    summarize(n_artists = n(),
              control_f_l_play = sum(control_f_l_play, na.rm = TRUE),
              control_f_n_play = sum(control_f_n_play, na.rm = TRUE), 
              respondent_f_l_play = sum(respondent_f_l_play, na.rm = TRUE),
              respondent_f_n_play = sum(respondent_f_n_play, na.rm = TRUE), 
              .by = included)
  return(tb)
}
