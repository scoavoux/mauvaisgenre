make_tbl_coverage <- function(artists_pop, artists){
  require(tidyverse)
  require(tidytable)
  
  tb <- artists_pop %>% 
    left_join(select(artists, artist_id, genre) %>% mutate(included = TRUE)) %>% 
    mutate(included = ifelse(is.na(included), FALSE, included)) %>% 
    summarize(control_f_l_play = sum(control_f_l_play),
              control_f_n_play = sum(control_f_n_play, na.rm = TRUE), 
              .by = included)
  return(tb)
}