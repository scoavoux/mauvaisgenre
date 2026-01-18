# Graph artistes
library(targets)
tar_load(artists)
library(tidyverse)
set_ggplot_options()

x <- select(artists, name, leg_endo_isei, leg_exo_score, genre, control_n_users) %>% 
  filter(!is.na(leg_exo_score),
         !(genre %in% c("reggae", "country")))
y <-  x %>% 
  filter(!is.na(name)) %>% 
  group_by(genre) %>% 
  filter(control_n_users > quantile(control_n_users, .75)) %>% 
  ungroup()
  
artists_to_plot <- y %>% 
  mutate(resid = lm(leg_endo_isei ~ leg_exo_score, data=y)$residuals,
         abs_resid = abs(resid)) %>% 
  group_by(genre) %>% 
  arrange(genre, desc(abs_resid)) %>% 
  slice(1:5) %>% 
  select(name, leg_endo_isei, leg_exo_score, genre)

z <- x %>% 
  filter(genre %in% c("classical", "rock", "frenchsongs", "jazz", "pop", "frenchrap", "raphiphop",
                      "metal", "edm")) %>% 
  mutate(genre = recode_vars(genre, "cleangenres"))
artists_to_plot <- read_csv("artists_unidim.csv") %>% 
  mutate(across(starts_with("leg"), ~.x/100),
         genre = recode_vars(genre, "cleangenres"))

ggplot(z, aes(leg_endo_isei, leg_exo_score)) +
  geom_point(size=.5, alpha = .1) +
  geom_smooth(method = "lm") +
  geom_text(aes(label = name), data = artists_to_plot) +
  facet_wrap(~genre, scales="free") +
  labs(y = "Maven score (Sens critique)", x = "Average social status of audience (ISEI)")



for(g in unique(z$genre)){
  gg <- ggplot(filter(z, genre == g), aes(leg_endo_isei, leg_exo_score)) +
    geom_point(size=.5, alpha = .2) +
    geom_smooth(method = "lm") +
    geom_text(aes(label = name), data = filter(artists_to_plot, genre == g), size = 2.8) +
#   facet_wrap(~genre, scales="free") +
    labs(y = "Maven score (Sens critique)", x = "Average social status of audience (ISEI)")
  filename <- paste0("genre_plot_", g, ".png")
  ggsave(filename, gg)
  
}

# Intra-genre diversity
library(gghalves)
z <- z %>% 
  mutate(genre = fct_reorder(genre, leg_exo_score, median))
ggplot(z, aes(leg_exo_score, genre)) +
  theme_minimal(base_size = 14) +
  geom_point(position = position_jitter(width = 0, height = .4), size = 1, alpha = .2) +
  geom_boxplot(outliers = FALSE) +
  scale_x_continuous(breaks = 1:10) +
  labs(x = "Maven score (Sens critique)", y = "")

# artists_to_plot2 <- read_csv("artists_unidim.csv") %>% 
#   mutate(across(starts_with("leg"), ~.x/100),
#          genre = recode_vars(genre, "cleangenres"))
# 
# ggplot(z, aes(leg_exo_score, genre)) +
#   theme_minimal(base_size = 14) +
#   geom_boxplot(outliers = FALSE) +
#   geom_text(aes(label = name), data=artists_to_plot2, position = position_jitter(width = 0, height = .4)) +
#   scale_x_continuous(breaks = 1:10, limits = c(1.8,9.5))



# Omni
require(tidyverse)
set_ggplot_options()
tar_load(survey)
tar_load(isei)
s <- survey %>% 
  left_join(isei) %>% 
  select(omni_stream_genres_hhi,
         sd_sc_exo_score,
         isei) %>% 
  filter(!is.na(isei)) %>% 
  pivot_longer(-isei) %>% 
  mutate(isei = cut(isei, breaks = quantile(isei, c(0, .25, .50, .75, 1)), labels = paste0("Q", 1:4)),
         name = factor(name, 
                       levels = c("sd_sc_exo_score", "omni_stream_genres_hhi"),
                       labels = c("Diversity over maven score", "Diversity over music genres")))
s
g <- s %>% 
  filter(!is.na(isei), !is.na(value)) %>% 
  group_by(name) %>% 
  filter(value < quantile(value, .95),
         value > quantile(value, .01)) %>% 
  ggplot(aes(isei, value)) +
  geom_boxplot(notch = TRUE) +
  labs(x = "Social status (ISEI quartiles)", y="Diversity") +
  coord_flip() +
  facet_wrap(~name, scales="free_x")
g
