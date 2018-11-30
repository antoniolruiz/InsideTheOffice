library(gsheet)
library(dplyr)
library(ggplot2)

theoffice_url <- 'docs.google.com/spreadsheets/d/18wS5AAwOh8QO95RwHLS95POmSNKA2jjzdt0phrxeAE0'
theoffice_df <- gsheet2tbl(theoffice_url)

# Scenes per episode
num_scenes_df <- theoffice_df %>% 
  group_by(season, episode) %>% 
  summarise(num_scenes = max(scene))

ggplot(data = num_scenes_df, aes(x = factor(season), y = num_scenes)) +
  geom_boxplot() +
  xlab("season") +
  ylab("scenes per episode")

# Scenes per episode per character
main_characters = c("Michael", "Jim", "Dwight", "Pam")
characters_scenes_df <- theoffice_df %>% 
  filter(speaker %in% main_characters) %>% 
  mutate(speaker = factor(speaker, levels = main_characters)) %>% 
  group_by(season, episode, speaker) %>% 
  summarise(num_scenes = length(table(scene)))

ggplot(data = characters_scenes_df, aes(x = factor(season), y = num_scenes)) +
  geom_boxplot() +
  xlab("season") +
  ylab("scenes per episode") +
  facet_wrap(~speaker)

# Lines per scene per episode
lines_per_scene_df <- theoffice_df %>% 
  group_by(season, episode, scene) %>% 
  summarise(lines = n())

ggplot(data = lines_per_scene_df, aes(x = lines)) +
  geom_histogram(bins = 20, aes(y=..density..)) +
  facet_wrap(~factor(season)) +
  xlab("lines per scene") 
