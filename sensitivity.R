library(tidyverse)
library(lubridate)
library(ggjoy)
library(ggplot2)
library(ggbeeswarm)
library(hrbrthemes)
library(viridis)
library(viridisLite)

path_data <- 'data/' 
planting <- seq(ymd('1980-04-15'), ymd('2011-04-15'), by = 'years')
evaluate <- 'Evaluate.OUT'

evaluate_df <- read_table(file = paste0(path_data, evaluate), skip = 2)

em_fl_df <- data_frame(TN = 1:10, EM_FL = seq(from = 17, to = 26, by = 1))

evaluate_df <- left_join( evaluate_df, em_fl_df, by = 'TN') %>%
  group_by(TN) %>%
  mutate(date = planting) %>%
  mutate(year = as.factor(year(date))) %>%
  select(EM_FL, TN, date, year, everything()) %>%
  mutate(EM_FL = factor(EM_FL, unique(EM_FL)))  %>%
  ungroup()




ggplot(evaluate_df, aes(x = HWAMS, y = EM_FL)) + 
  geom_joy(scale=3, rel_min_height=0.01) +
  labs(title = 'Sensibilidad EM-FL',
       subtitle = '',
       y = 'EM-FL') +
  theme_joy(font_size = 13, grid = T) +
  theme(axis.title.y = element_blank())

ggplot(evaluate_df) +
  geom_joy(aes(x = HWAMS, y = year, group = year), scale = 10, size = 0.3, rel_min_height = 0.03) +
  geom_jitter()


ggplot(evaluate_df, aes(year, HWAMS, color= EM_FL)) + 
  geom_jitter() +
  geom_quasirandom(varwidth = TRUE) +
  scale_color_viridis(discrete=TRUE)
  theme_bw()
  
  
  
