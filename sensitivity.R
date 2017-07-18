install.packages('ggbeeswarm')
install.packages('ggjoy')
library(tidyverse)
library(lubridate)
library(ggjoy)

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


proof <- evaluate_df %>%
  filter(year %in% c(1980, 1981))

evaluate_df %>%
  filter(year %in% 1980)

ggplot(proof, aes(x = HWAMS, y = EM_FL)) + 
  geom_joy(scale=1, rel_min_height=.01) +
  theme_joy(font_size = 13, grid = T) +
  theme(axis.title.y = element_blank()) +
  facet_wrap(~year, scales='free') 

ggplot(proof, aes(x = HWAMS, y = year, group = year)) +
  geom_joy(scale = 10, size = 0.3, rel_min_height = 0.03) +
  facet_wrap(~EM_F, scales='free') 
