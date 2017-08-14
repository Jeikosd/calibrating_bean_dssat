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

evaluate_P <- list( 'Evaluate_palmira.OUT', 'Evaluate_santander.OUT')

evaluate_d <- function(evaluate){
  
  evaluate_df <- read_table(file = paste0(path_data, evaluate), skip = 2)
  
  em_fl_df <- data_frame(TN = 1:11, EM_FL = seq(from = 17, to = 27, by = 1))
  
  Region.a<-strsplit(strsplit(evaluate,"_")[[1]][2], ".O")[[1]][1]
  
  
  evaluate_df <- left_join( evaluate_df, em_fl_df, by = 'TN') %>%
    group_by(TN) %>%
    mutate(date = planting) %>%
    mutate(year = as.factor(year(date))) %>%
    select(EM_FL, TN, date, year, everything()) %>%
    mutate(EM_FL = factor(EM_FL, unique(EM_FL)))  %>%
    ungroup() %>% mutate(Region = Region.a)
  
return(evaluate_df)}

evaluate_df <- sapply(X =  evaluate_P, FUN = evaluate_d, simplify = F) %>% 
  bind_rows()
 

ggplot(evaluate_df, aes(x = HWAMS, y = EM_FL, fill= Region)) + 
    geom_joy(scale=3, rel_min_height=0.01,alpha = .5) +
    labs(title = 'Sensibilidad EM-FL', subtitle = '', y = 'EM-FL') +
    theme_joy(font_size = 13, grid = T) +
    theme(axis.title.y = element_blank())

ggsave(filename = "point.png", width = 10, height = 5)



ggplot(evaluate_df, aes(x = HWAMS, y = year, fill = Region)) +
  geom_joy(scale = 10, size = 0.3, rel_min_height = 0.03, alpha = 0.5) + theme_bw()


ggsave(filename = "year.png", width = 10, height = 5)



ggplot(evaluate_df, aes(year, HWAMS, color= EM_FL)) + 
  geom_jitter() + facet_grid(.~Region) + 
  geom_quasirandom(varwidth = TRUE) +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_color_viridis(discrete=TRUE)
  

ggsave(filename = "scatter.png", width = 10, height = 5)

  
## Heatmap 
ggplot(evaluate_df, aes(x = year, y = EM_FL, fill=HWAMS)) +  geom_tile() +
 theme_bw() +  scale_fill_viridis() +  facet_grid(.~Region)+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#+ geom_text(aes(label = round(HWAMS, 2)), size=3) 
  

ggsave(filename = "Heatmap.png", width = 10, height = 5)



## boxplot
ggplot(evaluate_df, aes(x = EM_FL, y= HWAMS, colour=Region)) +
  geom_point(position=position_jitterdodge(dodge.width=0.9), shape =1) +
  geom_boxplot() + theme_bw()

ggsave(filename = "boxplot.png", width = 10, height = 5)






