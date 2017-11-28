# Make analysis
library(sensitivity)
library(tidyverse)
library(data.table)
library(rlang)
library(seplyr)
library(rrepast)
library(forcats)
# library(dtplyr)

source('main_functions.R')
dir_experiment <- "data/Experiment_data/Barichara/"
dssat_sim <- fread("Z:/Agroclimas/Modelación_frijol/sensibilidad/Barichara_response_sobol2007_stress.csv")  ## variables respuesta
dssat_sim <- dssat_sim %>%
  tbl_df()
sens_dssat <- readRDS("Z:/Agroclimas/Modelación_frijol/sensibilidad/Barichara_sobol2007_stress.rds")  ### cargar sobol (estamos utilizando 2007)


# choose the variable to make the analysis

y <- summarise_group(dssat_sim, group = "id_run", x = "HWAMS") 

make_sobol <- tell_sobol(sens_dssat, y)  ## sobol para la variable de interes


graphs_sobol(make_sobol, type = 1) +
  theme(axis.text.x=element_text(size=11.5)) +
  theme(axis.text.y=element_text(size=11.5))+
  lims(y = c(0,1.5))

write.csv(make_sobol$S, file = 'Z:/Agroclimas/Modelación_frijol/sensibilidad/graphs/Palmira_HWAMS1.csv', row.names = TRUE)

ggsave(paste0('Z:/Agroclimas/Modelación_frijol/sensibilidad/graphs/Palmira_HWAMS1.jpeg'), width = 20, height = 10, units = "cm")    
rm(list = ls())



###



y <- dssat_sim %>%
  group_by(id_run) %>%
  dplyr::summarise(mean(!!variable)) %>%
  select(yield_mean) %>%
  as.matrix()

rrepast::Plot.Sobol

d <- sens_dssat




params <- data_frame(params = rownames(sens_dssat$S))
main_effect <- sens_dssat$S %>%
  bind_cols(params) %>%
  mutate(params = fct_reorder(params, original, .desc = TRUE)) 

ggplot() + 
  geom_bar(data = main_effect, aes(x = params, y = original), stat = "identity") +
  geom_errorbar(data = main_effect, aes(ymin = `min. c.i.`, ymax = `max. c.i.`, x = params), colour = "black", width = 0.1) +
  labs(x = expression(paste("Parameter")), y = expression(S[i])) + 
  
  
  d$group <- rownames(obj$S)
y.label <- labs(y = expression(S[i]))



grouped_mean <- function(data, grouping_variables, value_variables) {
  data %>%
    group_by_at(grouping_variables) %>%
    mutate(count = n()) %>%
    summarise_at(c(value_variables, "count"), mean, na.rm = TRUE) %>%
    rename_at(value_variables, funs(paste0("mean_", .)))
}

starwars %>% 
  grouped_mean(data = starwars, grouping_variables = "eye_color", value_variables= c("mass", "birth_year"))
