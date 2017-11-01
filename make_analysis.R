# Make analysis
library(sensitivity)
library(tidyverse)
library(data.table)
library(rlang)
library(dtplyr)


dir_experiment <- "data/Experiment_data/Palmira/"
dssat_sim <- fread(paste0('outputs/', basename(dir_experiment), "_response.csv"))
dssat_sim <- dssat_sim %>%
  tbl_df()
sens_dssat <- readRDS(paste0('outputs/', basename(dir_experiment), "_sobol2007.rds"))

# choose the variable to make the analysis
variable <- quo(`H#UMS`)
dssat_sim %>%
  select(id_run, !!variable)
  select(id_run, !!enquo(variable))

  summarise_sim(dssat_sim, id_run, `H#UMS`, CWAMS)  
quo(mean(!!variable))
dplyr::enquo(variable)
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