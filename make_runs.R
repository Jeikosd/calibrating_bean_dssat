library(tidyverse)
library(stringr)
library(magrittr)
library(data.table)
library(doFuture)
library(sensitivity)
library(rrepast)  ### libreria para LHS (Latin hypercube Sampling)
library(pse) ### libreria para LHS (Latin hypercube Sampling)

source("make_cultivar.R")
source("write_cul.R")
source("main_functions.R")
source("run_dssat.R")




## Configuracion

dir_run <- "Runs/"
dir_experiment <- "data/Experiment_data/Palmira/"
# id_run <- 2  # to run in a particular .CUL (generated by runif)
dir_dssat <- 'C:/DSSAT46/'
dir_coef <- 'data/rangos_coeficientes_ajustado.csv'
cultivar <- 'IB0035'
model <- 'CRGRO046'
# random_cul = NULL ## parameters to simulate in DSSAT
cul_file <- paste0(dir_experiment, 'BNGRO046.CUL')

inputs_df <- suppressMessages(suppressWarnings(read_csv(dir_coef)))


x1 <- make_sampling(inputs_df, 10000) 

x2 <- make_sampling(inputs_df, 10000) 



sens_dssat <- sobol(model = NULL, X1 = x1, X2 = x2, order = 1, nboot = 5000)
sens_dssat_Eff <- sobolEff(model = NULL, X1 = x1, X2 = x2, order = 1, nboot = 0)
sens_dssat_sobol2007 <- sobol2007(model = NULL, X1 = x1, X2 = x2, order = 1, nboot = 5000)

# to normal sobol
# random_vars <- sens_dssat$X %>%
  # tbl_df

# to sobol 2007 about small indices
random_vars <- sens_dssat_sobol2007$X %>%
  tbl_df



cul_df <- make_cul(file = cul_file, random_vars, cultivar)
n <- nrow(cul_df) 
## proof
## id_run = 1
# run_dssat(dir_experiment, dir_dssat, cultivar, model, dir_run, id_run, cul_df)

n_cores <- 28
# n <- 100000   # number of simulations (remember it is using runif values)



options(future.globals.maxSize= 891289600)

registerDoFuture()
plan(future::cluster, workers = n_cores)
# future.globals.maxSize
dssat_sim <- run_mult_dssat()

y <- dssat_sim %>%
  group_by(id_run) %>%
  summarise(yield_mean = mean(`H#UMS`)) %>%
  select(yield_mean) %>%
  as.matrix()

sensitivity::tell(sens_dssat, (y-mean(y) /sd(y))) ; plot(sens_dssat)
sensitivity::tell(sens_dssat, y) ; plot(sens_dssat)
sensitivity::tell(sens_dssat_Eff, y) ; plot(sens_dssat_Eff)
sensitivity::tell(sens_dssat_Eff, (y-mean(y) /sd(y))) ; plot(sens_dssat)
sensitivity::tell(sens_dssat_sobol2007, (y-mean(y) /sd(y))) ; plot(sens_dssat_sobol2007)


Plot.Sobol(sens_dssat, type = 1)
Plot.Sobol(sens_dssat_Eff, type = 1)
Plot.Sobol(sens_dssat_sobol2007, type = 1)





# runs <- extract2(dssat_sim, 'runs')

# write_csv(random_vars, paste0('outputs/', basename(dir_experiment), "_random_vars.csv"))
# write_csv(dssat_sim, paste0('outputs/', basename(dir_experiment), "_response.csv"))
# write_csv(dssat_sim, paste0('outputs/', basename(dir_experiment), "_response_stress.csv"))


# write_csv(random_vars, paste0('outputs/', basename(dir_experiment), "_random_vars_sobol2007.csv"))
# write_csv(dssat_sim, paste0('outputs/', basename(dir_experiment), "_response_sobol2007.csv"))
write_csv(dssat_sim, paste0('outputs/', basename(dir_experiment), "_response_sobol2007_stress.csv"))

# saveRDS(sens_dssat, paste0('outputs/', basename(dir_experiment), "_sobol.rds"))
# saveRDS(sens_dssat_Eff, paste0('outputs/', basename(dir_experiment), "_sobol_Eff.rds"))
# saveRDS(sens_dssat_sobol2007, paste0('outputs/', basename(dir_experiment), "_sobol2007.rds"))
saveRDS(sens_dssat_sobol2007, paste0('outputs/', basename(dir_experiment), "_sobol2007_stress.rds"))


# to read
mod2 <- readRDS(paste0('outputs/', basename(dir_experiment), "_sobol2007.rds"))

write_csv(dssat_sim$coef_random, paste0('outputs/', dssat_sim$region, "_param.csv"))

# write_csv(dssat_sim$runs, paste0('outputs/', basename(dir_experiment), "_sim.csv"))
runs <- extract2(dssat_sim, 'runs')
coef_random <- extract2(dssat_sim, 'coef_random')




install.packages('sensitivity')
library(sensitivity)
## make statistics
proof <- runs %>%
  group_by(id_run) %>%
  summarise(mean_yield = mean(HWAMS))

hist(proof$mean_yield)

to_sobol <- coef_random %>%
  left_join(proof, by = 'id_run') 

to_sobol %>%
  filter(row_number()<=50000)



x1 <- to_sobol %>%
  sample_n(10) %>%
  # filter(row_number()<=500) %>%
  select(-id_run, -mean_yield) %>%
  as.matrix()

x2 <- to_sobol %>%
  sample_n(10) %>%
  # filter(row_number()>500) %>%
  select(-id_run, -mean_yield) %>%
  as.matrix()


dssat_fun <- function(X){
  
  
}  

x <- sobol(model = NULL, X1 = x1, X2 = x2, order = 1, nboot = 10)
# x$X <- x1
# y <- x$X %>%
#   tbl_df() %>%
#   left_join(to_sobol, by = colnames(x$X)) %>%
#   select(mean_yield) %>%
#   magrittr::extract2(1) %>%
#   as.matrix()
y <- to_sobol %>%
  summarise(mu = mean(mean_yield), sigma = sd(mean_yield))
y <- rnorm(dim(x$X)[1], y$mu, y$sigma) %>%
  as.matrix()

sensitivity::tell(x, y) ; plot(x)
str(x)
tell( x, (y-mean(y))/sd(y) ); plot(x) #Standardised
print(x)
plot(x)

## paginas sensitivity analysis

https://github.com/jdherman/r-sensitivity-wrapper
https://waterprogramming.wordpress.com/2012/09/19/starting-out-with-the-r-sensitivity-package/
  https://cran.r-project.org/web/packages/sensitivity/sensitivity.pdf
https://stackoverflow.com/questions/44275005/sobol-sensitivity-analysis-in-r
file:///C:/Users/jmesa/Downloads/tutorial_pse%20(1).pdf
https://stats.stackexchange.com/questions/43504/interpreting-results-from-sobol-sensitivity-analysis-in-r
http://www.sciencedirect.com/science/article/pii/S0378429009001531

LHS in R sensitivity
https://gist.github.com/jkeirstead/1730440
https://stackoverflow.com/questions/44275005/sobol-sensitivity-analysis-in-r
  

http://www.fromthebottomoftheheap.net/2017/10/19/first-steps-with-mrf-smooths/?utm_content=buffer4d972&utm_medium=social&utm_source=twitter.com&utm_campaign=buffer
http://naniar.njtierney.com/
  http://visdat.njtierney.com/
  https://flowingdata.com/2017/10/19/american-daily-routine/?utm_content=buffera2723&utm_medium=social&utm_source=twitter.com&utm_campaign=buffer

## Generate a Monte Carlo sample with Sobol' LDQR sequence
https://gist.github.com/jkeirstead/1730440
https://github.com/jdherman/r-sensitivity-wrapper ## Command-line wrapper for the R Sensitivity Package
