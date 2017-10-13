
# HWAMS ADAPS MDAPS  CWAMS  to make sensitivity analysis

# library(doSNOW) 


# mu <- 1.0
# sigma <- 2.0
# foo <- function() foreach(i = 1:3) %dopar% { rnorm(i, mean = mu, sd = sigma) }
# 
# x <- foo()

run_dssat <- function(dir_experiment, dir_dssat, dir_coef, cultivar, model, dir_run, id_run, random_cul = NULL){
  
  # dir_base <- paste0(dir_run, id_run)
  
  # make folder to run
  
  dir_run_id <- make_id_run(dir_run, id_run)
  
  # copy files necessary to run
  
  files_running <- files_dssat(dir_dssat, dir_experiment, dir_run_id)
  
  # make_cultivar
  if(is.null(random_cul)){
    
    
   
    random_cul <- make_combination(file = paste0(dir_experiment,  files_running$file_CUL), 
                                   inputs_df = suppressMessages(suppressWarnings(read_csv(dir_coef))) , 
                                   cultivar, 
                                   k = 1) 
    
    # random_cul
    
    # write cultivar
    
    write_cul(random_cul$Cul_parameters, out_dir = dir_run_id)
   
    
  }else{
    
    Cul_parameters <- random_cul$Cul_parameters %>%
      filter(row_number()== id_run)
    
    # write cultivar
    
    write_cul(Cul_parameters, out_dir = dir_run_id)
  }

  
  # read the treatments to run
  
  number_TN <- read_treatments(paste0(dir_run_id, '/', files_running$x_file))
    
  # read region running
  region <- read_region(file = paste0(dir_run_id, '/', files_running$x_file))
  
  
    
  # make batch file
  
  CSMbatch(crop = "BEAN", name = files_running$x_file, filename = paste0(dir_run_id, "/DSSBatch.v46"), 
           number_TN)
  
  ## run
  
  execute_dssat(dir_run_id, model)
  
  # load the information Evaluate.OUT
  evaluate <- read_evaluate(file = paste0(dir_run_id, '/Evaluate.OUT'))
  
  ## select or all the simulate variable
  
  evaluate <- evaluate %>%
    dplyr::select(ends_with('S'))

  # evaluate <- evaluate %>%
  #   select(HWAMS, ADAPS, MDAPS,  CWAMS)
  
  
  ## HWAH yield
  summary <- read_summary(file = paste0(dir_run_id, '/Summary.OUT')) %>%
    select(PDAT) # for now we select this variables to joint with evaluate
    
  
  coef_random <- magrittr::extract2(random_cul, 2) # select the random coefficients
  
  x <- bind_cols(summary, evaluate)  # joint random coefficients with evaluate information
  
  
  run <- cbind(x,  coef_random, row.names = NULL) %>% ## dataframe of coeffcients and variables of response
            tbl_df() %>%
            mutate(id_run = rep(id_run, nrow(.)), region = rep(region, nrow(.))) %>%
            select(id_run, region, everything())
  
  
  unlink(dir_run_id, recursive = TRUE)
  return(list(runs = run, coef_random = coef_random))
  
  
 
  
  
}

## funcion para trabajar con foreach

# run_mult_dssat <- function(n_cores, n, dir_experiment, dir_dssat, dir_coef, cultivar, model, dir_run, random_cul = NULL){
#   
#   cl <- makeCluster(n_cores)
#   registerDoSNOW(cl)  ## For Windows
#   
#   
#   length_run <- n
#   
#   pb <- txtProgressBar(max = length_run, style = 3)
#   progress <- function(n) setTxtProgressBar(pb, n)
#   
#   opts <- list(progress=progress)
#   
#   # iterators <- 1:n 
#   
#   out_simulation <- foreach(i = 1:n , .packages = c('data.table'), .options.snow=opts, .export = 'run_dssat') %dopar% {
#     
#     run_dssat(dir_experiment, dir_dssat, dir_coef, cultivar, model, dir_run, i, random_cul)
#   
#   }
#   
#   runs <- map_df(out_simulation, extract2, "runs")
#     
#   coef_random <- map_df(out_simulation, extract2, "coef_random")
#   
#   stopCluster(cl)
#   close(pb)
#   
#   rm(out_simulation)
#   gc()
#   gc(reset = T)
#   return(list(runs = runs, coef_random = coef_random))
# }

# funcion para integrar con doFuture
run_mult_dssat <- function(){
  
  # cl <- makeCluster(n_cores)
  # registerDoSNOW(cl)  ## For Windows
  
  
  length_run <- n
  # 
  # pb <- txtProgressBar(max = length_run, style = 3)
  # progress <- function(n) setTxtProgressBar(pb, n)
  # 
  # opts <- list(progress=progress)
  
  # iterators <- 1:n 
  
  out_simulation <- foreach(i = 1:n) %dopar% {
    
    run_dssat(dir_experiment, dir_dssat, dir_coef, cultivar, model, dir_run, i, random_cul)
    
  }
  
  runs <- map_df(out_simulation, extract2, "runs")
  
  coef_random <- map_df(out_simulation, extract2, "coef_random")
  
  # stopCluster(cl)
  # close(pb)
  
  rm(out_simulation)
  # gc()
  gc(reset = T)
  return(list(runs = runs, coef_random = coef_random))
}
# n_cores <- 2
# n <- 100   # number of simulations (remember it is using runif values)



# run_mult_dssat(n, dir_experiment, dir_dssat, dir_coef, cultivar, model, dir_run, random_cul = NULL)





