## function that copy and paste xfile, soil, .ECO, .SPE, WTH and .EXE in a particular folder\


## files_dssat(dir_dssat, dir_experiment, out_dir)

files_dssat <- function(dir_dssat, dir_experiment, out_dir){
  
  # dir_experiment <- 'data/Experiment_data/Palmira/'
  # dir_dssat <- 'C:/DSSAT46/'
  # out_dir <- 'Runs/'
  library(tidyverse)
  library(stringr)
  
  
  files_necessary <- '.ECO|.SPE|.WTH|.BNX|.SOL|.T|.A'
  
  experiment <- list.files(dir_experiment, full.names = T) %>%
    data_frame(files = .) %>%
    filter(str_detect(files,files_necessary)) %>%
    magrittr::extract2(1)
    
  x_file <- list.files(dir_experiment, full.names = T) %>%
    data_frame(files = .) %>%
    filter(str_detect(files, '.BNX')) %>%
    magrittr::extract2(1) %>%
    basename()
  
  file_CUL <- list.files(dir_experiment, full.names = T) %>%
    data_frame(files = .) %>%
    filter(str_detect(files, '.CUL')) %>%
    magrittr::extract2(1) %>%
    basename()
  
  exe_dssat <- paste0(dir_dssat, 'DSCSM046.EXE')    ## Executable DSSAT v 4.6
  
  
  file.copy(experiment, out_dir, recursive = T)
  file.copy(exe_dssat, out_dir, recursive = T)
  
  return(list(x_file = x_file, file_CUL = file_CUL))
  
}
# to test
# its necessary to add dir_run into a funtion than is goint to run DSSAT with all specification and run into a particular folder
# dir_run <- 'D:/CIAT/USAID/DSSAT/multiple_runs/R-DSSATv4.6/Proof_run/'
# crop <- "BEAN"
# name <- "PASA1703.BNX"  # for linux ./proof.MZX, for windows proof.MZX USAID
# filename <- "DSSBatch.v46"  # filename


# CSMbatch(crop, name, paste0(dir_run, filename))


CSMbatch <- function(crop, name, filename, tn) {
  
  outbatch <- rbind(
    rbind(
      # Batchfile headers            
      paste0("$BATCH(", crop, ")"),            
      "!",            
      cbind(sprintf("%6s %92s %6s %6s %6s %6s", "@FILEX", "TRTNO", "RP", "SQ", "OP", 
                    "CO"))),            
    cbind(sprintf("%93-s %5s %6i %6i %6i %6i",            
                  paste0(name),
                  tn,  # Variable for treatment number            
                  1,  # Default value for RP element            
                  0,  # Default value for SQ element            
                  1,  # Default value for OP element            
                  0)))  # Default value for CO element 
  
  # Write the batch file to the selected folder  
  write(outbatch, file = filename, append = F)
  
}

# execute_dssat(dir_run, model)
execute_dssat <- function(dir_run, model){
  
  # dir_run <- out_dir
  # model <- 'CRGRO046'
  setwd(dir_run)
  system(paste0("DSCSM046.EXE " , model," B ", "DSSBatch.v46"), ignore.stdout = T, show.output.on.console = F)
  setwd('..')
  setwd('..')
  
}

# id_cultivar <- 1
# make_id_run(dir_run, id_cultivar)
make_id_run <- function(dir_run, id_cultivar){
  
  dir_base <- paste0(dir_run, id_cultivar)
  
  if (!dir.exists(paste0(dir_run, id_cultivar))) { 
    
    dir.create(paste0(dir_run, id_cultivar), showWarnings = F, recursive = TRUE, mode = "777")
    # system('chmod 777 *.*')
    # paste0(dir_base, region, '/', cultivar,  '/', select_day)
    
  }
  
  return(dir_base)
}


read_evaluate <- function(file){
  
  suppressMessages(suppressWarnings(read_table(file, skip = 2, col_types = cols())))
  
  
}

# SDAT PDAT    EDAT    ADAT    MDAT    HDAT

read_summary <- function(file){
  
  summary_out <- suppressWarnings(read_table(file, skip = 3 , na = "*******",
                            col_types = cols(SDAT = col_character(),
                                             PDAT = col_character(), 
                                             EDAT = col_character(),
                                             ADAT = col_character(),
                                             MDAT = col_character(),
                                             HDAT = col_character()))) %>%
    mutate(SDAT = as.Date(SDAT, format("%Y%j")), 
           PDAT = as.Date(PDAT, format("%Y%j")), 
           EDAT = as.Date(EDAT, format("%Y%j")),
           ADAT = as.Date(ADAT, format("%Y%j")),
           MDAT = as.Date(MDAT, format("%Y%j")),
           HDAT = as.Date(HDAT, format("%Y%j"))) %>%
    dplyr::select(SDAT, PDAT, everything())
  
  
  return(summary_out)
}

## to read the x-file them keep the number of treatments to run 

read_treatments <- function(file){


  ##
  find_TN <- file %>%
    read_lines() %>%
    str_detect(pattern ="TREATMENTS") %>%
    which() %>%
    -2

  TN <- suppressWarnings(fread(file, autostart = find_TN, showProgress = FALSE)) %>%
    tbl_df() %>%
    magrittr::extract2(1)
  
  return(TN)
}

read_region <- function(file){
  
  line_number <- read_lines(file) %>%
    str_which(pattern = '@SITE') 
  
  region <- read_lines(file, skip = line_number, n_max = 1) %>%
    str_split(pattern = ",") %>%
    magrittr::extract2(1) %>%
    magrittr::extract(1)
  
  return(region)
  
}

num_decimals <- function(x) {
  # stopifnot(class(x)=="numeric")
  
  n_decimals <- str_replace(x, "^.+[.]", "") %>%
    str_count()
  return(n_decimals)
  
}


AddFactor_mod <- function(lambda = "qunif", name, min, max){
  
  AddFactor(lambda = lambda, name = name, min = min, max = max) %>%
    tbl_df()
}


make_sampling <- function(x, k){
  
  ## this make the random numbers following a uniform distribution
  # variables_to_change  <- x%>%
  #   mutate(data = map2(min, max, runif, n = k)) %>%
  #   dplyr::select(coefficients, data) %>%
  #   unnest() %>%
  #   group_by(coefficients) %>%
  #   mutate(id = 1:length(coefficients)) %>%
  #   spread(coefficients, data) %>%
  #   dplyr::select(-id)
  
  ## this make the random numbers following a LHS methodology (Latin Hypercube Sampling)
  
  variables_to_change  <- x %>%
    mutate(LHS = pmap(list(name = coefficients, min = min, max = max), AddFactor_mod)) %>%
    dplyr::select(LHS) %>%
    unnest() %>%
    as.matrix() %>%
    AoE.LatinHypercube(k, .) %>%
    as.data.frame
}

summarise_group <- function(df, group, x){

 
  df %>%
    dplyr::group_by_at(group) %>%
    dplyr::summarise_at(.vars = x, .funs = mean, na.rm = T) %>%
    select_at(x)
    # dplyr::rename_at(x, funs(paste0("mean_", .)))
}

tell_sobol <- function(sobol_model, y){
  
  y <- as.matrix(y)
  sobol_model <- sensitivity::tell(sobol_model, (y-mean(y) /sd(y)))
   
  return(sobo_model)
}


graphs_sobol <- function(sobol_model, type){
      
  # sobol_model <- make_sobol
    if (is.null(sobol_model$S)) {
      stop("Invalid Sobol object instance!")
    }
  
    switch(type, `1` = {
        
      variable <- colnames(sobol_model$y)
      
      main_effect <- sobol_model$S %>%
        tbl_df() %>%
        mutate(params = rownames(.)) %>%
        mutate(params = fct_reorder(params, original, .desc = TRUE)) %>%
        mutate(effect = 'Main_Effect') 
        
      
      p <- ggplot() + 
        geom_bar(data = main_effect, aes(x = params, y = original), stat = "identity") +
        geom_errorbar(data = main_effect, aes(ymin = `min. c.i.`, ymax = `max. c.i.`, x = params), colour = "black", width = 0.1) +
        labs(x = expression(paste("Parameters")), y = expression(S[i])) +
        ggtitle(variable) +
        theme_bw()
                        
    }, `2` = {
      
      total_effect <- sobol_model$T %>%
        tbl_df() %>%
        mutate(params = rownames(.)) %>%
        mutate(effect = 'Total_Effect') 
      
      prueba <-  suppressWarnings(dplyr::bind_rows(main_effect, total_effect)) %>%
        mutate(params = fct_reorder(params, original, .desc = TRUE))
  
     
      
     p <- ggplot() + 
        geom_bar(data = prueba, aes(x = params, y = original, fill = effect),
                 stat = "identity",
                 position = "dodge") +
        scale_fill_grey(start = 0.4, end = 0.8)  +
        theme_bw() +
        ggtitle(variable) +
        labs(x = expression(paste("Parameters")), y = expression(S[i]))
        
        
    

    }, stop("Invalid chart type!"))
  
  
 
  


    return(p)
  }


# https://cran.r-project.org/web/packages/mapsapi/index.html
# http://enhancedatascience.com/2017/07/10/the-packages-you-need-for-your-r-shiny-application/?utm_content=buffer86436&utm_medium=social&utm_source=twitter.com&utm_campaign=buffer
# https://github.com/benmarwick/huskydown 
