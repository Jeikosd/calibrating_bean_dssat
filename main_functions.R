## function that copy and paste xfile, soil, .ECO, .SPE, WTH and .EXE in a particular folder\

files_dssat <- function(dir_dssat, dir_experiment, out_dir){
  
  # dir_experiment <- 'data/Experiment_data/Palmira/'
  # dir_dssat <- 'C:/DSSAT46/'
  # out_dir <- 'Runs/'
  library(tidyverse)
  library(stringr)
  
  
  files_necessary <- '.CUL|.ECO|.SPE|.WTH|.BNX|.SOL'
  
  experiment <- list.files(dir_experiment, full.names = T) %>%
    data_frame(files = .) %>%
    filter(str_detect(files,files_necessary)) %>%
    magrittr::extract2(1)
    
  exe_dssat <- paste0(dir_dssat, 'DSCSM046.EXE')    ## Executable DSSAT v 4.6
  
  
  file.copy(experiment, out_dir, recursive = T)
  file.copy(exe_dssat, out_dir, recursive = T)
  ##
  https://cran.r-project.org/web/packages/mapsapi/index.html
  http://enhancedatascience.com/2017/07/10/the-packages-you-need-for-your-r-shiny-application/?utm_content=buffer86436&utm_medium=social&utm_source=twitter.com&utm_campaign=buffer
  https://github.com/benmarwick/huskydown 
  
  
  
  
}