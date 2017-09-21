## Code to make .CUL file varying the next information
#
## check the cultivar to run into the file .CUL

library(tidyverse)
library(stringr)
library(magrittr)

file <- 'D:/CIAT/Ayudas_2017/Patricia Alvarez/DSSAT_bean/data/Experiment_data/Barichara/BNGRO046.CUL'
cultivar <- 'IB0035'

## sel_cul(file, cultivar)
sel_cul <- function(file, cultivar){
  
  check_cul <- read_lines(file) %>%
    str_subset(pattern = cultivar) 
  
  # return TRUE or FALSE when the cultivar exists
  
  if(identical(check_cul, character(0))){
    
    print(paste("The cultivar doesn't exist into", basename(file)))
    
  }else{
    
    cul_run <- check_cul %>%
      str_split("  ") %>%
      magrittr::extract2(1) %>%
      magrittr::extract(1)
    
    print(paste("Cultivar to run", cul_run))
    
  }
  
}


## 

# inputs_df <- read_csv(paste0('data/rangos_coeficientes.csv'))
make_combination <- function(file, inputs_df, cultivar){
  
  # This are the parameters @VAR#  VRNAME.......... EXPNO   ECO#  CSDL PPSEN EM-FL FL-SH FL-SD SD-PM FL-LF LFMAX SLAVR SIZLF  XFRT WTPSD SFDUR SDPDV PODUR THRSH SDPRO SDLIP
  
  header_cul <- read_lines(file) %>%
    str_subset(pattern = '@VAR') %>%
    scan(text = ., what = "character")
  
  find_cul <- read_lines(file) %>%
    str_detect(pattern =  cultivar) %>%
    which()%>%
    - 1 

    ## the last value needs to change making a substraction by 1
    widths_cul <- read_lines(file) %>%
      str_subset(pattern = '@VAR') %>%
      str_match_all("\\s*\\S+") %>%
      magrittr::extract2(1) %>% 
      str_replace_all(pattern = '($)', "\\1 ") %>%
      str_replace_all(pattern = '(^[[:space:]])', "") %>%
      str_count(boundary("character")) 
    
    cul_df <- read_fwf(file, fwf_widths(widths_cul, col_names = header_cul), skip = find_cul, n_max = 1, col_types = cols())
    
    # match between the variables that we have in the inputs to the .CUL file  
    ## pensar como hacer el match para variar los parametros que se indican en el input file
    
  proof <- inputs_df %>%
    magrittr::extract2(1) 
  
  header_cul %>%
    data_frame(coefficients = .) %>%
    filter(coefficients %in% proof)
  %>%
    str_match(header_cul, .)
    coefficients, header_cul
    
  
  header_cul
  
  str_match()
  
}




file.remove(grep("*.OUT", list.files('D:/CIAT/Ayudas_2017/Patricia Alvarez/DSSAT_bean/data/Experiment_data/Barichara/', full.names = T), value = T))

