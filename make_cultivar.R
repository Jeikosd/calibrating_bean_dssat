## Code to make .CUL file varying the next information
#
## check the cultivar to run into the file .CUL

library(tidyverse)
library(stringr)
library(magrittr)

file <- 'data/Experiment_data/Barichara/BNGRO046.CUL'
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

make_cul_df <- function(cul_df, default_variables, vars_df){
  
  # cul_df
  # default_variables <- varaiables_static
  # vars_df <- variables_to_change
  
  order_cul <- colnames(cul_df)
  
  cul_df <- cul_df %>%
    select(!!default_variables) %>%
    cbind(vars_df) %>% 
    tbl_df() %>% 
    select(!!order_cul)
  
  return(cul_df)
}
## 

# inputs_df <- read_csv(paste0('data/rangos_coeficientes.csv'))
# k cantidad de numeros aleatorios


make_combination <- function(file, inputs_df, cultivar, k){
  
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
    
    variables_to_run <- inputs_df %>%
      magrittr::extract2(1)
  
    varaiables_static <- header_cul %>%
      data_frame(coefficients = .) %>%
      filter(!str_detect(coefficients,  paste(variables_to_run, collapse = '|'))) %>%
      magrittr::extract2(1)
    
    
    ## this make the random numbers following a uniform distribution
    variables_to_change  <- inputs_df %>%
      mutate(data = map2(min, max, runif, n = k)) %>%
      select(coefficients, data) %>%
      unnest() %>%
      group_by(coefficients) %>%
      mutate(id = 1:length(coefficients)) %>%
      spread(coefficients, data) %>%
      select(-id)
      
    
    
    random_cul <- make_cul_df(cul_df, varaiables_static, variables_to_change)
    
  
    
    return(list(Cul_parameters = random_cul, coef_random = variables_to_change))
    
    
    # inputs_df %>%
    #   mutate(data = pmap(list(min, max, by), seq)) %>%
    #   select(coefficients, data) %>%
    #   unnest() %>%
    #   group_by(coefficients) %>%
    #   mutate(id = 1:length(coefficients)) %>%
    #   ungroup() %>%
    #   spread(coefficients, data) %>%
    #   select(-id) %>%
    #   filter(row_number()<=8) %>%
    #   expand(!!!grouping) 

  # proof <- paste(variables_to_run, collapse = ',')
  # grouping <- rlang::syms(as.list(variables_to_run))
}


make_combination(file = 'data/Experiment_data/Barichara/BNGRO046.CUL',
                 inputs_df = read_csv(paste0('data/rangos_coeficientes.csv')), 
                 cultivar = 'IB0035', 
                 k = 1) 
## funcion para escribir el anterior cultivar en el archivo *.CUL



file.remove(grep("*.OUT", list.files('D:/CIAT/Ayudas_2017/Patricia Alvarez/DSSAT_bean/data/Experiment_data/Barichara/', full.names = T), value = T))

