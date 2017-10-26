## Code to make .CUL file varying the next information
#
## check the cultivar to run into the file .CUL



# file <- 'data/Experiment_data/Barichara/BNGRO046.CUL'
# cultivar <- 'IB0035'

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
  
  ## try to do this funcion that select automatically the order of the columns
  # cul_df
  # default_variables <- varaiables_static
  # vars_df <- variables_to_change
  
  order_cul <- colnames(cul_df)
  
  cul_df <- cul_df %>%
    dplyr::select(!!default_variables) %>%
    cbind(vars_df) %>% 
    tbl_df() %>% 
    dplyr::select(!!order_cul)
  
  #
  # vars_default <- cul_df %>%
  #   dplyr::select(!!default_variables)
  
  return(cul_df = cul_df)
}
## 

# inputs_df <- read_csv(paste0('data/rangos_coeficientes.csv'))
# k cantidad de numeros aleatorios

# make_cul(file, random_vars, cultivar)
make_cul<- function(file, variables_to_change, cultivar){
  
  # file: the .CUL file
  # variables_to_change: is the random parameters
  # cultivar: the name of the cultivar
  
  
  
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
    
    cul_df <- suppressWarnings(read_fwf(file, fwf_widths(widths_cul, col_names = header_cul), skip = find_cul, n_max = 1, col_types = cols())) %>%
      suppressMessages() %>%
      suppressWarnings()
    
    
    # match between the variables that we have in the inputs to the .CUL file  
    
    variables_to_run <- inputs_df %>%
      magrittr::extract2(1)
  
    varaiables_static <- header_cul %>%
      data_frame(coefficients = .) %>%
      filter(!str_detect(coefficients,  paste(variables_to_run, collapse = '|'))) %>%
      magrittr::extract2(1)
    
    
   
     
    # variables_to_change <- random_vars
    
    
    ####
    
    format_cul <- suppressWarnings(read_fwf(file, fwf_widths(widths_cul, col_names = header_cul), skip = find_cul, n_max = 1, col_types = cols(.default = "c"))) %>%
      suppressMessages() %>%
      suppressWarnings() %>%
      summarise_all(funs(num_decimals)) %>%
      gather(vars_cul, digits)  %>%
      filter(str_detect(vars_cul,  paste(variables_to_run, collapse = '|')))

    variables_to_change <- variables_to_change %>%
      gather(vars_cul, values) %>%
      left_join( format_cul, by = 'vars_cul') %>%
      mutate(values_adj = round(values, digits)) %>%
      select(vars_cul, values_adj) %>%
      group_by(vars_cul) %>%
      mutate(id = 1:length(values_adj)) %>%
      spread(vars_cul, values_adj) %>%
      select(-id)
      

    
    
    random_cul <- make_cul_df(cul_df, varaiables_static, variables_to_change)
    
    
    
    return(Cul_parameters = random_cul)
    
    
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


# make_combination(file = 'data/Experiment_data/Incertidumbre/BNGRO046.CUL',
#                  inputs_df = read_csv(paste0('data/rangos_coeficientes.csv')), 
#                  cultivar = 'IB0035', 
#                  k = 1) 
## funcion para escribir el anterior cultivar en el archivo *.CUL



# file.remove(grep("*.OUT", list.files('D:/CIAT/Ayudas_2017/Patricia Alvarez/DSSAT_bean/data/Experiment_data/Barichara/', full.names = T), value = T))

