## write BNGRO046.CUL


x <- make_combination(file = 'data/Experiment_data/Barichara/BNGRO046.CUL',
                 inputs_df = read_csv(paste0('data/rangos_coeficientes.csv')), 
                 cultivar = 'IB0035', 
                 k = 1) 

write_cul <- function(matrix_cul, out_dir){
  
  # matrix_cul <- x
  
  ## make the variables to type in the .CUL
  cultivar_id <- magrittr::extract2(matrix_cul, 1)
  cultivar_name <- magrittr::extract2(matrix_cul, 2)
  ecotype <- magrittr::extract2(matrix_cul, 4)
  CSDL <- magrittr::extract2(matrix_cul, 5)
  PPSEN <- magrittr::extract2(matrix_cul, 6)
  EM_FL <- magrittr::extract2(matrix_cul, 7)
  FL_SH <- magrittr::extract2(matrix_cul, 8)
  FL_SD <- magrittr::extract2(matrix_cul, 9)
  SD_PM <- magrittr::extract2(matrix_cul, 10)
  FL_LF <- magrittr::extract2(matrix_cul, 11)
  LFMAX <- magrittr::extract2(matrix_cul, 12)
  SLAVR <- magrittr::extract2(matrix_cul, 13)
  SIZLF <- magrittr::extract2(matrix_cul, 14)
  XFRT <- magrittr::extract2(matrix_cul, 15)
  WTPSD <- magrittr::extract2(matrix_cul, 16)
  SFDUR <- magrittr::extract2(matrix_cul, 17)
  SDPDV <- magrittr::extract2(matrix_cul, 18)
  PODUR <- magrittr::extract2(matrix_cul, 19)
  THRSH <- magrittr::extract2(matrix_cul, 20)
  SDPRO <- magrittr::extract2(matrix_cul, 21)
  SDLIP <- magrittr::extract2(matrix_cul, 22)
  
  
  sink(paste0(out_dir, '/BNGRO046.CUL'), append = F)
  
  cat("@VAR#  VRNAME.......... EXPNO   ECO#  CSDL PPSEN EM-FL FL-SH FL-SD SD-PM FL-LF LFMAX SLAVR SIZLF  XFRT WTPSD SFDUR SDPDV PODUR THRSH SDPRO SDLIP")
  # 
  cat("\n")
  cat(paste(sprintf("%6s", cultivar_id),
            sprintf("%-16s", cultivar_name), 
            sprintf("%5s", '.'),
            sprintf("%6s", ecotype),
            sprintf("%5.2f", CSDL),
            sprintf("%5.3f", PPSEN),
            sprintf("%5.1f", EM_FL),
            sprintf("%5.1f", FL_SH),
            sprintf("%5.1f", FL_SD), 
            sprintf("%5.2f", SD_PM),
            sprintf("%5.2f", FL_LF),
            sprintf(" %04.2f", LFMAX),
            sprintf("%4i.", as.integer(SLAVR)),
            sprintf("%5.1f", SIZLF),
            sprintf(" %4.2f", XFRT),
            sprintf("%05.3f", WTPSD),
            sprintf(" %4.1f", SFDUR), 
            sprintf(" %4.2f", SDPDV),
            sprintf(" %4.1f", PODUR),
            sprintf(" %4.1f", THRSH),
            "", sub("^(-?)0.", "\\1.", sprintf("%.3f", SDPRO)),
            "", sub("^(-?)0.", "\\1.", sprintf("%.3f", SDLIP))
  ))
  sink()
}

write_cul(x, out_dir = 'Runs/')

## CRGRO046 Modelo de Frijol utilizado 