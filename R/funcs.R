#### Parse authors ----
CMA_parse_authors <- function(data, authors_csv ='data/lista_de_autores.csv'){
  # data <- sp_db
  list_auth <- read.csv(authors_csv)
  stopifnot(nrow(data) > 0, nrow(list_auth) > 0)
  require(tidyverse)
  
  has_collabs <- !is.na(data[,122])
  authors <- data[,121] %>% 
    str_split(";") %>% 
    unlist() %>% 
    str_trim(side = 'both')
  
  authors_data <- list_auth %>% 
    filter(name  %in% authors)
  
  if(nrow(authors_data) == 0){
    stop('Autores no encontrados')
  } else if(nrow(authors_data)!= length(authors)){
    stop('No todos los autores fueron encontrados.')
  }
  
  if(has_collabs){
    collabs <- data[,122] %>% 
      str_split(";") %>%
      unlist() %>% 
      str_trim(side = 'both')
    
    collabs_data <- list_auth %>% 
      filter(name  %in% collabs)
    
    if(nrow(collabs_data) == 0){
      stop('Colaborador no encontrado')
    }
    return(list(autores = authors_data, colaboradores = collabs_data))
    break
  }
  
  return(list(autores = authors_data))
}

#### Parse conservation ----

##### Parse subpopulation ----
CMA_subpop_cons <- function(data){
  require(tidyverse)
  require(kableExtra)
  spListPop <- data$group_sp_eval_subpob %>% str_split('\n') %>% unlist() %>% str_trim()
  
  
  spListPop <- spListPop[ spListPop != ""]
  
  subPob <- spListPop[which(str_detect(spListPop, 'Subpoblación')) + 1]
  catPob <- spListPop[which(str_detect(spListPop, 'Subpoblación')) + 3]
  criPob <- spListPop[which(str_detect(spListPop, 'Subpoblación')) + 5]
  
  tbsPob <- vector()
  for(i in seq_along(subPob)){
    tbsPob[i] <- tribble(
      ~'Subpoblación', ~'Categoría', ~'Criterios y subcriterios',
      subPob[i], catPob[i], criPob[i]
    ) %>% 
      kbl(booktabs = T, format = 'latex') %>% 
      row_spec(0,bold=TRUE, extra_latex_after = "\\arrayrulecolor{white}") %>% 
      kable_styling(latex_options = c('striped', "HOLD_position"),
                    position = "center", full_width = T)
  }
  return(tbsPob)
}