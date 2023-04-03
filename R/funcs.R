#### Parse authors ----
CMA_parse_authors <- function(data, authors_csv ='data/lista_de_autores.csv'){
  # data <- db.sp
  list_auth <- read.csv(authors_csv)
  stopifnot(nrow(data) > 0, nrow(list_auth) > 0)
  require(tidyverse)
  
  has_collabs <- !is.na(data$sp_colaboradores_de_ficha)
  authors <- data$sp_autores_de_ficha %>% 
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
    collabs <- data$sp_colaboradores_de_ficha %>% 
      str_split(";") %>%
      unlist() %>% 
      str_trim(side = 'both')
    
    collabs_data <- list_auth %>% 
      filter(name  %in% collabs)
    
    if(nrow(collabs_data) == 0){
      stop('Colaborador no encontrado')
      break
    }
    return(list(autores = authors_data, colaboradores = collabs_data))
  }
  
  return(list(autores = authors_data))
}

CMA_print_authors <- function(authors){
  # authors <- CMA_parse_authors(db.sp)
  has_collabs <- length(authors) > 1
  
  cat('**AUTORES**')

  authors[[1]] %>% 
    mutate(aff = str_c(institucion, provincia, pais, sep = ', '), 
           bl = '') %>% 
    select(name, bl, aff) %>% 
    kbl(booktabs = T, format = 'latex', col.names = NULL) %>% 
    kable_styling(latex_options = c('striped', "HOLD_position"),
                  position = "center", full_width = T) %>%
    column_spec(1, bold=TRUE) %>% 
    column_spec(2, width = '2cm') %>% cat()
  
  if(has_collabs){
    cat('**COLABORADORES**')
    authors[[2]] %>% 
      mutate(aff = str_c(institucion, provincia, pais, sep = ', '), 
             bl = '') %>% 
      select(name, bl, aff) %>% 
      kbl(booktabs = T, format = 'latex', col.names = NULL) %>% 
      kable_styling(latex_options = c('striped', "HOLD_position"),
                    position = "center", full_width = T) %>% 
      column_spec(1, bold=TRUE) %>% 
      column_spec(2, width = '2cm') %>% cat()
    
  }
  
  }

#### Parse conservation ----

##### Parse subpopulation ----
CMA_subpop_cons <- function(data){
  require(tidyverse)
  require(kableExtra)
  # db <- read_csv('data/especies_nativas.csv')
  # data <- db %>% filter(title == 'Blastocerus dichotomus')
  
  stopifnot(nrow(data) == 1, ncol(data) > 1)
  
  if(is.na(data$group_sp_eval_subpob)){
    return(NULL)
  } else {
  spListPop <- data$group_sp_eval_subpob %>% 
    str_split('\n') %>% 
    unlist() %>% 
    str_trim()
  
  spListPop <- spListPop[ !spListPop %in%  c("", ",")]
  
  nSubPob <- sum(str_detect(spListPop, 'Subpoblación'))
  
  ies <- data.frame(
    from = which(str_detect(spListPop, 'Subpoblación')),
    to = c( (which(str_detect(spListPop, 'Subpoblación'))[-1]) -1,
            length(spListPop))
  )
  
  stopifnot(nrow(ies) == nSubPob)
  
  dfNames <- c('Subpoblación', 'Categoría', 'Criterios y subcriterios')
  tbsPob <- vector('list', nSubPob)
  for(i in 1:nSubPob){
    # i=1
    items <- c(ies$from[i]+c(1,3,5))
    
    tbsPob[[i]][[1]] <-  data.frame( t(spListPop[ items ]))
    colnames(tbsPob[[i]][[1]]) <- dfNames
    
    tbsPob[[i]][[1]] <-  tbsPob[[i]][[1]] %>% 
      kbl(booktabs = T, format = 'latex') %>% 
      row_spec(0,bold=TRUE, extra_latex_after = "\\arrayrulecolor{white}") %>% 
      kable_styling(latex_options = c('striped', "HOLD_position"),
                    position = "center", full_width = T)
    tbsPob[[i]][[2]] <- paste("**Justificación**\n\n",
                              paste(
                                spListPop[ (max(items)+2):ies$to[i] ],
                                collapse = '\n\n'))
  }
  return(tbsPob)
  }
}

#### Parse titles ----
CMA_print_titles <- function(text){
  
 paste0("\\invisiblesection{", text, "}\n", 
  tibble(
    paste0( "\\rule{0pt}{14pt}", text)
  ) %>% 
    kbl(booktabs = T, col.names = NULL,escape = F) %>% 
    kable_styling(full_width = F, latex_options = c("HOLD_position")) %>% 
    row_spec(1, bold=TRUE, color = 'white', background = "ceil") %>% 
    column_spec(1:2, width = '16cm', latex_valign='m'))
}