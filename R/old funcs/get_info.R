get_info <- function(sp_page, ...){
  require(rvest)
  require(tidyverse)
  
  db <- read.csv('database_subtitles.csv')
  dots <- list(...)[[1]]
  cat <- sp_page %>% 
    html_nodes('.effect-none')
  
  # dots <- 'bibli'
  
  container <- switch(dots, 
                      'conse' = 1, 
                      'taxon' = 2,
                      'evalu' = 3, 
                      'geogr' = 4, 
                      'morfo' = 5, 
                      'etoec' = 6, 
                      'inves' = 7,
                      'bibli' = 8)
  
  cont <- cat[[ container ]] %>% 
    html_text() %>% 
    str_split('\n') %>% unlist() %>% 
    str_replace_all('\n', '') %>% 
    str_trim() %>% str_squish() %>% 
    ifelse(. == '', NA, .) %>% 
    na.omit() %>% data.frame()
  
  if(dots == 'conse'){
  
  sub1 <- cat[[ container ]] %>% 
    html_nodes('.field__label') %>% 
    html_text() %>% 
    str_split('\n') %>% unlist() %>% 
    str_replace_all('\n', '') %>% 
    str_trim() %>% str_squish() %>% 
    ifelse(. == '', NA, .) %>% 
    na.omit()
  
  sub2 <- cat[[ container ]] %>% 
    html_nodes('.field-group-toggler') %>% 
    html_text() %>% 
    str_split('\n') %>% unlist() %>% 
    str_replace_all('\n', '') %>% 
    str_trim() %>% str_squish() %>% 
    ifelse(. == '', NA, .) %>% 
    na.omit()
  
  subs <- c(sub1, sub2)
  subs <- subs[ subs  %in% db$subtitulos ]
  subs <- subs[ match(db$subtitulos, subs, nomatch  = FALSE) ] 
  clas <- db$clase[ db$subtitulos  %in% subs ]
  
  } else {
    
    subs <- cat[[ container ]] %>% 
      html_nodes('.field__label') %>% 
      html_text() %>% 
      str_split('\n') %>% unlist() %>% 
      str_replace_all('\n', '') %>% 
      str_trim() %>% str_squish() %>% 
      ifelse(. == '', NA, .) %>% 
      na.omit()
    
    clas <- rep(1, length(subs))
  }
  
  
  dt.out <- vector('list', length(subs))
  names(dt.out) <- subs
  
  for(i in seq_along(dt.out)){
    if(i != length(dt.out)){
      range <- c(
        which(cont[,1] == names(dt.out[i])) + 1, 
        which(cont[,1] == names(dt.out[i + 1])) - 1
      )
    } else {
      range <- c(
        which(cont[,1] == names(dt.out[i])) + 1, 
        NROW(cont)
      )
    }
    if(clas[i] == 1){
      dt.out[[i]] <- cont[ range[1]:range[2], ]
      
    }else if(clas[i] == 2){
      slist <- cont[ range[1]:range[2], ]
      
      vect <- 1:length(slist) %% 2 == 0
      
      subs1 <- slist[ !vect ]
      cont1 <- slist[ vect ]
      
      inner.list <- vector('list', length(subs1))
      names(inner.list) <- subs1
      
      for(k in seq_along(inner.list)){ inner.list[[k]] <- cont1[[k]]}
      
      dt.out[[i]] <- inner.list
    }
  }
  
  return(dt.out)
}
