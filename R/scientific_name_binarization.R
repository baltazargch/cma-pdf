detect_all_names <- function(names, to_search){
  # names <- Names
  # to_search <- db$title
  flg <- vector(length = length(names))
  
  for(i in unique(to_search)){
    flg <- flg + str_detect(names, str_split(i, ' ', simplify = TRUE)[1]) 
    flg <- flg + str_detect(names, str_split(i, ' ', simplify = TRUE)[2])
  }
  return(flg)
}

make_abbv <- function(name){
  # name <- Names
  sapply(name, \(x){ 
    # x <- spList[21]
    x <- x %>% str_squish %>% str_split(' ', simplify = TRUE)
    
    hassubGenus <- x[-1] %>% str_sub(1,1) %>% str_detect(., '[A-Z]') %>% any
    
    if(hassubGenus) x  <- x[ -(which(x[-1] %>% str_sub(1,1) %>% str_detect(., '[A-Z]')) + 1) ]
    l <- x %>% length
    
    if(l == 1){
      NA
    } else if(l == 2){
      if(all(str_detect(str_sub(x, 1 ,1), '[A-Z]'))) {
        NA
      } else {
        paste(paste0(str_sub(x[1], 1, 1), '.'), x[2], collapse = ' ')
      }
    } else if(l > 2){
      paste( paste0(str_sub(x[1:(l-1)], 1, 1), '.', collapse = ' '), x[l], collapse = ' ' )
    }
  }, USE.NAMES = F) %>% unlist()
}

# make_pad <- function(string) 
# {
#   # string <- Names[1:1000]
#   out <- c()
#   for(s in string){
#     out <- c(out, 
#              # s,
#              paste0(s, ' '), 
#              paste0(' ', s)
#     )
#   }
#   return(out)
# }
# 
# atomic_names <- function(names){
#   # names <- Names
#   out <- c()
#   for(x in names) out <- c(out, str_split(x, ' ', simplify = T))
#   out <- unique(out) %>% na.omit()
#   out[ out != '']
#   out <- tibble(out = out, count = str_width(out))
#   out <- out %>% filter(count > 1) %>% arrange(-count) %>% .$out  
#   return(out)
# }
# 
# make_italics <- function(data, name){
#   data <- str_squish(data)
#   for(x in name) {
#     data <- str_replace_all(data, x, paste0('\\\\textit{', 
#                                             str_squish(x), '} ')) %>% str_squish()
#     }
#   
#   return(data)
# }
# 
# CMA_italize_db <- function(data, names){
#   # require(taxadb)
#   require(tidyverse)
#   
#   data <- read_csv('data/especies_nativas.csv')
#   data <- data %>% filter(title == 'Leopardus wiedii')
#   
#   dataIita <- data %>% 
#     select(-c(sp_id:sp_nombres_comunes_portugues, sp_autores_de_ficha:last_col())) 
#   
#   dataIita <- lapply(1:ncol(dataIita), \(x) make_italics(dataIita[[x]], names))
#   
#   dataOut <- cbind(
#     data %>% 
#       select(sp_id:sp_nombres_comunes_portugues, sp_autores_de_ficha:last_col()), 
#     dataIita %>% do.call(cbind, .)
#   ) %>% as_tibble()
#   return(dataOut)
# }

