CMA_italize_binomial <- function(data, exclude.col='title'){
  require(tidyverse)
  require(taxadb)

  data <- read_csv('data/especies_nativas.csv')[1,]
  spList <- c(filter_rank(name='Mammalia', 
                          rank='Class', 
                          'col')$scientificName, 
              filter_rank(name='Mammalia', 
                          rank='Class', 
                          'itis')$scientificName, 
              read_csv('data/especies_nativas.csv')$title) %>% unique()
  data.mod <- data %>% 
    # filter(title == 'Blastocerus dichotomus') %>%
    select(-sp_imagenes_de_la_especie , -title) %>% as.list()
  
  data.mod <- lapply(data.mod, \(x){
    x <- data.mod$sp_id
    
    for(y in spList){ 
    genus <- str_split(y, ' ', simplify = T)[1]
      epite <- paste0(str_remove_all(genus, '[a-z]'), '. ',
                      str_split(y, ' ', simplify = T)[2])
      x <- x  %>% 
        str_replace_all(genus, paste0('\\\\textit{', genus, '}')) %>%
        str_replace_all(epite, paste0('\\\\textit{', epite, '}'))
    }
  })
return(data.mod)
}

