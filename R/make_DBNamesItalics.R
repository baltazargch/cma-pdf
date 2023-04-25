dt_names <- read_csv('data/msw3-all.csv')
namesOut <- dt_names %>% filter(TaxonLevel  %in% c("GENUS",      
                                       "SPECIES",    
                                       "SUBSPECIES",
                                       "SUBGENUS" )) %>% 
  mutate(across(Genus:Subspecies, ~ifelse(is.na(.x), '', .x))) %>% 
  mutate(Name = str_c(Genus, Subgenus, Species, Subspecies, sep = ' ') %>%
           str_trim()) %>% 
  select(Name) %>% distinct() %>% as.vector() %>% unlist()

namesOut <- c(namesOut, db$title) %>% unique()

write_csv(tibble(Names = namesOut), 'data/Names_italics.csv')
 