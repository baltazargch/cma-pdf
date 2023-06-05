library(knitr)
library(sessioninfo)
library(rmarkdown)
library(purrr)
library(tidyverse)
library(tictoc)

db <- read_csv('data/especies_nativas.csv')

# db %>% select(sp_taxonomia_orden, sp_taxonomia_familia, title)
photo <- list.files('photos/', 'g$', full.names = T, recursive = T)

spIn <- map(db$title, 
            ~ photo %>% 
              str_to_lower() %>% str_detect(gsub(' ', '-', str_to_lower(.x))) %>% 
              any()) %>% unlist()

ords <- db$sp_taxonomia_orden[spIn] %>% unique()
species <- db %>% 
  filter(sp_taxonomia_orden %in% ords) %>%
  # filter(!sp_cat_nac_conserv_2019  %in% c('NE (No Evaluada')) %>% 
  select(title) %>% unlist() %>% unname() %>% sort()

ords <- db$sp_taxonomia_orden[ match(species, db$title) ]
stopifnot(length(ords) == length(species))

paste0('pdfs/', ords) %>% walk(~dir.create(.x,F))

dir.create('pdfs', recursive = T, showWarnings = F)

my_render <- function(x, ord){
  if(file.exists(paste0('pdfs/', ord, '/', x, '.pdf'))){
    NULL
  } else {
    print(x)
    xfun::Rscript_call(
      render, 
      list(
        input = "species_pdf.Rmd", 
        output_format = 'pdf_document', 
        params = list(species = x), 
        output_file = paste0('pdfs/', ord, '/', x, '.pdf'),
        envir = new.env(), 
        clean = TRUE
      )
    )
  }
}
library(parallel)

mclapply(seq_along(species), \(.x) try({my_render(species[.x], ords[.x])}), mc.cores = 10)

list.files('pdfs', '.tex$', full.names = T, recursive = T) %>% unlink()

ords <- db$sp_taxonomia_orden[ match(list.files('pdfs/', '*.pdf$') %>% str_remove('.pdf$'), 
                                     db$title) ]
# paste0('pdfs/', ords) %>% walk(~dir.create(.x,F))

db %>%  
  filter(
    # !sp_cat_nac_conserv_2019  %in% c('NE (No Evaluada'), 
    sp_taxonomia_orden  %in% ords) %>% 
  select(sp_taxonomia_orden) %>% table()

table(ords)
