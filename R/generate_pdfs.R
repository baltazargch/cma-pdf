library(knitr)
library(sessioninfo)
library(rmarkdown)
library(purrr)
library(tidyverse)
library(furrr)

future::plan(multisession, workers=4)

library(tictoc)
# db$sp_cat_nac_conserv_2019 %>% unique()
db <- read_csv('data/especies_nativas.csv')
species <- db %>% filter(sp_taxonomia_orden %in% c('Cetartiodactyla', 'Carnivora', 'Didelphimorphia')) %>%
  filter(!sp_cat_nac_conserv_2019  %in% c('NA (No Aplicable)', 
                                          'NE (No Evaluada')) %>% 
  select(title) %>% unlist() %>% unname() %>% sort()

dir.create('pdfs', recursive = T, showWarnings = F)

my_render <- function(x){
    tic()
  if(file.exists(paste0('pdfs/', x, '.pdf'))){
    NULL
  } else {
    xfun::Rscript_call(
      render, 
      list(
        input = "species_pdf.Rmd", 
        output_format = 'pdf_document', 
        params = list(species = x), 
        output_file = paste0('pdfs/', x, '.pdf'),
        envir = new.env(), 
        clean = TRUE
      )
    )
  }
    toc()
}


map(species, ~ try({my_render(.x)}))

list.files('pdfs', '.tex$', full.names = T) %>% unlink()
