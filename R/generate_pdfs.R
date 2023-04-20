library(knitr)
library(sessioninfo)
library(rmarkdown)
library(purrr)
library(tidyverse)
library(tictoc)

db <- read_csv('data/especies_nativas.csv')

photo <- list.files('photos/', 'g$', full.names = T, recursive = T)

spIn <- map(db$title, 
            ~ photo %>% 
              str_to_lower() %>% str_detect(gsub(' ', '-', str_to_lower(.x))) %>% 
              any()) %>% unlist()

ords <- db$sp_taxonomia_orden[spIn] %>% unique()
species <- db %>% 
  filter(sp_taxonomia_orden %in% ords) %>%
  filter(!sp_cat_nac_conserv_2019  %in% c('NE (No Evaluada')) %>% 
  select(title) %>% unlist() %>% unname() %>% sort()

dir.create('pdfs', recursive = T, showWarnings = F)

my_render <- function(x){
  if(file.exists(paste0('pdfs/', x, '.pdf'))){
    NULL
  } else {
    tic()
    print(x)
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
    toc()
  }
}

map(species, ~ try({my_render(.x)}))

list.files('pdfs', '.tex$', full.names = T) %>% unlink()

ords <- db$sp_taxonomia_orden[ match(list.files('pdfs/', '*.pdf$') %>% str_remove('.pdf$'), 
                                     db$title) ]
paste0('G:/My Drive/CMA pdfs/', ords) %>% walk(~dir.create(.x,F))

dt_to_copy <- tibble(
  basename = list.files('pdfs/', '*.pdf$'), 
  local = paste0(getwd(), '/',  list.files('pdfs/', '*.pdf$', full.names = T)), 
  local_exists = file.exists(local), 
  drive = paste0('G:/My Drive/CMA pdfs/', ords,'/', basename), 
  drive_exists = file.exists(drive)
)

file.copy( 
  dt_to_copy$local[dt_to_copy$local_exists], 
  dt_to_copy$drive[dt_to_copy$local_exists])

db %>%  filter(!sp_cat_nac_conserv_2019  %in% c('NE (No Evaluada'), 
               sp_taxonomia_orden   %in% ords) %>% 
  select(sp_taxonomia_orden) %>% table()

table(ords)
