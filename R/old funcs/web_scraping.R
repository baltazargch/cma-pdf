# library(rvest)
# library(tidyverse)
# 
# source('R/funcs.R', local = TRUE)
# 
# #### single species ####
# sp_page <- get_species('Lontra longicaudis')
# 
# authors  <- get_authors(sp_page)
# 
# citation <- get_citation(sp_page)
# 
# overview <- get_overview(sp_page)
# 
# conservacion <- get_content(sp_page, what = 'conservacion')
# 
# #### multiple species ####
# 
# species <- c(
#   'Sapajus cay', 
#   'Ctenomys sociabilis', 
#   'Rhyncholestes raphanurus', 
#   'Pecari tajacu'
# )
# 
# lapply(species, function(x){
#   sp_page <- get_species(x)
#   sp_page %>% get_citation()
# } )
# 
# 
# authors$Autores

#### combine and conquer ####
library(knitr)
library(sessioninfo)
library(rmarkdown)
library(purrr)

library(furrr)
future::plan(multisession)

library(tictoc)

species <- c(
  'Sapajus cay', 
  'Ctenomys sociabilis', 
  'Rhyncholestes raphanurus', 
  'Pecari tajacu', 
  'Dromiciops gliroides', 
  'Ctenomys maulinus', 
  'Sapajus nigritus', 
  'Myrmecophaga tridactyla', 
  'Puma concolor', 
  'Pudu puda', 
  'Chrysocyon brachyurus'
)

dir.create('fichas/especies/', recursive = T, showWarnings = F)

my_render <- function(x){
  if(file.exists(paste0('fichas/especies/', x, '.pdf'))){
    NULL
  } else {
    xfun::Rscript_call(
      render, 
      list(
        input = "out_word.Rmd", 
        output_format = 'pdf_document', 
        params = list(species = x), 
        output_file = paste0('fichas/especies/', x, '.pdf'),
        envir = new.env(), 
        clean = TRUE
      )
    )
  }
}

tic()

future_map(species, ~ try({my_render(.x)}))

toc()
