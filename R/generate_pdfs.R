library(knitr)
library(sessioninfo)
library(rmarkdown)
library(purrr)

library(furrr)
future::plan(multisession)

library(tictoc)

species <- c(
  'Blastocerus dichotomus', 
  'Necromys lilloi', 
  'Puma concolor', 
  'Rhyncholetes raphanurus'
)

dir.create('pdfs', recursive = T, showWarnings = F)

my_render <- function(x){
  # if(file.exists(paste0('pdfs/', x, '.pdf'))){
  #   NULL
  # } else {
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
  # }
}

tic()

future_map(species, ~ try({my_render(.x)}))

toc()
