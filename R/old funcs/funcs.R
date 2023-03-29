parse_species_cma <- function(sp, ..., autotry=TRUE){
  require(rvest)
  require(tidyverse)
  # sp <- 'Sapajus cay'
  sp_p <- str_to_lower(sp) %>% str_replace(' ', '-')
  dots <- list(...)[[1]]
  
  parsed <- paste0('https://cma.sarem.org.ar/es/especie-', dots, '/', sp_p)
  # parsed <- "https://cma.sarem.org.ar/es/especie-nativa/saphajus-cay"
  isPage <- try({read_html(parsed)}, silent = T)
  
  alt_type <- switch(dots,
                     "nativa" = "exotica",
                     "exotica" = "nativa"
  )
  
  if(class(isPage)[1] == "try-error" && autotry){
    cat(
      "Error al intentar descargar la página. Se usó:  \n\n",
      "Link:  ", parsed, "\n\n",
      "Se intentará usar ", paste0("'especie-", alt_type, "',"), 
      "ya que está la opción 'autotry' activada.\n\n", 
      "Link:  ", paste0('https://cma.sarem.org.ar/es/especie-', alt_type, '/', sp_p), 
      "\n\n"
    )
    
    cat("--------- Intento automático de descarga alterna ---------\n\n")
    Sys.sleep(5)
    
    parsed <- paste0('https://cma.sarem.org.ar/es/especie-', alt_type, '/', sp_p)
    
    isPage <- try({read_html(parsed)}, silent = T)
    
    if(class(isPage)[1] == "try-error"){
      cat(
        "Error al intentar descargar la página. Incluso al usar", 
        alt_type, "como tipo de especie. Se usó: \n\n",
        "Link:  ", parsed, "\n\n",
        "Revise el nombre en la página original y vuelva a intentar. \n\n"
      )
    } else {
      cat(
        "La especie", sp, "es", alt_type, ". Se descargó correctamente la información.",
        "Se continuará, pero considere corregir los datos origen de la búsqueda.", 
        "\n\n"
      )
      return(read_html(parsed))
    }
  } else if(class(isPage)[1] == "try-error" && !autotry) {
    cat(
      "Error al intentar descargar la página. Se usó:  \n\n",
      "Link:  ", parsed, "\n\n",
      "Considere buscar la especie en", alt_type, ", revisar que el nombre", 
      "esté bien escrito y vuelva a intantar ó activar el autotry\n\n"
    )
  } else {
    return(read_html(parsed))
  }
}

get_species <- function(sp, type='nativa', autotry=TRUE){
  require(rvest)
  require(tidyverse)
  
  parse_species_cma(sp, type, autotry = autotry )
}

get_habs <- function(sp_page){
  require(rvest)
  require(tidyverse)
  
  details1 <- sp_page %>% 
    html_nodes('summary') %>% 
    html_text()
  
  isAuth <- grep('Autores|Colaboradores', details1)
  
  details1 <- details1[ -isAuth ]
  
  details2 <- sp_page %>% 
    html_nodes('.details-wrapper') %>% 
    html_text()
  
  habs_cons <- details2[ -isAuth ] %>% 
    str_remove_all(c('\n')) %>% 
    str_trim() %>% 
    str_squish() 
  names(habs_cons) <- details1
}

get_multimedia <- function(sp_page){
  require(rvest)
  require(tidyverse)
}

get_citation <- function(sp_page){
  require(rvest)
  require(tidyverse)
  
  cita <- sp_page %>% 
    html_nodes('.content') %>% 
    html_nodes('.view-content')
  
  cita <- cita[[3]] %>% 
    html_text() %>% 
    str_remove_all('\n') %>% 
    str_trim() %>% str_squish()
}

get_authors <- function(sp_page, method='pais'){
  require(rvest)
  require(tidyverse)
  require(datos)
  # method <- 'pais'
  
  details1 <- sp_page %>% 
    html_nodes('summary') %>% 
    html_text()
  
  hasColab <- any(str_detect('Colaboradores', details1))
  
  details2 <- sp_page %>% 
    html_nodes('.details-wrapper') %>% 
    html_text()
  
  whichAuth <- grep('Autores|Colaboradores', details1)
  
  if(!hasColab){
    authors <- vector('list', 1)
    authors[[ 'Autores' ]] <- details2[ grep('Autores', details1) ] %>% 
      str_split('\n') %>% 
      unlist() %>% 
      str_trim() %>% 
      ifelse(. == "", NA, .) %>% 
      na.omit() %>% as.vector()
    
    authors <- compact(authors)
    
    if(method == 'pais'){
      div <- which(authors$Autores %in% as.character(unique(paises$pais) ) )
      start <- 1
      n <- list()
      for(d in div){
        n[[d]] <- rep( which(div == d), length(start:d) )
        start <- d + 1
      }
      div <- n %>% compact() %>% do.call(c, .)
      
      
    } else {
      div <- sapply(1:(length(authors$Autores)/6), function(x){ 
        rep(x, 6)}, simplify = F) %>% do.call(c, .)
    }
    authors$Autores <- split(authors$Autores, div)
  } else {
    
    authors <- vector('list', 2)
    for(i in whichAuth){
      authors[[i]] <- details2[i] %>% 
        str_split('\n') %>% 
        unlist() %>% 
        str_trim() %>% 
        ifelse(. == "", NA, .) %>% 
        na.omit() %>% as.vector()
    } 
    authors <- compact(authors)
    names(authors) <- details1[whichAuth]
    if(method == 'pais'){
      div <- which(authors$Autores %in% as.character(unique(paises$pais) ) )
      start <- 1
      n <- list()
      for(d in div){
        n[[d]] <- rep( which(div == d), length(start:d) )
        start <- d + 1
      }
      div <- n %>% compact() %>% do.call(c, .)
      
    } else {
      div <- sapply(1:(length(authors$Autores)/6), function(x){ 
        rep(x, 6)}, simplify = F) %>% do.call(c, .)
    }
    authors$Autores <- split(authors$Autores, div)
    
    if(method == 'pais'){
      div <- which(authors$Colaboradores %in% as.character(unique(paises$pais) ) )
      start <- 1
      n <- list()
      for(d in div){
        n[[d]] <- rep( which(div == d), length(start:d) )
        start <- d + 1
      }
      div <- n %>% compact() %>% do.call(c, .)
      
    } else {
      div <- sapply(1:(length(authors$Colaboradores)/6), function(x){ 
        rep(x, 6)}, simplify = F) %>% do.call(c, .)
    }
    authors$Colaboradores <- split(authors$Colaboradores, div)
  }
  return(authors)
}

cma_parse_authors <- function(autores){
  require(rvest)
  require(tidyverse)
  # autores <- authors$Autores
  
  autores <- lapply(autores, function(x){
    x <- x[1]
    x <- x %>% str_split(',') %>% unlist() %>% str_trim()
    x[2] <- x[2] %>%
      str_trunc(2, side='right', ellipsis = '.')
    x <- paste(x, collapse = ', ')
    
  })
  if(length(autores) > 3){
    auth <- paste(autores[[1]][1], 'et al., 2019')
  } else {
    auth <- paste(do.call(paste, list(autores, collapse=', ')), ', 2019')
  }
  return(auth)
}

get_overview <- function(sp_page){
  require(rvest)
  require(tidyverse)
  
  id <- sp_page %>% 
    html_nodes('.numero-de-id') %>% 
    html_text()
  
  binom <- sp_page %>% 
    html_nodes('.nombre-cientifico') %>% 
    html_text()
  
  comun <- sp_page %>% 
    html_nodes('.nombre-comun') %>% 
    html_text()
  
  ini <- paste0(c('.categoria', '.criterios', '.justificacion'), '-wrapper')
  
  iniList <- purrr::map(ini, ~html_nodes(sp_page, .x))
  
  iniList <- map(iniList, ~ html_text2(.x) %>% 
                   str_split('\n\n|\n') %>% 
                   unlist() %>% 
                   str_trim())
  
  overview <- list(
    gsub('Nº de ID: ','', id),
    binom, 
    comun, 
    iniList[[1]][2], 
    iniList[[2]][2], 
    str_c(iniList[[3]][-1], collapse = '\n\n')
  )
  names(overview) <- c("id", "binomial", "nombre_comun", 
                       sapply(iniList, function(x){
                         gsub(" ", "_", x[[1]][1]) %>% 
                           str_to_lower()
                       })
  )
  return(overview)
}

get_content <- function(sp_page, what='conservacion'){
  require(rvest)
  require(tidyverse)
  
  titles <- sp_page %>% 
    html_nodes('h3') %>% 
    html_text() %>% 
    str_remove('\n  ') %>% 
    str_remove('\n')
  
  subs <- sp_page %>% 
    html_nodes('.field__label') %>% 
    html_text()
  
  subtitles <- vector('list', length(titles) - 1)
  contents_subs <- c( which(subs == 'Orden') - 1, 
                      which(subs == 'Tendencia poblacional actual') - 1, 
                      which(subs == 'Presencia en el territorio nacional') - 1,
                      grep('Peso', subs)[1] - 1, 
                      which(subs == 'Hábitos') - 1, 
                      which(subs%in% c('Pérdida de hábitat', 
                                       'La especie ¿está presente en áreas naturales protegidas?'))[1] - 1, 
                      which(subs == 'Bibliografía citada') - 1, 
                      which(subs == 'Bibliografía citada') + 1 )
  # subs[contents_subs]
  contents_subs <- na.omit(contents_subs) %>% as.vector()
  names(subtitles) <- names(contents_subs) <-titles[1:length(titles) - 1]
  
  for(i in seq_along(contents_subs)){
    if(i == 1) {
      subtitles[[i]] <- subs[ 1:contents_subs[i] ]
    } else {
      subtitles[[i]] <- subs[ ends:contents_subs[i] ]
    }
    ends <- contents_subs[i]+1
  }
  
  for(tit in names(subtitles)[1]){
    # tit <- names(subtitles)[1]
    noms <- subtitles[[ tit ]]
    
    dt <- data.frame(
      title = tit, 
      subtitles = noms, 
      content = NA
    )
    
    dt <- dt %>% 
      add_row(title = tit, 
              subtitles = 'Categorías nacionales de conservación previas (SAREM)', 
              .after = which(dt$subtitles == 'Categoría Res. SAyDS 1030/04'))%>% 
      add_row(title = tit, 
              subtitles = 'Evaluación global UICN', 
              .after = which(dt$subtitles == 'Año de evaluación')[1])
    
    cat <- sp_page %>% 
      html_nodes('.field') %>%
      html_nodes('.field__label') %>% 
      html_text()
    
    tri <- sp_page %>% 
      html_nodes('.content') %>% 
      html_nodes('.field')
    
    for(nom in noms){
      # nom <- 'País'
      if(nom %in% c('Categorías de conservación actuales en países vecinos', 
                    'Evaluaciones globales previas de UICN')){next}
      ncat <- which(cat[ cat  %in% noms] == nom)
      
      if(is_empty(ncat)) {next}
      
      cont <- tri[ncat] %>% 
        html_elements('.field__item') %>% 
        html_text() %>% 
        str_replace_all('\n', '') %>% 
        str_trim() %>% str_squish()
      
      if(nom  %in% c('País', 'Año', 'Cita', 'Categoría', 
                     'Año de evaluación', 'Criterios de conservación', 
                     'Criterios y subcriterios')){
        to_add <- cont
      } else {
        to_add <- str_c(cont, collapse = '\n')
      }
      dt[ which(dt$subtitles == nom), 'content' ] <- to_add
    }
    dt[ is.na(dt$content), 'content' ] <- 'SUBTITULO'
  }
  
  return(dt)
}

get_titles <- function(sp_page){
  require(rvest)
  require(tidyverse)
  
  titles <- sp_page %>% 
    html_nodes('h3') %>% 
    html_text() %>% 
    str_remove('\n  ') %>% 
    str_remove('\n')
  
  subs <- sp_page %>% 
    html_nodes('.field__label') %>% 
    html_text()
  
  subtitles <- vector('list', length(titles) - 1)
  contents_subs <- c( which(subs == 'Orden') - 1, 
                      which(subs == 'Tendencia poblacional actual') - 1, 
                      which(subs == 'Presencia en el territorio nacional') - 1,
                      grep('Peso', subs)[1] - 1, 
                      which(subs == 'Hábitos') - 1, 
                      which(subs%in% c('Pérdida de hábitat', 
                                       'La especie ¿está presente en áreas naturales protegidas?'))[1] - 1, 
                      which(subs == 'Bibliografía citada') - 1, 
                      which(subs == 'Bibliografía citada') + 1 )
  # subs[contents_subs]
  contents_subs <- na.omit(contents_subs) %>% as.vector()
  names(subtitles) <- names(contents_subs) <-titles[1:length(titles) - 1]
  
  for(i in seq_along(contents_subs)){
    if(i == 1) {
      subtitles[[i]] <- subs[ 1:contents_subs[i] ]
    } else {
      subtitles[[i]] <- subs[ ends:contents_subs[i] ]
    }
    ends <- contents_subs[i]+1
  }
  
  return(subtitles)
}

get_taxonomy <- function(sp_page){
  require(rvest)
  require(tidyverse)
  
  subs <- sp_page %>% 
    html_nodes('.field__label') %>% 
    html_text()
  
  contents <- c(
    which(subs=='Orden'), 
    which(subs=='Comentarios taxonómicos')
  )
  
  subs_section <- subs[ contents[1]:contents[2] ]
  
  cat <- sp_page %>% 
    html_nodes('.effect-none')
  
  cat[[2]] %>% 
    html_text() %>% 
    str_split('\n') %>% unlist() %>% 
    str_replace_all('\n', '') %>% 
    str_trim() %>% str_squish() %>% 
    ifelse(. == '', NA, .) %>% 
    na.omit() -> cont
  
  cont <- cont %>% data.frame()
  
  dt.out <- vector('list', length(subs_section))
  names(dt.out) <- subs_section
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
      dt.out[[i]] <- cont[ range[1]:range[2], ]
  }
  
  return(dt.out)
}
# 
# sp_page <- get_species('Sapajus cay')
# 
# sp_page %>%  get_taxonomy()

# 'conse' = 1, 
# 'taxon' = 2,
# 'evalu' = 3, 
# 'geogr' = 4, 
# 'morfo' = 5, 
# 'etoec' = 6, 
# 'inves' = 7,
# 'bibli' = 8

# biblio <- get_info(sp_page, 'bibli')
# 
# cat(print(biblio))
