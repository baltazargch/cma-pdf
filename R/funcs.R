#### Parse authors ----
CMA_parse_authors <- function(data, authors_csv ='data/lista_de_autores.csv'){
  # data <- db.sp
  list_auth <- read.csv(authors_csv)
  stopifnot(nrow(data) > 0, nrow(list_auth) > 0)
  
  require(tidyverse)
  
  has_collabs <- !is.na(data$sp_colaboradores_de_ficha)
  authors <- data$sp_autores_de_ficha %>% 
    str_split(";", simplify = T) %>% 
    as.vector() %>% 
    str_trim(side = 'both')
  
  authors_data <- list_auth %>% 
    filter(name %in% authors)
  authors_data <- authors_data[ match(authors, authors_data$name),]
  
  if(nrow(authors_data) == 0){
    stop('Autores no encontrados')
  } else if(nrow(authors_data)!= length(authors)){
    stop('No todos los autores fueron encontrados.')
  }
  
  if(has_collabs){
    collabs <- data$sp_colaboradores_de_ficha %>% 
      str_split(";", simplify = T) %>% 
      as.vector() %>% 
      str_trim(side = 'both')
    
    collabs_data <- list_auth %>% 
      filter(name  %in% collabs)
    collabs_data <- collabs_data[ match(collabs, collabs_data$name),]
    
    if(nrow(collabs_data) == 0){
      stop('Colaborador no encontrado')
      break
    }
    return(list(autores = authors_data, colaboradores = collabs_data))
  }
  
  return(list(autores = authors_data))
}

CMA_print_authors <- function(authors){
  # authors <- CMA_parse_authors(db.sp)
  has_collabs <- length(authors) > 1
  
  cat('**AUTORES**\n\n')
  
  authors[[1]] %>% 
    remove_rownames() %>% 
    mutate(
      name = str_c("\\begin{justify}", name, '\\end{justify}' , sep=''),
      aff = str_c("\\begin{justify}",
                  str_c(institucion, provincia, 
                        str_remove(pais, ',$'), sep = ', ')
                  ,'\\end{justify}' , sep=' '), 
      bl = '') %>% 
    select(name, bl, aff) %>%
    kbl(booktabs = T, format = 'latex', escape = F, linesep = '', 
        longtable = T, col.names = NULL) %>% 
    kable_styling(latex_options = c('striped', "HOLD_position"),
                  position = "center", full_width = T) %>%
    column_spec(1, width = '5cm', bold=TRUE) %>% 
    column_spec(2, width = '1cm') %>% 
    column_spec(3, width = '9cm') %>%  cat()
  
  if(has_collabs){
    cat('\n\n**COLABORADORES**\n\n')
    authors[[2]] %>% 
      remove_rownames() %>% 
      mutate(
        name = str_c("\\begin{justify}", name, '\\end{justify}' , sep=''),
        aff = str_c("\\begin{justify}",
                    str_c(institucion, provincia, 
                          str_remove(pais, ',$'), sep = ', ')
                    ,'\\end{justify}' , sep=''),
        bl = '') %>% 
      select(name, bl, aff) %>%
      kbl(booktabs = T, format = 'latex',escape = F, linesep = '', 
          longtable = T, col.names = NULL) %>% 
      kable_styling(latex_options = c('striped', "HOLD_position"),
                    position = "center", full_width = T) %>% 
      column_spec(1, width = '5cm', bold=TRUE) %>% 
      column_spec(2, width = '1cm') %>% 
      column_spec(3, width = '9cm') %>%  cat()
    
  }
  
}


#### Parse conservation ----
##### Parse subpopulation ----
CMA_subpop_cons <- function(data){
  require(tidyverse)
  require(kableExtra)
  # db <- read_csv('data/especies_nativas.csv')
  # data <- db %>% filter(title == 'Blastocerus dichotomus')
  
  stopifnot(nrow(data) == 1, ncol(data) > 1)
  
  if(is.na(data$group_sp_eval_subpob)){
    return(NULL)
  } else {
    spListPop <- data$group_sp_eval_subpob %>% 
      str_split('\n') %>% 
      unlist() %>% 
      str_trim()
    
    spListPop <- spListPop[ !spListPop %in%  c("", ",")]
    
    nSubPob <- sum(str_detect(spListPop, 'Subpoblación'))
    
    ies <- data.frame(
      from = which(str_detect(spListPop, 'Subpoblación')),
      to = c( (which(str_detect(spListPop, 'Subpoblación'))[-1]) -1,
              length(spListPop))
    )
    
    stopifnot(nrow(ies) == nSubPob)
    
    dfNames <- c('Subpoblación', 'Categoría', 'Criterios y subcriterios')
    tbsPob <- vector('list', nSubPob)
    for(i in 1:nSubPob){
      # i=1
      items <- c(ies$from[i]+c(1,3,5))
      
      tbsPob[[i]][[1]] <-  data.frame( t(spListPop[ items ]))
      colnames(tbsPob[[i]][[1]]) <- dfNames
      
      tbsPob[[i]][[1]] <-  tbsPob[[i]][[1]] %>% 
        kbl(booktabs = T, format = 'latex') %>% 
        row_spec(0,bold=TRUE, extra_latex_after = "\\arrayrulecolor{white}") %>% 
        kable_styling(latex_options = c('striped', "HOLD_position"),
                      position = "center", full_width = T)
      tbsPob[[i]][[2]] <- paste("**Justificación**\n\n",
                                paste(
                                  spListPop[ (max(items)+2):ies$to[i] ],
                                  collapse = '\n\n'))
    }
    return(tbsPob)
  }
}

##### Parse neighbors countries ----

CMA_neighbor_countries <- function(db){
  db <- db %>% select(group_sp_eval_paises_vecinos) %>% unlist()
  db <- db %>% 
    str_split('\n;', simplify = T) %>% 
    str_remove('\n') %>% 
    map(~str_split(.x, ',', simplify = T) %>%  str_trim()) %>% 
    do.call(rbind, .) %>% as.data.frame() %>% 
    select(País=1, Categoría=2, Cita=3) %>% 
    mutate(Año = as.numeric(str_remove_all(Cita, '[^0-9]'))) %>% 
    select(País, Categoría, Año, Cita) %>% 
    arrange(-Año) %>% 
    mutate(Año = as.character(Año)) %>% 
    split(., .$País) 
  
  db %>% map(~.x %>% remove_rownames %>% 
               kbl(booktabs = T, format = 'latex') %>% 
               row_spec(0,bold=TRUE, 
                        extra_latex_after = "\\arrayrulecolor{white}") %>% 
               kable_styling(latex_options = c('striped', "HOLD_position"),
                             position = "center", full_width = T))
  
}

#### Parse titles ----
CMA_print_titles <- function(text){
  
  paste0("\\invisiblesection{", text, "}\n",
         tibble(
           paste0( "\\rule{0pt}{14pt}", text)
         ) %>% 
           kbl(booktabs = T, col.names = NULL,escape = F) %>% 
           kable_styling(full_width = F, 
                         latex_options = c("HOLD_position")) %>% 
           row_spec(1, bold=TRUE, color = 'white', background = "ceil") %>% 
           column_spec(1:2, width = '16cm', latex_valign='m')
  )
}

#### Parse taxonomy ----

CMA_parse_taxonomy <- function(data){
  # data <- db.sp
  
  nombCienti <- paste0('\\textit{', data$title,'}', ' ', 
                       str_remove(data$sp_nombre_cientifico, data$title) %>% str_trim())
  
  dbList <- vector('list', 7)
  dbList[[1]] <- tibble("Orden", '', data$sp_taxonomia_orden)
  dbList[[2]] <- tibble("Familia", '', data$sp_taxonomia_familia) 
  dbList[[3]] <- tibble("Nombre científico", "", nombCienti)
  dbList[[4]] <- tibble("Nombre común", '', data$sp_nombre_comun) 
  
  n <- data$sp_nombres_comunes_locales %>% str_split(',') %>% unlist()
  dbList[[5]] <- tibble(c("Nombres comunes locales", 
                          rep("", length(n)-1)), '', n) 
  
  n <- data$sp_nombres_comunes_ingles %>% str_split(',') %>% unlist()
  dbList[[6]] <- tibble(c("Nombres comunes en inglés", 
                          rep("", length(n)-1)), '', n)  
  
  n <- data$sp_nombres_comunes_portugues %>% str_split(',') %>% unlist()
  dbList[[7]] <- tibble(c("Nombres comunes en portugués", 
                          rep("", length(n)-1)), '', n)
  
  dbList <- dbList %>%  
    lapply(\(x) if(is.na(x[[3]][1]))NULL else x) %>% 
    plyr::compact() %>% 
    lapply(CMA_kable_output)
  return(dbList)
}

#### Parse threats ----

CMA_parse_threats <- function(data){
  # data <- db.sp
  cols <- c("Pérdida de hábitat", 
            "Degradación de hábitat", 
            "Fragmentación de poblaciones", 
            "Contaminación", 
            "Impacto de especies exóticas", 
            "Depredación por perros", 
            "Urbanizaciones / infraestructura energética", 
            "Impactos asociados al turismo", 
            "Caza directa ilegal", 
            "Caza directa legal",
            "Captura de ejemplares", 
            "Reducción de presas",
            "Atropellamiento en rutas", 
            "Otros impactos asociados al transporte",
            "Incendios", 
            "Inundaciones", 
            "Enfermedades",
            "Otros impactos indirectos asociados a la especie humana"
  )
  
  data <- data %>% select(contains("amenaza")) %>% select(!sp_amenazas_comentarios) %>% 
    mutate(across(everything(.), ~as.character(.x))) %>% 
    pivot_longer(
      cols = everything(.)
    ) %>% 
    mutate(namesNew = cols) %>% 
    filter(!is.na(value)) %>% 
    mutate(value=as.numeric(value), bl='') %>% arrange(value) %>% 
    select(namesNew, bl, value)
  
  if((nrow(data) %% 1) == 0){
    l <- (nrow(data)/2) + 0.5
    x = data$namesNew[ 1:l]
    y = data$value[ 1:l] 
    x1 <- c(data$namesNew[ (l+1):nrow(data)], '')
    y1 = c(data$value[ (l+1):nrow(data)], '')
  } else {
    l <- (nrow(data)/2)
    x = data$namesNew[ 1:l]
    y = data$value[ 1:l]
    x1 <- data$namesNew[ (l+1):nrow(data)]
    y1 = data$value[ (l+1):nrow(data)]
  }
  tibble(x, y, x1, y1) %>% 
    mutate(across(x:y1, ~as.character(.x))) %>%
    CMA_kable_output(cat = 'threats')
}

#### Parse eval info ----
CMA_parse_eval <- function(data){
  # data <- db.sp
  require(tidyverse)
  db.eval <- data %>% 
    select(sp_tendencia_poblacional:sp_fluct_extrem_en_indiv_maduros) %>% 
    rbind(., c('Tendencia poblacional actual', 
               '', '', '', '',
               'Estudios de viabilidad poblacional', 
               'Tiempo generacional', '', 'Tiempo generacional, justificación', #9 
               'Reducción del tamaño poblacional en los últimos 10 años o 3 generaciones', 
               'Aumento del tamaño poblacional en los últimos 10 años o 3 generaciones', 
               'Variabilidad genética', #col 12 
               'Tamaño poblacional efectivo', '', '', 
               'Extensión de presencia (EOO)', '',
               'Extensión de presencia: comentarios', 
               'Área de ocupación (AOO)', '', 
               'Número de localidades',#21 
               'Área poblacional severamente fragmentada', '', 
               'Extensión de presencia (EOO)', 
               'Área de ocupación (AOO)', 
               'Calidad de hábitat', 
               'Número de localidades o subpoblaciones', 
               'Número de individuos maduros', 
               'Extensión de presencia (EOO)', 
               'Área de ocupación (AOO)', 
               'Número de localidades o subpoblaciones', 
               'Número de individuos maduros')) 
  
  db.eval1 <- db.eval %>% select(1:23) %>% 
    select(where(~!all(is.na(.x[1]))))
  c <- 1  
  while(c <= ncol(db.eval1)){
    if(db.eval1[[c]][2] != '') cat(paste0('**', db.eval1[[c]][2], ': **'))
    
    if(db.eval1[[c]][2] != '' && 
       any(str_detect(colnames(db.eval1)[c], 'coment'), 
           db.eval1[[c]][2] == 'Estudios de viabilidad poblacional')) 
      cat(paste0('\n\n', db.eval1[[c]][1] %>% CMA_italize_binomial(., data$title), '\n\n'))
    else 
      cat(paste0(db.eval1[[c]][1], '\n\n'))
    
    c <- c+1
  }
  
  db.eval1 <- db.eval %>% select(!1:23) %>% 
    select(where(~!all(is.na(.x[1]))))
  if(ncol(db.eval1) > 0){
    db.dismi <- db.eval1 %>% 
      select(contains('dismin'))
    
    if(ncol(db.dismi) > 0){
      db.dismi <- db.dismi %>% as.list()
      cat('**Disminución continua observada, estimada, inferida o proyectada de:**\n\n')
      
      for(x in db.dismi) cat(paste0('- **', x[2], ':** ', x[1], '\n\n'))
    }
    
    db.dismi <- db.eval1 %>% 
      select(!contains('dismin'))
    if(ncol(db.dismi) > 0){
      db.dismi <- db.dismi %>% as.list()
      cat('**Fluctuaciones extremas en:**\n\n')
      
      for(x in db.dismi) cat(paste0('- **', x[2], ':** ', x[1], '\n\n'))
    }
  }
}

#### Parse eto-eco ----
CMA_parse_eto_eco <- function(data){
  # data <- db.sp
  
  dbeto <- data %>% 
    select(sp_habitat_selvas_o_bosques:sp_habitat_canales_artificiales) %>%
    rbind(., c("Selvas / Bosques", 
               "Arbustales", 
               "Pastizales", 
               "Hábitat rupestres", 
               "Estepas", 
               "Lagos o lagunas", 
               "Rios o arroyos", 
               "Hábitat palustre", 
               "Hábitat costeros", 
               "Oceánicos", 
               "Cultivos agrícolas", 
               "Forestaciones", 
               "Pasturas ganaderas", 
               "Embalses o diques", 
               "Urbano o periurbano",
               "Canales artificiales"))
  grp <- list(Terrestres = c(1:5), 
              "De agua dulce"= c(6:8),  
              "Marinos" = c(9:10),  
              "Antrópicos" = c(11:16))
  
  
  for(i in seq_along(grp)){
    trr <- dbeto %>% 
      select(grp[[i]]) %>%  
      select(where(~!all(is.na(.x[1]))))
    
    if(ncol(trr) > 0){
      if(i == 1) cat('**Tipos de hábitat en donde la especie está presente**\n\n')
      trr <- trr %>% as.list()
      cat(paste0('**', names(grp)[i], '**\n\n'))
      
      for(x in trr) cat(paste0('- **', x[2], ':** ', x[1], '\n\n'))
    }
  }
}


#### General functionalities ----
##### Make tables ----
CMA_kable_output <- function(table, cat='taxo'){
  if(cat == 'taxo'){
    table %>%
      kbl(booktabs = T, format = 'latex', escape = F, col.names = NULL) %>% 
      kable_styling(latex_options = c('striped', "HOLD_position"),
                    position = "center", full_width = T) %>%
      column_spec(1, width = '6cm', bold=TRUE) %>% 
      column_spec(2, width = '1cm')
  } else if(cat=='other'){
    table %>%
      kbl(booktabs = T, format = 'latex',linesep = "", escape = F, col.names = NULL) %>% 
      kable_styling(latex_options = c('striped', "HOLD_position"),
                    position = "center", full_width = T) %>%
      column_spec(1, width = '8cm', bold=TRUE) %>% 
      column_spec(2, width = '0.5cm')
  } else if(cat=='threats'){
    table %>%
      kbl(booktabs = T, format = 'latex',linesep = "", escape = F, col.names = NULL) %>% 
      kable_styling(latex_options = c('striped', "HOLD_position"),
                    position = "center", full_width = T) %>%
      column_spec(1,  bold=TRUE) %>% 
      column_spec(3,  bold=TRUE)
  }
}

##### Italized species or genus text ----
CMA_italize_binomial <- function(text, species){
  # text <- db.sp$sp_taxonomia_comentarios
  # species <- db.sp$title
  
  genus <- str_split(species, ' ', simplify = T)[1]
  
  epite <- paste0(str_remove_all(genus, '[a-z]'), '. ',
                  str_split(species, ' ', simplify = T)[2])
  
  if(str_detect(text, genus)){
    text <- str_replace_all(text, genus, paste0('\\\\textit{', genus, '}'))
  } 
  if(str_detect(text, epite)){
    text <- str_replace_all(text, epite, paste0('\\\\textit{', epite, '}'))
  }
  text
}

CMA_get_photo_credits <- function(x){
  x %>% 
    str_remove('photos//.*./') %>% 
    str_remove(str_replace(db.sp$title, " ", "-")) %>% 
    str_remove('.jpg$') %>% 
    str_remove_all('[0-9]') %>% 
    str_replace_all('_', '') %>% 
    str_replace_all('-', ' ')
}

CMA_get_photo_index <- function(x){
  list.files('photos/', '.jpg$', full.names = F, recursive = T) %>% 
    str_to_lower() %>% str_detect(gsub(' ', '-', str_to_lower(x))) %>% 
    which()
}

CMA_print_photo <- function(x, credits){
  # x <- photo[filePhoto[1]]; credits=credistPhoto[1]
  paste0("\\begin{figure}[H]", 
         "\\centering", 
         paste0("\\includegraphics[width=0.850\\textwidth]{", x, "}"), 
         paste0( "\\caption{Foto: ", credits, "}"), 
         "\\end{figure}", sep='\n')
}