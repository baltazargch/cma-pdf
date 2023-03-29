library(tidyverse)

db <- read_csv('data/especies_nativas.csv')
db.list <- split(db, db$title)

db.na.sum <-lapply(db.list, \(x) sum( !apply(x, 1, is.na) ))

most.complete <- db.na.sum %>% 
  do.call(rbind, .) %>% 
  as.data.frame() %>% 
  rownames_to_column(var='sp')

colnames(most.complete) <- c('sp', 'n')

head(most.complete %>% arrange(-n)) %>% as_tibble()
colnames(db)
col.orders <- c(
  #id
  1, 
  #binomial titulo
  5, 
  #comun titulo 
  7, 
  #cita sugerida
  147,
  #cat CMA 2019, y criterios 
  13, 14, 15, 
  #subpoblaciones
  16, 
  #cat cons viejas
  17, 21,24, 20,23, 18,19,22,
  #cat paises vecinos
  25,
  #cat global
  28,26,27,
  #TODO faltan evaluaciones previas
  #### Tax y nomen ----
  4,3,6:11,
  ### Info rel eva conserva ----
  29, 30, 
  34,
  35,36,37,
  38, 41, 42,
  # EOO
  44,45,46,
  49,50,52,53,54,56,60, 
  #### Rango geo, occ, abund ----
  61, 62, 
  63,65, 68, 71, 
  73, 74, 72, 
  76, 77, 
  #TODO falta si exist act progr monitor
  78,
  #### Datos morfo ----
  79,80,81,82,
  #### Rasgos eto-eco ----
  84,85,83, 106, 
  86:102, 
  103:105, 107:109, 
  111,112,110, 
  #### Conservacion e invest ----
  113:119, 130, 
  121:123, 
  125, 127,128,129,131,
  132,133,134,135,
  138,139,
  140,141,
  142,137,
  #### Biblio ----
  143,144,
  #### Autores ----
  145, 146
)12
f <- !1:ncol(db)  %in%  col.orders
v <- 1:ncol(db)db %>% 
  filter(title =='Blastocerus dichotomus') %>% 
  select(all_of(col.orders))
v[f]
sp_db <- 
