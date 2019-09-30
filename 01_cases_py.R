library(tidyverse)
library(forcats)

pop_2012_2017 <- read_csv("pop_fonasa12_17.csv", col_types = "ccddc")

casos <- read_csv("psoriasis_summary.csv")
casos_2016_17 <- casos %>% filter(F_ENTRADA %in% c(2016, 2017))

b <- c(-Inf, 5, 15, 25, 35, 45, 55, 65, 75, Inf)
names <- c("0-5","5-15", "10-25", "25-35", "35-45", "45-55", "55-65", "65-75", ">75")
casos_2016_17$edad <- cut(casos_2016_17$age, breaks = b, labels = names)



## funcion casos y py. entrega casos y personas año de cada servicio segun año. entrega una lista con un df y un vector chr
funcion_casos <- function(año) {
  year <- año
  year_before <- año-1
  casos <- casos_2013_2017 %>% filter(año == year)
  
  #regiones participantes
  regiones_presentes <- unique(casos$servicio)
  
  #crear pobl mitad año - person year mid year
  pop <- filter(pop_2012_2017, año %in% c(year_before, year) &
                  servicio %in% regiones_presentes)
  pop_mid <- pop %>% group_by(servicio, sexo, edad) %>%
    summarise(py = sum(n)/2)
  
  pop_mid$edad <- as.character(pop_mid$edad) #para poder hacer join
  
  ### join
  join <- pop_mid %>% left_join(casos) %>% #mantiene tabla izquierda
    mutate(n_psor = replace(n_psor,
                            which(is.na(n_psor)), 0),
           año = replace(año,
                         which(is.na(año)), year))  %>%
    select(año, servicio, sexo, edad, py, n_psor)
  productos <- vector("list", 2)
  productos[[1]] <- join
  productos[[2]] <- regiones_presentes
  return(productos)
}

### esta es una lista que contiene los casos y persona-año para cada año, junto con los servicios de salud presentes cada año.
### ademas un dataframe con la informacion durante todo el periodo.
anos <- 2013:2017
lista_casos_py <- vector("list", length(anos))
i <- 1

for (ano in anos) {
  casos_py <- funcion_casos(ano)
  lista_casos_py[[i]] <- casos_py
  names(lista_casos_py[[i]]) <- as.character(ano)
  i <- i+1
}

df_casos_py <- bind_rows(lista_casos_py[[1]][[1]], lista_casos_py[[2]][[1]], lista_casos_py[[3]][[1]],
                         lista_casos_py[[4]][[1]], lista_casos_py[[5]][[1]])

df_casos_py <- df_casos_py %>% ungroup()

### factor edad
bandas_edad <- c("0-5", "5-15", "15-25", "25-35", "35-45", "45-55", "55-65", "65-75", ">75")
df_casos_py$edad <- factor(df_casos_py$edad, levels = unique(bandas_edad))

### factor region
df_casos_py <- df_casos_py %>% mutate(region = fct_recode(servicio, 
                                                          "Los Lagos" = "Chiloé",
                                                          "Metropolitana" = "Metropolitano Central",
                                                          "Metropolitana" = "Metropolitano Norte",
                                                          "Bio-Bio y Los Angeles" = "Ñuble",
                                                          "Bio-Bio y Los Angeles" = "Talcahuano",
                                                          "Los Lagos" = "Osorno",
                                                          "Los Lagos" = "Reloncavi",
                                                          "Valparaiso" = "Viña del Mar y Quillota",
                                                          "Valparaiso" = "Aconcagua",
                                                          "Metropolitana" = "Metropolitano Occidente"))

regiones <- c("Arica y Parinacota",
              "Tarapaca",
              "Antofagasta",
              "Atacama",
              "Coquimbo",
              "Valparaiso",
              "Metropolitana",
              "O'Higgins",
              "Maule",
              "Bio-Bio y Los Angeles",
              "Araucania",
              "Los Rios",
              "Los Lagos",
              "Aysen",
              "Magallanes")

df_casos_py$region <- factor(df_casos_py$region, levels = unique(regiones))

### factor servicios de salud

servicios <- c("Arica y Parinacota",
                         "Iquique y Tarapaca",
                         "Antofagasta",
                         "Atacama",
                         "Coquimbo",
                         "Valparaiso y San Antonio",
                         "Viña del Mar y Quillota",
                         "Aconcagua",
                         "Metropolitano Norte",
                         "Metropolitano Occidente",
                         "Metropolitano Central",
                         "Metropolitano Oriente",
                         "Metropolitano Sur",
                         "Metropolitano Sur-Oriente",
                         "Libertador B. O'Higgins",
                         "Maule",
                         "Ñuble",
                         "Concepcion",
                         "Talcahuano",
                         "Bio-Bio y Los Angeles",
                         "Arauco",
                         "Araucanía Norte",
                         "Araucanía Sur",
                         "Valdivia",
                         "Osorno",
                         "Reloncavi",
                         "Chiloé",
                         "Aysen",
                         "Magallanes")


df_casos_py$servicio <- factor(df_casos_py$servicio, levels = unique(servicios))

df_casos_py$sexo <- factor(df_casos_py$sexo)
#
df_casos_py <- df_casos_py %>% arrange(año, region, servicio, edad)

#para guardar y cargar la tabla con los tipos
#saveRDS(df_casos_py, "data_psor_py.RDS") 

###
rm(ano, anos, i, lista_casos_py, casos_py, pop_2012_2017, casos_2013_2017, funcion_casos)

