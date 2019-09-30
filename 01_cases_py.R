library(tidyverse)
library(forcats)

pop_2012_2017 <- read_csv("pop_fonasa12_17.csv", col_types = "ccddc")

casos_total <- read_csv("psoriasis_summary.csv")
casos_2016_17 <- casos_total %>% filter(F_ENTRADA %in% c(2016, 2017))

b <- c(-Inf, 5, 15, 25, 35, 45, 55, 65, 75, Inf)
names <- c("0-5","5-15", "10-25", "25-35", "35-45", "45-55", "55-65", "65-75", ">75")
casos_2016_17$edad <- cut(casos_2016_17$age, breaks = b, labels = names)

casos_2016_17 <- casos_2016_17 %>% rename(servicio = SS, sexo = SEXO)
sexo_ordenado <- c("f" = "F", "m" = "M" )
casos_2016_17$sexo <- sexo_ordenado[casos_2016_17$sexo]

servicios_ordenados <- c("Ar" = "Arica y Parinacota",
                         "Iq" = "Iquique y Tarapaca",
                         "An"= "Antofagasta",
                         "At" = "Atacama",
                         "Co" = "Coquimbo",
                         "Valpo" = "Valparaiso y San Antonio",
                         "VQ" = "Viña del Mar y Quillota",
                         "Ac" = "Aconcagua",
                         "MN" = "Metropolitano Norte",
                         "MOcc" = "Metropolitano Occidente",
                         "MC" = "Metropolitano Central",
                         "MOri" = "Metropolitano Oriente",
                         "MS" = "Metropolitano Sur",
                         "MSO" = "Metropolitano Sur-Oriente",
                         "LBO" = "Libertador B. O'Higgins",
                         "Mau" = "Maule",
                         "Nu" = "Ñuble",
                         "Con" = "Concepcion",
                         "Tal" = "Talcahuano",
                         "BB" = "Bio-Bio y Los Angeles",
                         "Ara" = "Arauco",
                         "ArN" = "Araucanía Norte",
                         "ArS" = "Araucanía Sur",
                         "Valdi" = "Valdivia",
                         "O" = "Osorno",
                         "Rel" = "Reloncavi",
                         "Chi" = "Chiloé",
                         "Ay" = "Aysen",
                         "Ma" = "Magallanes")

casos_2016_17$servicio <- servicios_ordenados[casos_2016_17$servicio]
casos_2016_17
## funcion casos y py. entrega casos y personas año de cada servicio segun año. entrega una lista con un df y un vector chr
funcion_casos <- function(year) {
  
  year_before <- year-1
  casos <- casos_2016_17 %>% filter(F_ENTRADA == year)
  
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
    mutate(cases = replace(cases,
                            which(is.na(cases)), 0),
           F_ENTRADA = replace(F_ENTRADA,
                         which(is.na(F_ENTRADA)), year))  %>%
    select(F_ENTRADA, servicio, sexo, edad, py, cases)
  productos <- vector("list", 2)
  productos[[1]] <- join
  productos[[2]] <- regiones_presentes
  return(productos)
}

### esta es una lista que contiene los casos y persona-año para cada año, junto con los servicios de salud presentes cada año.
### ademas un dataframe con la informacion durante todo el periodo.
anos <- 2016:2017
lista_casos_py <- vector("list", length(anos))
i <- 1

for (ano in anos) {
  casos_py <- funcion_casos(ano)
  lista_casos_py[[i]] <- casos_py
  names(lista_casos_py[[i]]) <- as.character(ano)
  i <- i+1
}

df_casos_py <- bind_rows(lista_casos_py[[1]][[1]], lista_casos_py[[2]][[1]])

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

