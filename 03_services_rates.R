source("01_cases_py.R")
library(epiR)

## servicio_year(). this function gives crude and direct-adjusted rates for each health service by input year.

servicio_year <- function(year) {
psor <- filter(df_casos_py, año == year) 
servicios_presentes <- unique(psor$servicio)

output <- vector("list", length(servicios_presentes))
i <- 1

for(servicio_i in servicios_presentes){
  
  casos_servicio <- filter(psor, servicio == servicio_i)
  
  por_sexo <- casos_servicio %>% group_by(sexo, edad) %>%  summarise(py = sum(py), n_psor = sum(n_psor))
  total <- por_sexo %>% group_by(edad) %>% summarise(py = sum(py), n_psor = sum(n_psor)) %>% add_column(sexo = "T", .before = "edad")
  
  #este es un entregable: casos femenino, masculino y total segun bandas de edad por servicio segun año
  lista_temp <- list(por_sexo, total)
  df_casos_py_f_m_t <- bind_rows(lista_temp)
  
  ### incidence rates
  obs <- matrix(df_casos_py_f_m_t$n_psor, nrow = 3, byrow = TRUE,
                dimnames = list(c("F", "M", "T"), bandas_edad))
  
  tar <- matrix(df_casos_py_f_m_t$py, nrow = 3, byrow = TRUE,
                dimnames = list(c("F", "M", "T"), bandas_edad))
  
  std <- matrix(data = c(868313, 1856718, 2014749, 1984841, 1757590, 1860911, 1569723, 1049899, 773764), nrow = 1, byrow = TRUE,
                dimnames = list("", bandas_edad))
  
  ajuste <- epi.directadj(obs, tar, std, units = 100000, conf.level = 0.95)
  
  # aqui se guarda el df con los datos sin calculo (casos/py, ie df_casos_py_f_m_t) y las tasas ajustados con su ic
  productos <- vector("list", 2)
  productos[[1]] <- df_casos_py_f_m_t
  productos[[2]] <- ajuste
 
  output[[i]] <- productos #guarda una lista (df+df) dentro de la lista de cada region
  names(output[i][[1]]) <- servicio_i
  output[[i]][[2]]$adj.strata <- output[[i]][[2]]$adj.strata %>%
    add_column(servicio = servicio_i, .before = "strata") %>%
    add_column(año = year)
  
  i <- i+1}
return(output)
}


servicio_year(2013)
