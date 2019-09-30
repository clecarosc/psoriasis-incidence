source("01_cases_py.R")
library(epiR)

## servicio_df_ano(). It gives a df with incidence rates estimates + ic by service and year

servicio_df_ano <- function(año) {
  
  psor <- filter(df_casos_py, año == año)
  servicios_presentes <- unique(psor$servicio)
  
  df_total_servicios <- vector("list", length(servicios_presentes))
  i <- 1
  
  for(servicio_i in servicios_presentes){
    
    casos_servicio <- filter(psor, servicio == servicio_i)
    por_sexo <- casos_servicio %>% group_by(sexo, edad) %>%  summarise(py = sum(py), n_psor = sum(n_psor))
    total <- por_sexo %>% group_by(edad) %>% summarise(py = sum(py), n_psor = sum(n_psor)) %>% add_column(sexo = "T", .before = "edad")
    
    lista <- list(por_sexo, total)
    sexo_total <- bind_rows(lista)
    
    obs <- matrix(sexo_total$n_psor, nrow = 3, byrow = TRUE,
                  dimnames = list(c("F", "M", "T"), bandas_edad))
    
    tar <- matrix(sexo_total$py, nrow = 3, byrow = TRUE,
                  dimnames = list(c("F", "M", "T"), bandas_edad))
    
    ### FONASA 2017
    std <- matrix(data = c(868313, 1856718, 2014749, 1984841, 1757590, 1860911, 1569723, 1049899, 773764), nrow = 1, byrow = TRUE,
                  dimnames = list("", bandas_edad))
    
    ajuste <- epi.directadj(obs, tar, std, units = 100000, conf.level = 0.95)
    
    df <- ajuste$adj.strata %>%  
      add_column(servicio = servicio_i, .before = "strata") %>%
      add_column(año = año)
    
    df_total_servicios[[i]] <- df
    
    i <- i+1}
  
  df_total_servicios <- bind_rows(df_total_servicios) 
  return(df_total_servicios)
}

servicio_df_ano(2013)
