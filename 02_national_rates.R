source("01_cases_py.R")
library(epiR)

### function tasas_nacionales_sexo(). function that gives national estimation for each year by sex. 
### standard pop is fonasa 2017, output is a df 3x5 (strata: F,M,T; est; lower; upper; año)

tasas_nacionales_sexo <- function(ano) {
  psor <- df_casos_py %>% filter(año == ano)
  regiones_presentes <- unique(psor$servicio)
  
  nacionales_por_sexo <- psor %>% group_by(sexo, edad) %>%  summarise(py = sum(py), n_psor = sum(n_psor))
  nacionales_total <- nacionales_por_sexo %>% group_by(edad) %>% summarise(py = sum(py), n_psor = sum(n_psor)) %>% add_column(sexo = "T", .before = "edad")
  
  lista_nacional <- list(nacionales_por_sexo, nacionales_total)
  nacional_sexo_total <- bind_rows(lista_nacional)
  
  #parte 2.
  obs <- matrix(nacional_sexo_total$n_psor, nrow = 3, byrow = TRUE,
                dimnames = list(c("F", "M", "T"), bandas_edad))
  tar <- matrix(nacional_sexo_total$py, nrow = 3, byrow = TRUE,
                dimnames = list(c("F", "M", "T"), bandas_edad))
  std <- matrix(data = c(868313, 1856718, 2014749, 1984841, 1757590, 1860911, 1569723, 1049899, 773764), nrow = 1, byrow = TRUE,
                dimnames = list("", bandas_edad))
  ajuste <- epi.directadj(obs, tar, std, units = 100000, conf.level = 0.95)
  
  #resultado <- ajuste$adj.strata 
  resultado <- ajuste$adj.strata %>% add_column(año = ano)
  return(resultado) }



### for loop, para conseguir las tasas nacionales usando la funcion tasas_nacionales_sexo()

anos <- 2013:2017
lista_tasas <- vector("list", length(anos))
i <- 1

for (ano in anos) {
  tasa <- tasas_nacionales_sexo(ano)
  lista_tasas[[i]] <- tasa
  i <- i+1
}

tasas_nacionales <- bind_rows(lista_tasas)
tasas_nacionales

rm(lista_tasas, tasa, ano, anos, i, tasas_nacionales_sexo)
