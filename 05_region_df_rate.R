source("01_cases_py.R")
library(epiR)

## region_df_ano(). It gives a df with incidence rates estimates + ic by region and year

region_df_ano <- function(año) {
  
  psor <- filter(df_casos_py, año == año)
  regiones_presentes <- unique(psor$region)
  
  df_total_regiones <- vector("list", length(regiones_presentes))
  i <- 1
  
  for(region_i in regiones_presentes){
    
    casos_region <- filter(psor, region == region_i)
    por_sexo <- casos_region %>% group_by(sexo, edad) %>%  summarise(py = sum(py), n_psor = sum(n_psor))
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
      add_column(region = region_i, .before = "strata") %>%
      add_column(año = año)
    
    df_total_regiones[[i]] <- df
    
    i <- i+1}
  
  df_total_regiones <- bind_rows(df_total_regiones) 
  return(df_total_regiones)
}


df2013 <- region_df_ano(2013)
df2014 <- region_df_ano(2014) 
df2015 <- region_df_ano(2015)
df2016 <- region_df_ano(2016) 
df2017 <- region_df_ano(2017)

df_13_17 <- bind_rows(df2013, df2014, df2015, df2016, df2017)
df_13_17$region <- factor(df_13_17$region, levels = unique(regiones))

