#Nacional
nac1 <- df_casos_py %>% group_by(F_ENTRADA, edad, sexo) %>% summarise(cases = sum(cases), 
                                                                  py = sum(py)) %>% rename(year = F_ENTRADA) %>% ungroup()
nac1
nac2 <- nac1 %>% group_by(year, edad) %>% summarise_if(is.numeric, sum) %>% add_column(sexo = "T", .after = "edad") %>% ungroup()

casos_nacional <- bind_rows(list(nac1, nac2)) %>% arrange(year, edad, sexo)
write_csv(casos_nacional, "casos_nacional.csv")
saveRDS(casos_nacional, "casos_nacional.rds")

casos_nacional

#Regional

regional1 <- df_casos_py %>% group_by(F_ENTRADA, region, edad, sexo) %>% summarise(cases = sum(cases), 
                                                                      py = sum(py)) %>% rename(year = F_ENTRADA) %>% ungroup()

regional2 <- regional1 %>% group_by(year, region, edad) %>% summarise_if(is.numeric, sum) %>% add_column(sexo = "T", .after = "edad") %>% ungroup()

casos_regional <- bind_rows(list(regional1, regional2)) %>% arrange(year, region, edad, sexo)
write_csv(casos_regional, "casos_regional.csv")
saveRDS(casos_regional, "casos_regional.rds")

#Servicios
df_casos_py
serv1 <- df_casos_py %>% group_by(F_ENTRADA, servicio, sexo, edad) %>% summarise(cases = sum(cases), 
                                                                                   py = sum(py)) %>% rename(year = F_ENTRADA) %>% ungroup()
serv2 <- serv1 %>% group_by(year, servicio, edad) %>% summarise_if(is.numeric, sum) %>% add_column(sexo = "T", .after = "edad") %>% ungroup()

casos_servicio <- bind_rows(list(serv1, serv2)) %>% arrange(year, servicio, edad, sexo)
write_csv(casos_servicio, "casos_servicio.csv")
saveRDS(casos_servicio, "casos_servicio.rds")






