#Nacional
nac1 <- df_casos_py %>% group_by(F_ENTRADA, edad, sexo) %>% summarise(cases = sum(cases), 
                                                                  py = sum(py)) %>% rename(year = F_ENTRADA) %>% ungroup()
nac1
nac2 <- nac1 %>% group_by(year, edad) %>% summarise_if(is.numeric, sum) %>% add_column(sexo = "T", .after = "edad") %>% ungroup()

casos_nacional <- bind_rows(list(nac1, nac2)) %>% arrange(year, edad, sexo)
write_csv(casos_nacional, "casos_nacional.csv")

#Regional


