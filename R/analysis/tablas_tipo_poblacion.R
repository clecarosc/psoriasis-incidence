library(kableExtra)

c1 <- pivot_longer(pop_rnle_wide, cols = c(2:4), names_to = "sexo", values_to = "n")
c2 <- pivot_longer(fonasa_wide, cols = c(2:4), names_to = "sexo", values_to = "n")
c3 <- pivot_longer(pop_ine_wide, cols = c(2:4), names_to = "sexo", values_to = "n")

pobl <- bind_rows(c1,c2,c3)
pobl <- pobl %>% pivot_wider(names_from = c("poblacion", "year"), values_from = "n") %>% 
  select(sexo, RNLE_2016, FONASA_2016, INE_2016, 
         RNLE_2017, FONASA_2017, INE_2017)


kable(pobl, booktabs = T, linesep = "", longtable = T,
      caption = "Poblaciones fuente (INE), base de datos (FONASA) y en estudio (RNLE) durante el periodo de observacion") %>% 
  kable_styling() %>% 
  add_header_above(c("", "2016" = 3, "2017" = 3), bold = T)



