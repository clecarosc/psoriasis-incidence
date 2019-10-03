wd <- getwd()
setwd(paste(wd,"/R", sep = ""))
source("01_cases_py.R")

df_nac_edad_sexo <- df_casos_py %>% group_by(F_ENTRADA, edad, sexo) %>% summarise(py = sum(py), 
                                                              cases = sum(cases))
df_nac_edad_sexo_17 <- df_nac_edad_sexo %>% filter(F_ENTRADA == 2017) %>% ungroup %>% select(- F_ENTRADA)

total <- df_nac_edad_sexo_17 %>% group_by(edad) %>% summarise(py = sum(py), cases = sum(cases)) %>% add_column(sexo = "T", .after = "edad")

lista_nacional <- list(df_nac_edad_sexo_17, total)
nacional_sexo_total <- bind_rows(lista_nacional)
  
nacional_sexo_total <- nacional_sexo_total %>% arrange(edad, sexo)

obs <- matrix(nacional_sexo_total$cases, nrow = 3, byrow = F,
                dimnames = list(c("F", "M", "T"), bandas_edad))

tar <- matrix(nacional_sexo_total$py, nrow = 3, byrow = F,
                dimnames = list(c("F", "M", "T"), bandas_edad))
std <- matrix(data = c(868313, 1856718, 2014749, 1984841, 1757590, 1860911, 1569723, 1049899, 773764), nrow = 1, byrow = TRUE,
                dimnames = list("", bandas_edad))

  
ajuste <- epi.directadj(obs, tar, std, units = 100000, conf.level = 0.95)
  

####
ggplot(ajuste$crude, aes( x = factor(cov, levels = bandas_edad),
                            y = est)) +
   geom_line(data = filter(ajuste$crude, strata != "T"),
             aes(group = strata, col = strata, linetype = strata)) +
   
   geom_ribbon(data = filter(ajuste$crude, strata != "T"),
               alpha = .3,
               aes(ymin = lower,
                   ymax = upper, 
                   group = strata, fill = strata)) +
   
   geom_point(data = filter(ajuste$crude, strata == "T"),
              col = "black",
              size = 1) +
   
   geom_errorbar(data = filter(ajuste$crude, strata == "T"),
                 width = .2,
                 col = "black",
                 aes(ymin = lower,
                     ymax = upper)) +

theme_classic(base_line_size = .7, base_size = 15) +
labs(x = "Age bands", y = "Crude incidence rates of psoriasis",
     fill = "Gender", color = "Gender", linetype = "Gender") +
theme( legend.position ="bottom")
