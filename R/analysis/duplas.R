library(tidyverse)

servicios <- list.files()
#servicios <- servicios

dat <- tibble()
for (servicio in servicios) {
  nombre <- sub("WL-SS", "", servicio)
  nombre <- sub(".csv$", "", nombre)
  temp <- read_csv(servicio)
  temp <- temp %>% select(FECHA_NAC, SEXO, F_ENTRADA, SOSPECHA_DIAG) %>% add_column( SS = nombre)
  obs_total <- nrow(temp)
  temp <- temp %>% filter(str_detect(F_ENTRADA, "201(6|7)$"))
  obs_1617 <- nrow(temp)
  
  temp <- temp %>% group_by(SEXO, FECHA_NAC, F_ENTRADA) %>%
    tally()
  obs_f_entrada <- nrow(temp)
  
  temp <- temp %>% tally()
  obs_f_nac <- nrow(temp)
  temp_df <- tibble(obs_total, obs_1617, obs_f_entrada, obs_f_nac, nombre)
  dat <- rbind(dat, temp_df)
}


#dummy example with one service
#ac <- read_csv("WL-SSAc.csv")
#ac <- ac %>% select(FECHA_NAC, SEXO, F_ENTRADA, SOSPECHA_DIAG) %>% add_column( SS = "Ac")
#obs_total <- nrow(ac)
#ac <- ac %>% filter(str_detect(F_ENTRADA, "201(6|7)$"))
#obs_1617 <- nrow(ac)
#
#ac
#ac <- ac %>% group_by(SEXO, FECHA_NAC, F_ENTRADA) %>%
#  tally(); ac
#obs_f_entrada <- nrow(ac)
#
#ac <- ac %>% tally(); ac
#obs_f_nac <- nrow(ac)