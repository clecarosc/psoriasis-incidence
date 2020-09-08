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

mso <- read_csv("WL-SSMSO.csv", col_types =  cols_only(FECHA_NAC = "c",
                                          SEXO ="c",
                                          F_ENTRADA ="c",
                                          SOSPECHA_DIAG = "c"))
mso_1617 <- mso %>% filter(str_detect(F_ENTRADA, "^201(6|7)"))
mso_1617$F_ENTRADA <- str_remove(mso_1617$F_ENTRADA," 00:00:00.000")
mso_1617
dat$obs_1617[17] <- nrow(mso_1617)

temp <- mso_1617 %>% group_by(SEXO, FECHA_NAC, F_ENTRADA) %>%
  tally()
dat$obs_f_entrada[17] <- nrow(temp)

temp <- temp %>% tally(); temp
dat$obs_f_nac[17] <- nrow(temp)

sum(dat$obs_total)
sum(dat$obs_1617)
sum(dat$obs_f_entrada)
sum(dat$obs_f_nac)

dat <- dat %>% filter(nombre != "MSO")
dat <- dat %>% filter(nombre != "ArS")

ggplot(dat, aes(x= factor(nombre))) +
geom_point(aes(y= obs_total, size = obs_total), color = "green") +
geom_point(aes(y = obs_1617, size = obs_total), color = "red") +
geom_point(aes(y = obs_f_entrada, size = obs_total), color ="blue") +
geom_point(aes(y = obs_f_nac, size = obs_f_nac), color ="purple") 


dat

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