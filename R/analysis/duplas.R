library(tidyverse)


servicios <- list.files()
servicios <- sub("WL-SS", "", servicios)
servicios <- sub(".csv$", "", servicios)
servicios


ac <- read_csv("WL-SSAc.csv")
ac <- ac %>% select(FECHA_NAC, SEXO, F_ENTRADA, SOSPECHA_DIAG) %>% add_column( SS = "Ac")
ac
ac %>% group_by(FECHA_NAC, SEXO, F_ENTRADA)
ac %>% group_by(FECHA_NAC, SEXO, F_ENTRADA)
