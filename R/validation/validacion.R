library(tidyverse)
library(stringr)

validation <- read_delim("psoriasis_primera_consulta.xls", ";")
glimpse(validation)
validation <- validation %>% select(`Causal de Egreso`,`Tipo Profesional solicitante`,
                                    `Centro Origen`, `Sospecha Diagóstica`, `Primera Atención`,
                                    `Fecha Primera Atención`, `Fecha Alta`,
                                    `Atención Alta`)

validation$`Fecha Primera Atención` <- str_replace(validation$`Fecha Primera Atención`, "^[0-9]{2}/[0-9]{2}/", "")

#datos disponibles para analisis
sum(!is.na(validation$`Fecha Primera Atención`)) # 377 datos tienen fecha
sum(is.na(validation$`Fecha Primera Atención`)) # 681/1058 no tienen
unique(validation$`Fecha Primera Atención`)

table(validation$`Fecha Primera Atención`)


### causas de egreso: 7 posibles respuestas
validation$`Causal de Egreso` <- factor(validation$`Causal de Egreso`)

egreso <- validation %>% group_by(`Causal de Egreso`) %>% summarise( N = n()) %>% arrange(N)
sum(egreso$N)

ggplot(egreso, aes(x= `Causal de Egreso`, y = n)) +
  geom_bar() +
  coord_flip()

### tipo de profesional solicitante: medico, otro, matrona
ggplot(validation, aes(x= factor(`Tipo Profesional solicitante`))) +
  geom_bar() +
  coord_flip()

#se filtra solo las ic hechas en el 2017 (184) o hechas por medicos (80 ic, todas el 2017)
todas <- validation %>% filter(`Fecha Primera Atención` == 2017) %>% select(`Sospecha Diagóstica`, `Primera Atención`, `Atención Alta`, `Causal de Egreso`)
medicos <- validation %>%
  filter(`Tipo Profesional solicitante` == "Médico" & `Fecha Primera Atención` == 2017) %>% 
  select(`Sospecha Diagóstica`, `Primera Atención`, `Atención Alta`)

# la mayoria tienen causa antecion realizada, algunas sin comentario
table(todas$`Causal de Egreso`)

#se revisan manualmente 3 cols: sospecha diagnostica, primera atencion y atencion alta
todas
medicos

#se registra cuales casos no corresponde
falsos_positivos <- todas[c(14, #liquen
                            17, #DA
                            18, #DA
                            20, #parapsoriasis
                            #21:32, #control
                            29, #tinea
                            36, #rosacea
                            37, #DS
                            40, #acne
                            43, #DS
                            44, #estudio
                            45, #mtx
                            46, #mtx
                            47, #mtx
                            50, #DA
                            52, #otro (hola)
                            55, #refract
                            57, #lupus
                            58, #refract
                            60, #DAC
                            64, #CEC
                            66, #refract
                            67, #dermatofitosis
                            68, #estudio
                            71, #estudio
                            72, #dermatitis pañal
                            74, #bowen
                            78, #dermatofitosis
                            79, #keratosis act
                            80, #control
                            81, #DCA
                            82, #refract
                            92, #estudio
                            94, #otro (lan)
                            95, #otro (lan)
                            96, #otro (lan)
                            100, #control
                            101, #dermatofitosis
                            102, #dermatofitosis
                            108, #no
                            109, #liquen
                            110, #otro (lesion)
                            115, #refract
                            120, #refract
                            141, #liquen
                            137, #refract
                            138, #parapsor
                            139, #parapsor
                            142, #otro (genetica)
                            184),] #control

vpp <- (nrow(todas)- nrow(falsos_positivos))/nrow(todas); vpp
