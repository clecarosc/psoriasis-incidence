library(tidyverse)
library(stringr)

validation <- read_delim("psoriasis_primera_consulta.xls", ";")
validation <- validation %>% select(`Causal de Egreso`,`Tipo Profesional solicitante`,
                                    `Centro Origen`, `Sospecha Diagóstica`, `Primera Atención`,
                                    `Fecha Primera Atención`, `Fecha Alta`,
                                    `Atención Alta`)

validation$`Fecha Primera Atención` <- str_replace(validation$`Fecha Primera Atención`, "^[0-9]{2}/[0-9]{2}/", "")

#datos disponibles para analisis
sum(!is.na(validation$`Fecha Primera Atención`)) # 377 datos tienen fecha
sum(is.na(validation$`Fecha Primera Atención`)) # 681/1058 no tienen
unique(validation$`Fecha Primera Atención`)
#datos durante el periodo de observacion del estudio 13-17
validation_13_17 <- validation %>% filter(`Fecha Primera Atención` %in% c(2013,2014,2015,2016,2017))

sum(is.na(validation_13_17$`Fecha Primera Atención`)) # 0
sum(!is.na(validation_13_17$`Fecha Primera Atención`)) # 217 datos tienen fecha durante el periodo

table(validation_13_17$`Fecha Primera Atención`)

table(validation$`Fecha Primera Atención`)
table(validation$`Fecha Primera Atención`)

### causas de egreso: 7 posibles respuestas
ggplot(validation_13_17, aes(x= factor(`Causal de Egreso`))) +
  geom_bar() +
  coord_flip()

### tipo de profesional solicitante: medico, otro, matrona
ggplot(validation_13_17, aes(x= factor(`Tipo Profesional solicitante`))) +
  geom_bar() +
  coord_flip()

#se filtra solo las ic hechas por medicos (80 ic, todas el 2017)
medicos <- validation_13_17 %>% filter(`Tipo Profesional solicitante` == "Médico")

#todas el 2017
table(medicos$`Fecha Primera Atención`)

#se revisan manualmente las sospechas diagnosticas y el comentario de primera atencion y de atencion al alta
medicos$`Sospecha Diagóstica` # 80 casos, cuantos dicen control 45,80: 2/80; los que se piensa otro dg:10,118,28,29,34,37,38,41,46,52,54,73.
medicos_1aten <- medicos %>% drop_na(`Primera Atención`); medicos_1aten$`Primera Atención`
##de los 75 comentarios de primera atencion, 1(22 es tto acne-rosacea), 1 estudio (26), 1 dice hola(31), 1 dice ic derm(32), 1 celulitis,
## 1 es artr psor humira ley ricarte soto[35], 1 es psor+celulitis [36],1 es dermatitis de contacto manos [37], 1 es un bcc [38],
## 1 es celulitis+psor [39], 1 es psor-dermatitis de panañal [44], bowen vs psor [46], dermatofitosis [49], keratosis actinica [50],
## 1 control  [51], dishidrosis palmar [52], estudio [62], no registra dato x 3 [64-66], "s" [70], liquen plano pilaris [72]

medicos_alta <- medicos %>% drop_na(`Atención Alta`); medicos_alta$`Atención Alta`

# la mayoria tienen causa antecion realizada, algunas sin comentario
ggplot(medicos, aes(x= factor(`Causal de Egreso`))) +
  geom_bar() +
  coord_flip()



#hay un caso de no pertinencia solicitado por otro profesional no medico
no_pert <- validation_13_17 %>% filter(`Causal de Egreso` == "No pertinencia")
no_pert$`Sospecha Diagóstica`;no_pert$`Primera Atención`

###############
### otro enfoque, sin considerar la fecha de atencion, solo drop los que no tienen comentario ()
validation_comentario <- validation  %>%  drop_na(`Atención Alta`)
