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
##los que se piensa otro dg:10,118,28,29,34,37,38,41,46,52,54,73.
##de los 75 comentarios de primera atencion, 1(22 es tto acne-rosacea), 1 estudio (26), 1 dice hola(31), 1 dice ic derm(32), 1 celulitis,
## 1 es artr psor humira ley ricarte soto[35], 1 es psor+celulitis [36],1 es dermatitis de contacto manos [37], 1 es un bcc [38],
## 1 es celulitis+psor [39], 1 es psor-dermatitis de panañal [44], bowen vs psor [46], dermatofitosis [49], keratosis actinica [50],
## 1 control  [51], dishidrosis palmar [52], estudio [62], no registra dato x 3 [64-66], "s" [70], liquen plano pilaris [72]