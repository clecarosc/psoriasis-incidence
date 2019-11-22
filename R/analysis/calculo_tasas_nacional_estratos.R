library(epiR)
library(tidyverse)
casos_nacional <- readRDS("~/Desktop/tesis/08 tesis_r/data/casos_nacional.rds")

casos_nacional <- casos_nacional %>% filter(sexo %in% c("F", "M"))

casos_wide <- pivot_wider(casos_nacional, names_from = c("sexo", "edad"), values_from = c("cases", py))

obs <- casos_wide %>% select(starts_with("cases")) %>% ungroup()
obs <- as.matrix(obs)
rownames(obs) <- c("2016", "2017")

tar <- casos_wide %>% select(starts_with("py")) %>% ungroup()
tar <- as.matrix(tar)
rownames(tar) <- c("2016", "2017")

std <- tar[1,]
esp <- c(2500, 2500, 5500, 5500, 5750, 5750, 6250, 6250, 7000, 7000, 7000, 7000, 6250, 6250, 5250, 5250, 4500, 4500)
esp <- matrix(esp, nrow = 1, dimnames = list("", colnames(obs)))

epi.directadj(obs, tar, std, units = 100000, conf.level = 0.95)
tasas_nac_fonasa17 <- epi.directadj(obs, tar, std, units = 100000, conf.level = 0.95)

epi.directadj(obs, tar, esp, units = 100000, conf.level = 0.95)
tasas_nac_esp <- epi.directadj(obs, tar, esp, units = 100000, conf.level = 0.95)
