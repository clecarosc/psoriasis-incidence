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

epi.directadj(obs, tar, std, units = 100000, conf.level = 0.95)

