library(tidyverse)
library(forcats)

pop_2012_2017 <- read_csv("data/pop_fonasa12_17.csv", col_types = "ccddc")

casos_total <- read_csv("data/psoriasis_summary.csv")
casos_2017 <- casos_total %>% filter(F_ENTRADA %in% c(2017))

# aqui se observa la cantidad de casos durante el periodo de observacion que aumenta

casos_periodo <- casos_total %>% filter(F_ENTRADA < 2018) %>%  group_by(F_ENTRADA) %>% summarise(cases = sum(cases))
ggplot(casos_periodo, aes(x = as.integer(F_ENTRADA), y = cases)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(2008, 2018)) +
  labs(x = "Years of observation", y = "Number of psoriasis referrals") +
  theme_classic()




