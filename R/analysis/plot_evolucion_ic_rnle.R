casos_periodo
scale(casos_periodo$cases)

cne <- c(1810705,1857242,2015219,2205079,2593418)
aps <- c(11630877,10872166,10785778,11065377,11194847)

demandas <- tibble( year = casos_periodo$F_ENTRADA[1:5],
                    ic = casos_periodo$cases[1:5],
                    cne = cne,
                    aps = aps)

#centrado en el promedio del vector y escalado segun la desviacion estandar
d1 <- scale(demandas[,-1])
d1 <- as_tibble(d1)
d1$year <- c(2012:2016)

d1 <- pivot_longer(d1, -year, names_to = "demanda", values_to = "n")
d2 <- d1 %>% filter(!str_detect(demanda, "aps") )

ggplot(d2, aes(x = factor(year), y = n)) +
  geom_point( aes(col = demanda)) +
  geom_line( aes( col = demanda,
                  group = demanda,
                  linetype = demanda) )

#centrado en el maximo del vector y no escalado
scale(demandas[,-1], center = apply(demandas[,-1], 2, max), scale = F)

#centrado en el maximo del vector y escalado segun rmse
d1 <-scale(demandas[,-1], 
      center = apply(demandas[,-1], 2, max), 
      scale = apply(demandas[,-1], 2, sd, na.rm = TRUE))
d1 <- as_tibble(d1)
d1$year <- c(2012:2016)

d1 <- pivot_longer(d1, -year, names_to = "demanda", values_to = "n")
d2 <- d1 %>% filter(!str_detect(demanda, "aps") )

ggplot(d2, aes(x = factor(year), y = n)) +
  geom_point( aes(col = demanda)) +
  geom_line( aes( col = demanda,
                  group = demanda,
                  linetype = demanda) )
