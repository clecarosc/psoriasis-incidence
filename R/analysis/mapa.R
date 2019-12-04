library(tmap)
library(sf)
library(tidyverse)

## Cargar datos de tasas 2017
rates <- read_csv("rate17.csv")
rates <- rates %>% rename(NOM_SS = servicio)


## Cargar datos de mapas de salud
### Territorios de los Servicios de Salud
ss <- read_sf("mapas/terr_ss/ss.shp")
ss <- st_simplify(ss, dTolerance = 0.02)
ss <- ss %>% select(NOM_SS)

### Arreglar nombres
ss_nombres <- c("Arica y Parinacota" = "ARICA",
                "Iquique y Tarapaca" = "IQUIQUE",
                "Antofagasta" = "ANTOFAGASTA",
                "Atacama" = "ATACAMA",
                "Coquimbo" = "COQUIMBO",
                "Valparaiso y San Antonio" = "VALPARAISO SAN ANTONIO",
                "Viña del Mar y Quillota" = "VIÑA DEL MAR QUILLOTA",
                "Aconcagua" = "ACONCAGUA",
                "Metropolitano Norte" = "METROPOLITANO NORTE",
                "Metropolitano Occidente" = "METROPOLITANO OCCIDENTE",
                "Metropolitano Central" = "METROPOLITANO CENTRAL",
                "Metropolitano Oriente" = "METROPOLITANO ORIENTE",
                "Metropolitano Sur" = "METROPOLITANO SUR",
                "Metropolitano Sur-Oriente" = "METROPOLITANO SURORIENTE",
                "Libertador B. O'Higgins" = "LIBERTADOR B. OHIGGINS",
                "Maule" = "DEL MAULE",
                "Ñuble" = "ÑUBLE",
                "Concepcion" = "CONCEPCION",
                "Talcahuano" = "TALCAHUANO",
                "Bio-Bio y Los Angeles" = "BIOBIO",
                "Arauco" = "ARAUCO",
                "Araucanía Norte" = "ARAUCANIA NORTE",
                "Araucanía Sur" = "ARAUCANIA SUR",
                "Valdivia" = "VALDIVIA",
                "Osorno" = "OSORNO",
                "Reloncavi" = "DEL RELONCAVI",
                "Chiloé" = "CHILOE",
                "Aysen" = "AYSEN",
                "Magallanes" = "MAGALLANES")

rates$NOM_SS <- ss_nombres[rates$NOM_SS]

### Left join

ss_rates <- left_join(ss, rates, by = "NOM_SS")
class(ss_rates)

# Mapas
### Establecimientos Atención Primaria y Secundaria
estab <- read_sf("mapas/ESTAB_1A_2A/ESTAB_1A_2A_20190926.shp")
estab <- estab %>% select(NOMBRE)

### Hospitales del Servicio de Salud
hosp <- read_sf("mapas/HOSPITALES_SNSS/HOSPITALES_SNSS_20190926.shp")

### Clínicas privadas
estab_noss <- read_sf("mapas/ESTAB_NOSNSS_3A/ESTAB_NOSNSS_3A_20190926.shp")





## Graficar Mapa Nacional
ss_rates



t1 <- tm_shape(ss_rates) +
  tm_polygons("est", style = "quantile", n= 5,
              title = expression("Tasas (por 10"^5*" hbtes.)")) +
  tm_style("col_blind") +
  tm_layout(legend.outside = T,
            legend.title.size = 1.5,
            legend.text.size = 1.3,
            frame = F)
t1

library(patchwork)



t1 <- ggplot(ss_rates) +
  geom_sf(aes(fill = est)) +
  scale_fill_gradient(low = "lightblue", high = "red",
                      breaks= c(0, 30, 60, 90, 120, Inf),
                      limits=c(0, 170), guide = "legend") +
  labs(title = "Mediana de Est por Región") +
  theme_minimal()


t2 <- ggplot(ss_rates, aes(x = est)) +
  stat_bin(#binwidth = 15, bins = 5,
           fill = "white", col = "black",
           breaks = t3$breaks) +
  theme_minimal(); t2


class(ss_rates)

summary(ss_rates$est)
quant <- quantile(ss_rates$est, na.rm = T)
         #probs = seq(0,1, 0.2))
t2 + geom_vline(xintercept = quant[2:5], 
                col = "red",
                linetype = "dashed")

#tmap_mode("view")
#ttm()

tm_shape(ss) +
  tm_borders() +
tm_shape(estab) +
  tm_dots(col = "red") +
tm_shape(estab_noss) +
  tm_dots(col = "blue")

## Graficar Mapa Region Metropolitana

library(stringr)
ss <- read_sf("mapas/terr_ss/ss.shp")
estab <- read_sf("mapas/ESTAB_1A_2A/ESTAB_1A_2A_20190926.shp")
aps_rm <- estab %>% filter(str_detect(REGION_, "Metro"))
clinicas_rm <- estab_noss %>% filter(str_detect(REGION_, "Metro")) %>% select(NOMBRE)
ss_rm <- ss %>% filter(str_detect(NOM_SS, "METRO"))

tm_shape(ss_rm) +
  tm_borders() +
tm_shape(aps_rm) +
  tm_dots(col = "red") +
tm_shape(clinicas_rm) +
  tm_dots(col = "blue")


## Raster de radiacion solar
library(raster)
rad_sol <- raster("ghi-m00-ll.tif")
class(rad_sol)
nlayers(rad_sol)

plot(rad_sol)

ncell(rad_sol)  


