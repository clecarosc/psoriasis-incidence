source("01_cases_py.R")

### RNLE
df_pyramid_2017 <- df_casos_py %>% filter(F_ENTRADA == 2017) %>% select(sexo, edad, py) %>% group_by(sexo, edad) %>% summarise(pop = sum(py))

df_pyramid_2017 <- df_pyramid_2017 %>% mutate(pop = ifelse(sexo=="M", pop*-1, pop))


g1 <- ggplot(data=df_pyramid_2017,aes(x=edad,y = pop, fill=sexo)) + 
  geom_bar(data=subset(df_pyramid_2017,sexo=="F"), stat = "identity") + 
  geom_bar(data=subset(df_pyramid_2017,sexo=="M"), stat = "identity") + 
  scale_y_continuous(breaks = c(-500000,-250000,0,250000,560000),
                     labels = paste0(as.character(c(500, 250, 0, 250, 560)), "M")) + 
  coord_flip() +
  scale_fill_brewer(palette = "Set1") + theme_minimal() +
  labs(title = "Study population", x = "Age bands", y = "") +
  theme(legend.position = "none")
  
g1

### FONASA
pop_2012_2017 <- read_csv("pop_fonasa12_17.csv", col_types = "ccddc")
pop_fonasa <- filter(pop_2012_2017, año %in% c(2017, 2016)) %>% group_by(sexo, edad) %>%
              summarise(pop = sum(n)/2)
pop_fonasa$edad <- factor(pop_fonasa$edad, levels = unique(bandas_edad))
pop_fonasa <- pop_fonasa %>% arrange(sexo,edad)

pop_fonasa <- pop_fonasa %>% mutate(pop = ifelse(sexo=="M", pop*-1, pop))

g2 <- ggplot(data=pop_fonasa,aes(x=edad,y = pop, fill=sexo)) + 
  geom_bar(data=subset(pop_fonasa,sexo=="F"), stat = "identity") + 
  geom_bar(data=subset(pop_fonasa,sexo=="M"), stat = "identity") + 
  scale_y_continuous(breaks = c(-1000000, -500000, 0, 500000, 1100000),
                     labels = paste0(as.character(c(1, 0.5, 0, 0.5, 1.1)), "MM")) + 
  coord_flip() +
  scale_fill_brewer(palette = "Set1") + theme_minimal() +
  labs(title = "Database population", x = "", y = "") +
  theme(legend.position = "none")
g2
### INE

pop_ine <- read_csv("pop_ine.csv")
pop_ine <- pop_ine %>%  select(SEXO, EDAD, a2017) 
b <- c(-Inf,5, 15, 25, 35, 45, 55, 65, 75, Inf)
pop_ine$edad <- cut(pop_ine$EDAD, breaks = b, labels = bandas_edad)
pop_ine <- pop_ine %>% select(-EDAD) %>% rename(pop = a2017, sexo = SEXO)
pop_ine <- pop_ine %>% group_by(sexo, edad) %>% summarise(pop = sum(pop)) %>% ungroup
pop_ine <- pop_ine %>% mutate(sexo = ifelse(sexo == 1, "M", "F"))
pop_ine <- pop_ine %>% mutate(pop = ifelse(sexo=="M", pop*-1, pop))


g3 <- ggplot(data=pop_ine,aes(x=edad,y = pop, fill=sexo)) + 
  geom_bar(data=subset(pop_ine,sexo=="F"), stat = "identity") + 
  geom_bar(data=subset(pop_ine,sexo=="M"), stat = "identity") + 
  scale_y_continuous(breaks = c(-1500000, -750000, 0, 750000, 1500000),
                     labels = paste0(as.character(c(1.5, 0.75, 0, 0.75, 1.5)), "MM")) + 
  coord_flip() +
  scale_fill_brewer(palette = "Set1") + theme_minimal() +
  labs(title = "Source population", x = "", y = "", fill = "Sex")

library(gridExtra)
grid.arrange(g1, g2, g3, nrow = 1)

ggsave("pyramid.pdf")
