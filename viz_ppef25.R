rm(list=ls(all=T))

library(showtext)
font_add_google(name = "Ubuntu", family = "ubuntu")
showtext_auto()

library(tidyverse)
library(treemapify)

#Cargamos PPEF25 y PEF24
ppef25 <-read_csv("PPEF_2025.csv")
pef24 <-read_csv("PEF_2024.csv")

# Agrupamos por ramo
ramos25 <- ppef25 %>% group_by(DESC_RAMO) %>%
  summarize(monto = sum(MONTO_PROYECTO, na.rm = TRUE))
ramos25 <- ramos25 %>% ungroup()

ramos24 <- pef24 %>% group_by(DESC_RAMO) %>%
  summarize(monto = sum(MONTO_PEF_2024, na.rm = TRUE))
ramos24 <- ramos24 %>% ungroup()

#Comparamos el PPEF25 con el PEF24, añadiendo una columna con el cambio
#porcentual en el ramo, y otra diciendo si bajó o subió

comparativo <- ramos24 %>%
  left_join(ramos25, by = "DESC_RAMO")

comparativo <- comparativo %>%
  #renombramos las columnas
  rename(monto24 = monto.x,
         monto25 = monto.y) %>%
  #Vemos si el monto crecio, bajo o quedó igual
  mutate(comparison = case_when(
    monto25 > monto24 ~ "crecio",
    monto25 < monto24 ~ "bajo",
    TRUE ~ "igual"))

#Creamos las gráficas

# Grafica1 muestra únicamente cómo se divide el presupuesto
grafica1 <-ggplot(ramos25, aes(area = monto,
                            label = DESC_RAMO)) +
  
  geom_treemap(color = "white",
               fill= "#006341",
               start = "topleft") +
  
  geom_treemap_text(alpha = 0.9,
                    color = "white",
                    start = "topleft",
                    grow = T,
                    reflow = T,
                    family = "ubuntu") +

  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  
  coord_cartesian(expand = FALSE, clip = "off") +
  
  labs(title="En esto gastará México el próximo año",
       subtitle = "Cada rectángulo representa un ramo del Proyecto de Presupuesto de Egresos del 2025. Su tamaño es equivalente al monto proyectado.\nEl presupuesto definitivo se votará el 5 de diciembre del 2024",
       caption = "Fuente: Proyecto de Presupuesto de Egresos de la Federación, 2025") +
  
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(family = "ubuntu"),
        legend.position = "none",
        plot.title = element_text(hjust = 0,
                                  color = "#631100",
                                  family = "ubuntu",
                                  face = "bold",
                                  size = 36),
        plot.subtitle = element_text(size = 18),
        plot.caption = element_text(hjust = 0,
                                    size = 12))


grafica1
