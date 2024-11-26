rm(list=ls(all=T))

library(showtext)
showtext_auto()

library(tidyverse)
library(treemapify)

#Cargamos PPEF
ppef25 <-read_csv("PPEF_2025.csv")

# Agrupamos por ramo
ramos <- ppef25 %>% group_by(DESC_RAMO) %>%
  summarize(monto = sum(MONTO_PROYECTO))

ramos <- ramos %>% ungroup()

#Creamos la gráfica
grafica <-ggplot(ramos, aes(area = monto,
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
  
  labs(title="Esto (probablemente) gastará México el próximo año",
       subtitle = "Cada cuadrado representa un ramo del Proyecto de Presupuesto de Egresos del 2025. Su tamaño es equivalente al monto proyectado.\nEl presupuesto definitivo se votará el 5 de diciembre del 2024",
       caption = "Fuente: Proyecto de Presupuesto de Egresos de la Federación, 2025") +
  
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(family = "ubuntu"),
        legend.position = "none",
        linewidth = 5,
        plot.title = element_text(hjust = 0,
                                  color = "#631100",
                                  family = "ubuntu",
                                  face = "bold",
                                  size = 36),
        plot.caption = element_text(hjust = 0))


grafica