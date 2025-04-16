library(tidyverse)
library(knitr)
library(scales)
library(wordcloud)
library(visNetwork)
library(networkD3)
library(knitr)
library(ggplot2)
library(showtext)

font_add_google("Oswald")
showtext_auto()

population <- read.csv("/Users/borja/Desktop/poblacion_extranjera.csv", sep=",", fileEncoding="UTF-8")


# Ordered levels
population$Age <- ordered(population$Age, levels=c("0-4", "5-9", "10-14", "15-19",
                                                   "20-24", "25-29", "30-34", "35-39",
                                                   "40-44", "45-49", "50-54", "55-59",
                                                   "60-64", "65-69", "70-74", "75-79",
                                                   "80-84", "85-89", "90-94", "95-99","100&more"))




ggplot(data=population, aes(x=Age, fill=Gender)) +
  geom_bar(data=filter(population, Gender=="Female"), aes(y=Number), stat="identity") + 
  geom_bar(data=filter(population, Gender=="Male"), aes(y=Number*(-1)), stat="identity") +
  scale_y_continuous(breaks=seq(-100000, 100000, 20000), 
                     labels=comma(abs(seq(-100000, 100000, 20000)))) + 
  scale_fill_manual(values = c("#91C8E4", "#FFABAB"), name = "Gender") +
  labs(x="Age", y="Population", title="Foreing population by age (2023)") +
  coord_flip() +
  theme_minimal() + # Cambiado a theme_minimal()
  theme(
    plot.background = element_rect(fill = "#f6f0ec", color = NA) # Color de fondo del gráfico
  ) +
  theme(
    panel.border = element_blank(), # Eliminar bordes del panel
    panel.grid = element_blank(), # Eliminar líneas de la cuadrícula
    axis.line = element_line(color = "#f6f0ec"), # Eliminar líneas de los ejes
    text = element_text(family = "Oswald", size = 10, color = "black") # Cambiar la tipografía aquí
  )
