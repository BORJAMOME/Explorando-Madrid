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

population <- read.csv("/Users/borja/Desktop/Madrid_Poblacion.csv", sep=",", fileEncoding="UTF-8")

# Structure
str(population)

population$date <- factor(population$date, levels = 2018:2023)


# Population by year 
population %>%
  group_by(date, Gender) %>%
  summarise(count = sum(Number)) %>%
  mutate(percent = paste0(round((count / sum(count)) * 100, 2), "%")) %>%
  ggplot(aes(x = date, y = count)) +
  geom_bar(stat = "identity", aes(fill = Gender)) +
  geom_text(aes(label = percent, group = Gender), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#91C8E4", "#FFABAB"), name = "Gender") +
  scale_y_continuous(labels = comma) +
  labs(x = "Year", y = "Population", title = "Population by year (2018-2023)") +
  theme_minimal() + # Cambiado a theme_minimal()
  theme(
    plot.background = element_rect(fill = "#f6f0ec", color = NA) # Color de fondo del gráfico
  ) +
  theme(
    panel.border = element_blank(), # Eliminar bordes del panel
    panel.grid = element_blank(), # Eliminar líneas de la cuadrícula
    axis.line = element_line(color = "#f6f0ec"), # Eliminar líneas de los ejes
    text = element_text(family = "Oswald", size = 12, color = "black") # Cambiar la tipografía aquí
  )
  
