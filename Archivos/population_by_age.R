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


population <- read.csv("/Users/borja/💼/R/Madrid_I/Madrid_I/population.csv", sep=",", fileEncoding="UTF-8")


# Population by year 

population$Year <- ordered(population$Year, levels=c(2018,2019,2020,2021,2022,2023))
population %>%
  group_by(Year, Gender) %>%
  summarise(count=sum(Number)) %>%
  mutate(percent=paste0(round((count/sum(count))*100, 2), "%")) %>%
  ggplot(aes(x=Year, y=count)) +
  geom_bar(stat="identity", aes(fill=Gender)) +
  geom_text(aes(label=percent, group=Gender), position=position_stack(vjust=0.5), size = 3) +
  scale_y_continuous(labels=comma) +
  scale_fill_manual(values = c("#1d8f89", "#ee5a45"), name = "Genero") +
  labs(x="Año", y="Población", title="Población por año (2018-2023)") +
  theme_minimal() + 
  theme(
    plot.background = element_rect(fill = "#fffefc", color = NA)
  ) +
  theme(
    panel.border = element_blank(), 
    panel.grid = element_blank(), 
    axis.line = element_line(color = "#fffefc"), 
    text = element_text(family = "Oswald", size = 15, color = "black") 
  )

ggsave('población.png', height = 5, width = 8)

#____________________________________

age_population <- read.csv("/Users/borja/💼/R/Madrid_I/Madrid_I/age_population.csv", sep=",", fileEncoding="UTF-8")


# Ordered levels
age_population$Age <- ordered(age_population$Age, levels=c("0-4", "5-9", "10-14", "15-19",
                                                   "20-24", "25-29", "30-34", "35-39",
                                                   "40-44", "45-49", "50-54", "55-59",
                                                   "60-64", "65-69", "70-74", "75-79",
                                                   "80-84", "85-89", "90-94", "95-99"  ,"100&more"))

ggplot(data=age_population, aes(x=Age, fill=Gender)) +
  geom_bar(data=filter(age_population, Gender=="Female"), aes(y=Number), stat="identity") + 
  geom_bar(data=filter(age_population, Gender=="Male"), aes(y=Number*(-1)), stat="identity") +
  scale_y_continuous(breaks=seq(-100000, 100000, 20000), 
                     labels=comma(abs(seq(-100000, 100000, 20000)))) + 
  scale_fill_manual(values = c("#1d8f89", "#ee5a45"), name = "Genero") +
  labs(x="Rango de edad", y="Población", title="Población por rango de edad (2023)") +
  coord_flip() +
  theme_minimal() + # Cambiado a theme_minimal()
  theme(
    plot.background = element_rect(fill = "#fffefc", color = NA) # Color de fondo del gráfico
  ) +
  theme(
    panel.border = element_blank(), # Eliminar bordes del panel
    panel.grid = element_blank(), # Eliminar líneas de la cuadrícula
    axis.line = element_line(color = "#fffefc"), # Eliminar líneas de los ejes
    text = element_text(family = "Oswald", size = 15, color = "black") # Cambiar la tipografía aquí
  )
