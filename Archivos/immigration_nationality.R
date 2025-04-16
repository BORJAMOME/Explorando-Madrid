# Leer los datos
immigrants3 <- read.csv("/Users/borja/💼/R/Madrid_I/Madrid_I/immigrants_by_nationality.csv", sep = ",", fileEncodin = "UTF-8")


immigrants3 %>%
  filter(Year == 2020, Nationality != "Spain") %>%
  group_by(Nationality) %>%
  summarise(count = sum(Number)) %>%
  top_n(25, count) %>%
  
  ggplot(aes(x = fct_reorder(Nationality, count), y = count)) +
  geom_col(aes(fill = count), show.legend = FALSE) +
  geom_text(aes(label = count), vjust = 0.5) +
  scale_y_continuous(labels = comma) +
  scale_fill_gradient(low = "#c8b7e8", high = "#8724f5") +
  labs(x = "Nacionalidad", y = "Población", title = "Inmigrantes por nacionalidad (2020)") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#fffefc", color = NA), # Color de fondo de la gráfica
    panel.border = element_blank(), # Eliminar bordes del panel
    panel.grid.major = element_blank(), # Eliminar líneas de la cuadrícula
    axis.line = element_blank(), # Eliminar líneas de los ejes
    text = element_text(family = "Oswald", size = 15, color = "black"), # Cambiar la tipografía aquí
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank()
  ) + 
  coord_flip()
