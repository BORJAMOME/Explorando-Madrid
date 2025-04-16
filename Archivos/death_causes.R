# Leer los datos
death_causes <- read.csv("/Users/borja/💼/R/Madrid_I/Madrid_I/death_causes.csv", sep = ",", fileEncoding = "UTF-8")

death_causes %>%
  filter(Year == "2019") %>%
  group_by(Cause) %>%
  summarise(total = sum(Number)) %>%
  top_n(15, total) %>%
  
  ggplot(aes(x = reorder(Cause, total), y = total, fill = total)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label = total), vjust = 0.5, size = 3) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_gradient(low = "#d9a365", high = "#9d5254") +
  labs(x = "Causa", y = "Total", title = "Top 20 de causas de muerte en Madrid (2019)") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#fffefc", color = NA),
    panel.border = element_blank(), 
    panel.grid.major = element_blank(), 
    axis.line = element_blank(), 
    text = element_text(family = "Oswald", size = 15, color = "black"), 
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank()
  ) + 
  coord_flip()
