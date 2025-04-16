
immigrants_emigrants_by_sex <- read.csv("/Users/borja/💼/R/Madrid_I/Madrid_I/immigrants_emigrants_by_sex.csv", sep=",", fileEncoding="UTF-8")


immigrants_emigrants_by_sex <- gather(immigrants_emigrants_by_sex, `Immigrants/Emigrants`,
                                      value, Immigrants:Emigrants, na.rm=TRUE)

library(ggplot2)
library(dplyr)
library(scales)


immigrants_emigrants_by_sex %>%
  group_by(Gender, `Immigrants/Emigrants`, Year) %>%
  summarise(count = sum(value)) %>%
  ggplot(aes(x = `Immigrants/Emigrants`, y = count, fill = Gender)) +
  geom_bar(stat = "identity", position = "stack") + 
  facet_grid(~Year) +
  scale_fill_manual(values = c("#1d8f89", "#ee5a45"), name = "Genero") +
  labs(y = "Población", title = "Inmigrantes y emigrantes por año y genero (2017-2021)") + 
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#fffefc", color = NA), 
    panel.border = element_blank(), 
    panel.grid = element_blank(), 
    axis.line = element_line(color = "#fffefc"), 
    text = element_text(family = "Oswald", size = 15, color = "black"), 
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank()
  )
