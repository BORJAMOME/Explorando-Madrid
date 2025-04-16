immigrants_emigrants_by_sex <- read.csv("/borja/💼/R/Madrid_I/immigrants_emigrants_by_sex.csv", sep=",", fileEncoding="UTF-8")
immigrants_emigrants_by_age <- read.csv("/Users/borja/💼/R/Madrid_I/Madrid_I/immigrants_emigrants_by_sex.csv", sep=",", fileEncoding="UTF-8")


immigrants_emigrants_by_age <- gather(immigrants_emigrants_by_age, `Immigrants/Emigrants`, 
                                      value, Immigrants:Emigrants, na.rm=TRUE)

# Ordered levels
immigrants_emigrants_by_age$Age <- ordered(immigrants_emigrants_by_age$Age, 
                                           levels=c("0-4", "5-9", "10-14", "15-19",
                                                    "20-24", "25-29", "30-34", "35-39",
                                                    "40-44", "45-49", "50-54", "55-59",
                                                    "60-64", "+ 65"))

# Immigration and emigration by age (2021)
immigrants_emigrants_by_age_2021 <- immigrants_emigrants_by_age %>%
  filter(Year=="2021")

ggplot(data=immigrants_emigrants_by_age_2021, aes(x=Age, fill=`Immigrants/Emigrants`)) +
  geom_bar(data=filter(immigrants_emigrants_by_age_2021, `Immigrants/Emigrants`=="Immigrants"), 
           aes(y=value), stat="identity") + 
  geom_bar(data=filter(immigrants_emigrants_by_age_2021, `Immigrants/Emigrants`=="Emigrants"), 
           aes(y=value*(-1)), stat="identity") +
  scale_y_continuous(breaks=seq(-30000, 30000, 5000), labels=comma(abs(seq(-30000, 30000, 5000)))) + 
  scale_fill_manual(values = c("#1d8f89", "#ee5a45"), name = "Genero") +
  labs(x="Rango de edad", y="Población", title="Inmigrantes y emigrantes por rango de edad (2021)") +
  coord_flip() +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#fffefc", color = NA),
    panel.border = element_blank(), 
    panel.grid = element_blank(), # 
    axis.line = element_line(color = "#fffefc"), 
    text = element_text(family = "Oswald", size = 15, color = "black"), 
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank()
  )
