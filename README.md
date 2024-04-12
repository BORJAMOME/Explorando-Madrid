# Exploring Madrid: A Data-driven Analysis with R 🐻🌳
![Madrid](https://github.com/BORJAMOME/Madrid_I/assets/19588053/02c8eaf8-cb54-488d-9d39-37d5ecd6bc90)

# Introduction
In this project focused on the city of Madrid, I will explore various key aspects including population, immigration and emigration movements, birth and death statistics, unemployment rates, and the most common names among residents. To conduct this analysis, I will use data provided by the Banco de Datos del Ayuntamiento de Madrid, a reliable and comprehensive source.

This portal contains a wide range of datasets on demographics, economy, education, health, and other relevant aspects of life in the city of Madrid, providing a solid foundation for my research.

# Loading Data
To begin, we'll need to load necessary libraries and import the datasets.

``` r
# Load libraries
library(tidyverse)
library(knitr)
library(scales)
library(wordcloud)
library(visNetwork)
library(networkD3)
library(knitr)
library(ggplot2)


# Change text
library(showtext)
font_add_google("Oswald")
showtext_auto()

# Read the data 
population <- read.csv("../input/population.csv", sep=",", fileEncoding="UTF-8")
age_population <- read.csv("../input/population.csv", sep=",", fileEncoding="UTF-8")
immigrants_emigrants_by_sex <- read.csv("../input/immigrants_emigrants_by_sex.csv", sep=",", fileEncoding="UTF-8")
immigrants_emigrants_by_age <- read.csv("../input/immigrants_emigrants_by_age.csv", sep=",", fileEncoding="UTF-8")
immigrants_emigrants_by_destination <- read.csv("../input/immigrants_emigrants_by_destination.csv", sep=",", fileEncoding="UTF-8")
immigrants_emigrants_by_destination2 <- read.csv("../input/immigrants_emigrants_by_destination2.csv", sep=",", fileEncoding="UTF-8")
immigrants_by_nationality <- read.csv("../input/immigrants_by_nationality.csv", sep=",", fileEncoding="UTF-8")
births <- read.csv("../input/births.csv", sep=",", fileEncoding="UTF-8")
deaths <- read.csv("../input/deaths.csv", sep=",", fileEncoding="UTF-8")
unemployment <- read.csv("../input/unemployment.csv", sep=",", fileEncoding="UTF-8")
baby_names <- read.csv("../input/most_frequent_baby_names.csv", sep=",")
names <- read.csv("../input/most_frequent_names.csv", sep=",")

```
# Data Analysis
Population

```r
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
  scale_fill_manual(values = c("#91C8E4", "#FFABAB"), name = "Gender") +
  labs(x="Year", y="Population", title="Population by year (2018-2023)") +
  theme_minimal() + 
  theme(
    plot.background = element_rect(fill = "#f6f0ec", color = NA)
  ) +
  theme(
    panel.border = element_blank(), 
    panel.grid = element_blank(), 
    axis.line = element_line(color = "#f6f0ec"), 
    text = element_text(family = "Oswald", size = 15, color = "black") 
  )

```
<img width="1266" alt="1 population" src="https://github.com/BORJAMOME/Madrid_I/assets/19588053/13a8408b-e7b9-4dd3-b1a0-47a9261a3e16">


--------------
--------------
--------------
```r
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
  scale_fill_manual(values = c("#91C8E4", "#FFABAB"), name = "Gender") +
  labs(x="Age", y="Population", title="Population by age (2023)") +
  coord_flip() +
  theme_minimal() + # Cambiado a theme_minimal()
  theme(
    plot.background = element_rect(fill = "#f6f0ec", color = NA)
  ) +
  theme(
    panel.border = element_blank(),
    panel.grid = element_blank(), 
    axis.line = element_line(color = "#f6f0ec"), 
    text = element_text(family = "Oswald", size = 15, color = "black") 
  )
```
<img width="1274" alt="2 Age_population" src="https://github.com/BORJAMOME/Madrid_I/assets/19588053/8862e89c-8567-4e89-b480-7e890dc0c2b8">


--------------
--------------
--------------

![Madrid_district_map](https://github.com/BORJAMOME/Madrid_I/assets/19588053/5776fd01-d1bb-4695-b02c-e6313afc5507)

```r
# Population by year 
population %>%
  filter(Year=="2023") %>%
  group_by(District, Gender) %>%
  summarise(count = sum(Number)) %>%
  mutate(percent = paste0(round((count / sum(count)) * 100, 2), "%")) %>%
  ggplot(aes(x = reorder(District, count), y = count)) +
  coord_flip() +
  geom_bar(stat = "identity", aes(fill = Gender)) +
  geom_text(aes(label = percent, group = Gender), position = position_stack(vjust = 0.5), size= 3) +
  scale_fill_manual(values = c("#91C8E4", "#FFABAB"), name = "Gender") +
  scale_y_continuous(labels = comma) +
  labs(x = "District", y = "Population", title = "Population by District (2023)") +
  theme_minimal() + 
  theme(
    plot.background = element_rect(fill = "#f6f0ec", color = NA) 
  ) +
  theme(
    panel.border = element_blank(), 
    panel.grid = element_blank(), 
    axis.line = element_line(color = "#f6f0ec"), 
    text = element_text(family = "Oswald", size = 15, color = "black")
  )
````








