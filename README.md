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
library(wordcloud2)
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
age_population <- read.csv("../input/age_population.csv", sep=",", fileEncoding="UTF-8")
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
# Population by district (2023)
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
<img width="1270" alt="3 population_district" src="https://github.com/BORJAMOME/Madrid_I/assets/19588053/fb16c1f4-8ada-4112-920c-70d33adb48c3">

--------------
--------------
--------------
```r
# Population by neighborhood (2023)
population %>%
  filter(Year=="2023") %>%
  group_by(Neighborhood, Gender) %>%
  summarise(count = sum(Number)) %>%
  mutate(percent = paste0(round((count / sum(count)) * 100, 2), "%")) %>%
  ggplot(aes(x = reorder(Neighborhood, count), y = count)) +
  coord_flip() +
  geom_bar(stat = "identity", aes(fill = Gender)) +
  geom_text(aes(label = percent, group = Gender), position = position_stack(vjust = 0.5), size= 1.5) +
  scale_fill_manual(values = c("#91C8E4", "#FFABAB"), name = "Gender") +
  scale_y_continuous(labels = comma) +
  labs(x = "Neighborhood", y = "Population", title = "Population by Neighborhood (2023)") +
  theme_minimal() + 
  theme(
    plot.background = element_rect(fill = "#f6f0ec", color = NA) 
  ) +
  theme(
    panel.border = element_blank(), 
    panel.grid = element_blank(), 
    axis.line = element_line(color = "#f6f0ec"), 
    text = element_text(family = "Oswald", size = 6, color = "black")
  )
```
<img width="1268" alt="4 population_neighborhood" src="https://github.com/BORJAMOME/Madrid_I/assets/19588053/0946fe58-b462-4d41-8830-30da24d1ca95">

# Immigration and emigration
--------------
--------------
--------------
```r
immigrants_emigrants_by_sex <- gather(immigrants_emigrants_by_sex, `Immigrants/Emigrants`,
                                      value, Immigrants:Emigrants, na.rm=TRUE)
immigrants_emigrants_by_sex %>%
  group_by(Gender, `Immigrants/Emigrants`, Year) %>%
  summarise(count = sum(value)) %>%
  ggplot(aes(x = `Immigrants/Emigrants`, y = count, fill = Gender)) +
  geom_bar(stat = "identity", position = "stack") + 
  facet_grid(~Year) +
  scale_fill_manual(values = c("#91C8E4", "#FFABAB"), name = "Gender") +
  labs(y = "Population", title = "Immigration and Emigration by Year (2017-2021)") 
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#f6f0ec", color = NA), 
    panel.border = element_blank(), 
    panel.grid = element_blank(), 
    axis.line = element_line(color = "#f6f0ec"), 
    text = element_text(family = "Oswald", size = 10, color = "black"), 
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank()
  )
````
<img width="1271" alt="5 immi_emi_sex" src="https://github.com/BORJAMOME/Madrid_I/assets/19588053/d1c5dc6c-c9e2-4bed-a6f6-524f27913ef7">
--------------
--------------
--------------

```r

immigrants_emigrants_by_age <- gather(immigrants_emigrants_by_age, `Immigrants/Emigrants`, 
                                      value, Immigrants:Emigrants, na.rm=TRUE)

# Ordered levels
immigrants_emigrants_by_age$Age <- ordered(immigrants_emigrants_by_age$Age, 
                                           levels=c("0-4", "5-9", "10-14", "15-19",
                                                    "20-24", "25-29", "30-34", "35-39",
                                                    "40-44", "45-49", "50-54", "55-59",
                                                    "60-64", "65&more"))
# Immigration and emigration by age (2021)

immigrants_emigrants_by_age_2021 <- immigrants_emigrants_by_age %>%
  filter(Year=="2021")

ggplot(data=immigrants_emigrants_by_age_2021, aes(x=Age, fill=`Immigrants/Emigrants`)) +
  geom_bar(data=filter(immigrants_emigrants_by_age_2021, `Immigrants/Emigrants`=="Immigrants"), 
           aes(y=value), stat="identity") + 
  geom_bar(data=filter(immigrants_emigrants_by_age_2021, `Immigrants/Emigrants`=="Emigrants"), 
           aes(y=value*(-1)), stat="identity") +
  scale_y_continuous(breaks=seq(-30000, 30000, 5000), labels=comma(abs(seq(-30000, 30000, 5000)))) + 
  scale_fill_manual(values = c("#91C8E4", "#FFABAB"), name = "Gender") +
  labs(x="Age", y="Population", title="Immigration and emigration by age (2021)") +
  coord_flip() +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#f6f0ec", color = NA),
    panel.border = element_blank(), 
    panel.grid = element_blank(), # 
    axis.line = element_line(color = "#f6f0ec"), 
    text = element_text(family = "Oswald", size = 15, color = "black"), 
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank()
  )

```
<img width="1267" alt="6 immi_emi_age" src="https://github.com/BORJAMOME/Madrid_I/assets/19588053/0512b055-d29d-4ff6-9704-510c5bc8d2aa">

--------------
--------------
--------------

```r
# Nodes 
nodes <- data.frame(label = unique(immigrants_emigrants_by_destination$from))
nodes$id <- 1:nrow(nodes)



# Emigrants 
emigrants_by_destination <- immigrants_emigrants_by_destination %>%
  filter(from == "Madrid")

# Edges 
edges <- emigrants_by_destination %>%
  left_join(nodes, by = c("from" = "label")) %>%
  select(-from) %>%
  rename(from = id)

edges <- emigrants_by_destination %>% 
  left_join(nodes, by = c("from" = "label")) %>%
  select(-from) %>%
  dplyr::rename(from = id) 

edges <- edges %>%
  left_join(nodes, by = c("to" = "label")) %>%
  select(-to) %>%
  rename(to = id)

nodes_d3 <- mutate(nodes, id = id - 1)
edges_d3 <- mutate(edges, from = from - 1, to = to -1)

# sankeyNetwork - Emigrants destination 
sankeyNetwork(Links = edges_d3, Nodes = nodes_d3, Source= 'from', 
              Target = 'to', NodeID = 'label', Value = 'weight', 
              fontSize = 16, unit = 'Letter(s)')

````
<img width="1275" alt="7 emigrants_destination" src="https://github.com/BORJAMOME/Madrid_I/assets/19588053/01906d6c-3879-4a08-9d23-b202279aa068">

--------------
--------------
--------------

```r
nodes <- as.data.frame(unique(immigrants_emigrants_by_destination2$from))
nodes$id <- 1:nrow(nodes)
nodes <- nodes[, c(2,1)]
names(nodes) <- c("id", "label")

# Emigrants 
emigrants_by_destination2 <- immigrants_emigrants_by_destination2 %>%
  filter(from %in% c("Centro", "Arganzuela", "Retiro", "Salamanca", 
                     "Chamartín", "Tetuán", "Chamberí", "Fuencarral-El Pardo",
                     "Moncloa-Aravaca", "Latina","Carabanchel", "Usera", "Puente de Vallecas", 
                     "Moratalaz", "Ciudad Lineal", "Hortaleza", "Villaverde", "Villa de Vallecas",
                     "Vicálvaro", "San Blas-Canillejas", "Barajas"))

# Edges
edges <- emigrants_by_destination2 %>% 
  left_join(nodes, by=c("from"="label")) %>%
  select(-from) %>%
  dplyr::rename(from=id)

edges <- edges %>% 
  left_join(nodes, by=c("to"="label")) %>%
  select(-to) %>%
  dplyr::rename(to=id)

nodes_d3 <- mutate(nodes, id=id-1)
edges_d3 <- mutate(edges, from=from-1, to=to-1)

# sankeyNetwork - Emigrants destination

sankeyNetwork(Links=edges_d3, Nodes=nodes_d3, Source="from", Target="to", 
              NodeID="label", Value="weight", fontSize=16, unit="Letter(s)")
````
<img width="1270" alt="8 emigrants_destination_district" src="https://github.com/BORJAMOME/Madrid_I/assets/19588053/39d559bc-4c4e-449b-ac8c-bafaf2504786">


```r

# Nodes
nodes <- data.frame(label = unique(c(immigrants_emigrants_by_destination2$from, immigrants_emigrants_by_destination2$to)))
nodes$id <- 1:nrow(nodes)

# Immigrants 
immigrants_by_destination2 <- immigrants_emigrants_by_destination2 %>%
  filter(to %in% c("Centro", "Arganzuela", "Retiro", "Salamanca", 
                   "Chamartín", "Tetuán", "Chamberí", "Fuencarral-El Pardo",
                   "Moncloa-Aravaca", "Latina","Carabanchel", "Usera", "Puente de Vallecas",  
                   "Moratalaz", "Ciudad Lineal", "Hortaleza", "Villaverde", "Villa de Vallecas",
                   "Vicálvaro", "San Blas-Canillejas", "Barajas"))

# Edges
edges <- immigrants_by_destination2 %>% 
  left_join(nodes, by=c("from"="label")) %>%
  select(-from) %>%
  dplyr::rename(from=id)

edges <- edges %>% 
  left_join(nodes, by=c("to"="label")) %>%
  select(-to) %>%
  dplyr::rename(to=id)

nodes_d3 <- mutate(nodes, id=id-1)
edges_d3 <- mutate(edges, from=from-1, to=to-1)

# sankeyNetwork - Emigrants destination
sankeyNetwork(Links=edges_d3, Nodes=nodes_d3, Source="from", Target="to", 
              NodeID="label", Value="weight", fontSize=16, unit="Letter(s)")
```


<img width="1260" alt="9 immigrants_destination_district" src="https://github.com/BORJAMOME/Madrid_I/assets/19588053/b62ee49c-92b7-41fd-a883-a527f35c0360">

--------------
--------------
--------------

```r
# Immigrants by nationality

immigrants_by_nationality %>%
  filter(Year == 2020, Nationality != "Spain") %>%
  group_by(Nationality) %>%
  summarise(count = sum(Number)) %>%
  top_n(25, count) %>%
  
  ggplot(aes(x = fct_reorder(Nationality, count), y = count)) +
  geom_col(aes(fill = count), show.legend = FALSE) +
  geom_text(aes(label = count), vjust = 0.5) +
  scale_y_continuous(labels = comma) +
  scale_fill_gradient(low = "#D2E3C8", high = "#86A789") +
  labs(x = "Nationality", y = "Population", title = "Immigrants by nationality (2020)") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#f6f0ec", color = NA), 
    panel.border = element_blank(), 
    panel.grid.major = element_blank(), 
    axis.line = element_blank(), 
    text = element_text(family = "Oswald", size = 15, color = "black"), 
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank()
  ) + 
  coord_flip()
````
<img width="1260" alt="10 immigrants_nationality" src="https://github.com/BORJAMOME/Madrid_I/assets/19588053/cbb4d93b-8944-4247-9a29-837ce258a016">

--------------
--------------
--------------
```r
# Births by year

births %>%
  group_by(Year, Gender) %>%
  summarise(Count=sum(Number)) %>%
  mutate(percent=paste0(round((Count/sum(Count))*100, 2), "%")) %>%
  filter(Year %in% c(2018, 2019)) %>%
  mutate(Year = factor(Year, levels = c(2018, 2019))) %>%
  ggplot(aes(x=Year, y=Count, fill=Gender)) +
  geom_bar(stat="identity", position="stack") + 
  geom_text(aes(label=percent, group=Gender), position=position_stack(vjust=0.5)) +
  scale_fill_manual(values = c("#91C8E4", "#FFABAB"), name = "Gender") +
  labs(x="Year", y="Births", title="Births by year (2018-2019)") +
  scale_y_continuous(labels=comma) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#f6f0ec", color = NA),
  ) +
  theme(
    panel.border = element_blank(), 
    panel.grid = element_blank(), 
    axis.line = element_line(color = "#f6f0ec"), 
    text = element_text(family = "Oswald", size = 15, color = "black") 
  )
````
<img width="1275" alt="11 births_by_year" src="https://github.com/BORJAMOME/Madrid_I/assets/19588053/aa9b04ff-d6c0-461d-b704-57a72cafd3bb">


```r
# Births by district (2017)
births %>%
  filter(Year=="2019") %>%
  group_by(District, Gender) %>%
  summarise(count=sum(Number)) %>%
  mutate(percent=paste0(round((count/sum(count))*100, 2), "%")) %>%
  ggplot(aes(x=reorder(District, count), y=count)) +
  geom_bar(stat="identity", aes(fill=Gender)) +
  geom_text(aes(label=percent, group=Gender), position=position_stack(vjust=0.5), size=3) +
  scale_fill_manual(values = c("#91C8E4", "#FFABAB"), name = "Gender") +
  scale_y_continuous(breaks=seq(0, 5000, 500), labels=comma) +
  labs(x="District", y="Births", title="Births by district (2019)") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#f6f0ec", color = NA), 
  ) +
  theme(
    panel.border = element_blank(), 
    panel.grid = element_blank(),
    axis.line = element_line(color = "#f6f0ec"), 
    text = element_text(family = "Oswald", size = 15, color = "black") 
  ) + 
  coord_flip()
````
<img width="1274" alt="12 births_by_district" src="https://github.com/BORJAMOME/Madrid_I/assets/19588053/7e90dfaf-dc31-42dd-afcc-95614a4136e2">

```r
# Births by neighborhood (2017)
births %>%
  filter(Year=="2019") %>%
  group_by(Neighborhood, Gender) %>%
  summarise(count=sum(Number)) %>%
  mutate(percent=paste0(round((count/sum(count))*100, 2), "%")) %>%
  ggplot(aes(x=reorder(Neighborhood, count), y=count)) +
  geom_bar(stat="identity", aes(fill=Gender)) +
  geom_text(aes(label=percent, group=Gender), position=position_stack(vjust=0.5), size = 1.5) +
  scale_fill_manual(values = c("#91C8E4", "#FFABAB"), name = "Gender") +
  scale_y_continuous(breaks=seq(0, 5000, 500), labels=comma) +
  labs(x="Neighborhood", y="Births", title="Births by neighborhood (2019)") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#f6f0ec", color = NA), 
  ) +
  theme(
    panel.border = element_blank(), 
    panel.grid = element_blank(),
    axis.line = element_line(color = "#f6f0ec"), 
    text = element_text(family = "Oswald", size = 4.5, color = "black") 
  ) + 
  coord_flip()
````
<img width="1267" alt="13 births_by_neighborhood" src="https://github.com/BORJAMOME/Madrid_I/assets/19588053/675edea7-666a-4dd7-a46c-31c4f457c805">

```r
# Deaths in 2019

deaths %>%
  group_by(Year, Gender) %>%
  summarise(Count=sum(Number)) %>%
  mutate(percent=paste0(round((Count/sum(Count))*100, 2), "%")) %>%
  filter(Year %in% c(2019)) %>%
  mutate(Year = factor(Year, levels = c(2019))) %>%
  ggplot(aes(x=Year, y=Count, fill=Gender)) +
  geom_bar(stat="identity", position="stack") + 
  geom_text(aes(label=percent, group=Gender), position=position_stack(vjust=0.5), size=3) +
  scale_fill_manual(values = c("#91C8E4", "#FFABAB"), name = "Gender") +
  labs(x="Year", y="Deaths", title="Deaths by year (2019)") +
  scale_y_continuous(labels=comma) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#f6f0ec", color = NA), 
  ) +
  theme(
    panel.border = element_blank(), 
    panel.grid = element_blank(), 
    axis.line = element_line(color = "#f6f0ec"), 
    text = element_text(family = "Oswald", size = 15, color = "black") 
  )
```
<img width="1274" alt="14 deaths_by_year" src="https://github.com/BORJAMOME/Madrid_I/assets/19588053/30885a1f-1f1b-40cb-aa49-11fe7caf224d">

```r
# Deaths by district (2019)

deaths %>%
  filter(Year=="2019") %>%
  group_by(District, Gender) %>%
  summarise(count=sum(Number)) %>%
  mutate(percent=paste0(round((count/sum(count))*100, 2), "%")) %>%
  ggplot(aes(x=reorder(District, count), y=count)) +
  geom_bar(stat="identity", aes(fill=Gender)) +
  geom_text(aes(label=percent, group=Gender), position=position_stack(vjust=0.5), size = 3) +
  scale_fill_manual(values = c("#91C8E4", "#FFABAB"), name = "Gender") +
  scale_y_continuous(breaks=seq(0, 5000, 500), labels=comma) +
  labs(x="District", y="Deaths", title="Deaths by district (2019)") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#f6f0ec", color = NA), 
  ) +
  theme(
    panel.border = element_blank(), 
    panel.grid = element_blank(), 
    axis.line = element_line(color = "#f6f0ec"), 
    text = element_text(family = "Oswald", size = 15, color = "black") 
  ) + 
  coord_flip()
````
<img width="1274" alt="15 deaths_by_district" src="https://github.com/BORJAMOME/Madrid_I/assets/19588053/54aa32a7-aa4f-4977-a205-e63871821221">

```r
# Deaths by neighborhood (2017)

deaths %>%
  filter(Year=="2019") %>%
  group_by(Neighborhood, Gender) %>%
  summarise(count=sum(Number)) %>%
  mutate(percent=paste0(round((count/sum(count))*100, 2), "%")) %>%
  ggplot(aes(x=reorder(Neighborhood, count), y=count)) +
  geom_bar(stat="identity", aes(fill=Gender)) +
  geom_text(aes(label=percent, group=Gender), position=position_stack(vjust=0.5), size = 1) +
  scale_fill_manual(values = c("#91C8E4", "#FFABAB"), name = "Gender") +
  scale_y_continuous(breaks=seq(0, 5000, 500), labels=comma) +
  labs(x="Neighborhood", y="Deaths", title="Deaths by neighborhood (2019)") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#f6f0ec", color = NA), 
  ) +
  theme(
    panel.border = element_blank(), 
    panel.grid = element_blank(), 
    axis.line = element_line(color = "#f6f0ec"), 
    text = element_text(family = "Oswald", size = 5, color = "black") 
  ) + 
  coord_flip()
````
<img width="1275" alt="16 deaths_by_neighborhood" src="https://github.com/BORJAMOME/Madrid_I/assets/19588053/ee5444f4-4a8d-4047-8849-92d7ffbcf082">

```r
# Causes of death in Madrid 2019
death_causes %>%
  filter(Year == "2019") %>%
  group_by(Cause) %>%
  summarise(total = sum(Number)) %>%
  top_n(15, total) %>%
  
  ggplot(aes(x = reorder(Cause, total), y = total, fill = total)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label = total), vjust = 0.5, size = 3) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_gradient(low = "#DADAEB", high = "#9E9AC8") +
  labs(x = "Cause", y = "Total", title = "Top 20 causes of death (2019)") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#f6f0ec", color = NA),
    panel.border = element_blank(), 
    panel.grid.major = element_blank(), 
    axis.line = element_blank(), 
    text = element_text(family = "Oswald", size = 10, color = "black"), 
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank()
  ) + 
  coord_flip()
```
<img width="1276" alt="17 cause_of_death" src="https://github.com/BORJAMOME/Madrid_I/assets/19588053/06837ef6-3ef0-4127-9b92-a46031463c9f">

```r
# Unemployment by month and year (2021-2023)

month_order <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

unemployed <- unemployed %>%
  mutate(Month = factor(Month, levels = month_order))

ggplot(unemployed, aes(x = Month, y = Number, fill = factor(Year))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = comma(Number)), vjust = -0.5, size = 2, position = position_dodge(width = 0.9)) +
  labs(x = "Month", y = "Number of unemployed", title = "Unemployment by month and year (2021-2023)", fill = "Year") +
  scale_fill_manual(values = c("#EFBC9B", "#FBF3D5", "#D6DAC8")) +  
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#f6f0ec", color = NA), 
    panel.grid.major = element_blank(), 
    axis.line = element_line(color = "#f6f0ec"), 
    text = element_text(family = "Oswald", size = 15, color = "black"), 
    axis.text.x = element_text(angle = 45, hjust = 1) 
  )
````
<img width="1272" alt="18 unemployed" src="https://github.com/BORJAMOME/Madrid_I/assets/19588053/21fb7582-6b01-411b-8478-689e441a3a7c">

```r
#Name wordcloud

names <- names %>%
  group_by(Name) %>%
  summarise(count = sum(Frecuency)) %>%
  arrange(desc(count))


min_freq <- 10
names_filtered <- names %>%
  filter(count >= min_freq)


wordcloud2(names_filtered, size = 1.5, fontFamily = "Oswald", color = "random-light",
           backgroundColor = "#f6f0ec", rotateRatio = 0.3, minRotation = -pi/4, maxRotation = pi/4,
           gridSize = 10, minSize = 10, shuffle = TRUE)
```
<img width="1201" alt="19 name_wordcloud" src="https://github.com/BORJAMOME/Madrid_I/assets/19588053/94d317b4-cef5-44ba-95ca-b5d91904ecdd">

```r
#Surname wordcloud

surname <- surname %>%
  group_by(Surname) %>%
  summarise(count = sum(Frecuency)) %>%
  arrange(desc(count))

min_freq <- 10
surname_filtered <- surname %>%
  filter(count >= min_freq)

wordcloud2(surname_filtered, size = 1.5, fontFamily = "Oswald", color = "random-light",
           backgroundColor = "#f6f0ec", rotateRatio = 0.3, minRotation = -pi/4, maxRotation = pi/4,
           gridSize = 10, minSize = 10, shuffle = TRUE)
````
<img width="1194" alt="20 surname_wordcloud" src="https://github.com/BORJAMOME/Madrid_I/assets/19588053/20469004-e11c-4dae-aa2a-d53d7db048f0">

```r
#Baby names wordcloud

baby_names <- baby_names %>%
  group_by(Name) %>%
  summarise(count = sum(Frecuency)) %>%
  arrange(desc(count))

min_freq <- 10
baby_names_filtered <- baby_names %>%
  filter(count >= min_freq)

wordcloud2(baby_names_filtered, size = 1.5, fontFamily = "Oswald", color = "random-light",
           backgroundColor = "#f6f0ec", rotateRatio = 0.3, minRotation = -pi/4, maxRotation = pi/4,
           gridSize = 10, minSize = 10, shuffle = TRUE)

```
<img width="1201" alt="21 baby_names_wordcloud" src="https://github.com/BORJAMOME/Madrid_I/assets/19588053/7d0c46ec-b273-4146-8cd2-991371d8ab63">

