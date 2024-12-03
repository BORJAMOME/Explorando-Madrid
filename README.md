# Explorando Madrid: Un análisis basado en datos con R 

## **Introducción**
En este proyecto, realizaremos un análisis exhaustivo sobre varios aspectos clave de la ciudad de **Madrid**, con el objetivo de comprender mejor su dinámica social y económica. Algunos de los temas que exploraremos incluyen:

- **Evolución de la población**
- **Flujos migratorios** (inmigración y emigración)
- **Estadísticas de nacimientos y defunciones**
- **Tasas de desempleo**
- **Nombres más comunes entre los residentes**

Para realizar este análisis, utilizamos datos del **Banco de Datos del Ayuntamiento de Madrid**, una fuente pública y confiable.

# <sub> Carga de Datos </sub>

El primer paso en este análisis es cargar las bibliotecas necesarias y los conjuntos de datos que se utilizarán en la investigación. A continuación se muestra el código que utilizamos para importar las bibliotecas y leer los archivos CSV:

``` r
# Cargar bibliotecas necesarias para el análisis de datos
library(tidyverse)  # Paquete para manipulación de datos y gráficos
library(knitr)      # Para crear informes dinámicos
library(scales)     # Para escalas en gráficos
library(wordcloud2) # Para generar nubes de palabras
library(visNetwork) # Para redes interactivas
library(networkD3)  # Para redes dinámicas

# Cambiar la fuente del texto a "Oswald" para mejorar la apariencia visual
library(showtext)  # Paquete para añadir fuentes personalizadas
font_add_google("Oswald")  # Añadir fuente desde Google Fonts
showtext_auto()  # Activar la fuente personalizada para todo el proyecto

# Leer los datos desde los archivos CSV
population <- read.csv("../input/population.csv", sep=",", fileEncoding="UTF-8")  # Población total
age_population <- read.csv("../input/age_population.csv", sep=",", fileEncoding="UTF-8")  # Población por edad
immigrants_emigrants_by_sex <- read.csv("../input/immigrants_emigrants_by_sex.csv", sep=",", fileEncoding="UTF-8")  # Inmigrantes y emigrantes por sexo
immigrants_emigrants_by_destination <- read.csv("../input/immigrants_emigrants_by_destination.csv", sep=",", fileEncoding="UTF-8")  # Inmigrantes y emigrantes por destino
immigrants_emigrants_by_destination2 <- read.csv("../input/immigrants_emigrants_by_destination2.csv", sep=",", fileEncoding="UTF-8")  # Inmigrantes y emigrantes por destino (segunda tabla)
immigrants_by_nationality <- read.csv("../input/immigrants_by_nationality.csv", sep=",", fileEncoding="UTF-8")  # Inmigrantes por nacionalidad
births <- read.csv("../input/births.csv", sep=",", fileEncoding="UTF-8")  # Estadísticas de nacimientos
deaths <- read.csv("../input/deaths.csv", sep=",", fileEncoding="UTF-8")  # Estadísticas de defunciones
deaths_causes <- read.csv("../input/deaths_causes.csv", sep=",", fileEncoding="UTF-8")  # Causas de defunciones
unemployment <- read.csv("../input/unemployment.csv", sep=",", fileEncoding="UTF-8")  # Tasa de desempleo
baby_names <- read.csv("../input/most_frequent_baby_names.csv", sep=",")  # Nombres de bebés más frecuentes
names <- read.csv("../input/most_frequent_names.csv", sep=",")  # Nombres más frecuentes
surname <- read.csv("../input/most_frequent_surname.csv", sep=",")  # Apellidos más frecuentes


```
# **Análisis de Datos**
## **Población** 

El primer gráfico presenta un análisis de la **población de la ciudad de Madrid** durante el período comprendido entre los años **2018 y 2023**.

### **Objetivo del Análisis:**

- **Analizar la evolución** de la población de Madrid en términos de género durante los últimos cinco años.
- **Identificar tendencias** en el cambio de la proporción entre hombres y mujeres.

```r

# Niveles ordenados
population$Year <- ordered(population$Year, levels=c(2018,2019,2020,2021,2022,2023))

# Análisis de población por género y visualización
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
<br/><br/>
La población de la ciudad de Madrid se mantiene estable en alrededor de 6 millones de personas. La población femenina es ligeramente mayor que la masculina. Este patrón de estabilidad en la población es claro, y por esta razón, para la siguiente visualización, utilizaremos solo los datos del último año (2023).<br/><br/>


### **Población por Edad (2023)**
En este análisis, exploramos la distribución de la población de Madrid por grupos de edad durante el año 2023.

```r
# Ordenar las edades
age_population$Age <- ordered(age_population$Age, levels=c("0-4", "5-9", "10-14", "15-19",
                                                   "20-24", "25-29", "30-34", "35-39",
                                                   "40-44", "45-49", "50-54", "55-59",
                                                   "60-64", "65-69", "70-74", "75-79",
                                                   "80-84", "85-89", "90-94", "95-99"  ,
                                                   "100&more"))

# Población por Edad (2023)
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
<br/><br/>


La distribución de la población se centra en el rango de 45-65 años. Es interesante observar cómo la población masculina disminuye considerablemente a partir del rango de 60-65 años, mientras que en la población femenina esta disminución es menos pronunciada.

<br/><br/>

### **Mapa de los distritos de la ciudad de Madrid**
Ahora vamos a analizar la población por distritos. Podemos obtener una idea del tamaño y la ubicación de cada distrito utilizando el siguiente mapa.


![Madrid_district_map](https://github.com/BORJAMOME/Madrid_I/assets/19588053/5776fd01-d1bb-4695-b02c-e6313afc5507)

<br/><br/>

### **Población de Madrid por distrito**

---
```r
# Población por distrito (2023)
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
<br/><br/>

**Carabanchel** lidera como el distrito más poblado de Madrid, seguido de cerca por **Fuencarral-El Pardo**, **Latina** y **Puente de Vallecas**. En contraste, **Barajas** es el distrito menos poblado. Observamos una tendencia en la que la población femenina supera a la masculina en todos los barrios, con la excepción de **Centro**. Vamos a profundizar más y desglosar la población por barrios, con más de **131** en total.

<br/><br/>

---
### **Población de Madrid por barrios**


```r
# Población de Madrid por barrios (2023)
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
<br/><br/>


En los **50 principales barrios**, la población femenina es mayor que la masculina, excepto en **Embajadores**.

<br/><br/>

---

# Inmigración y Emigración

En esta sección, presentaremos visualizaciones para desentrañar las complejas dinámicas de la inmigración y emigración en Madrid. Comenzaremos con un análisis organizado por años.

### **Inmigración y Emigración por año (2017-2021)**

---
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
<br/><br/>


Según los últimos datos proporcionados por la **Ciudad de Madrid** hasta **2021**, la ciudad ha experimentado un **aumento progresivo** desde **2017** hasta **2019**, seguido de una **disminución** en **2020** debido a la **pandemia de Covid**, y ha vuelto a registrar aumentos en **2021**. En cuanto a la **emigración de los madrileños**, se puede observar un **aumento progresivo** en los últimos años.
<br/><br/>

---
### **Inmigración y Emigración por rango de edad (2021)**


```r

immigrants_emigrants_by_age <- gather(immigrants_emigrants_by_age, `Immigrants/Emigrants`, 
                                      value, Immigrants:Emigrants, na.rm=TRUE)

# Ordenar según rango de edad
immigrants_emigrants_by_age$Age <- ordered(immigrants_emigrants_by_age$Age, 
                                           levels=c("0-4", "5-9", "10-14", "15-19",
                                                    "20-24", "25-29", "30-34", "35-39",
                                                    "40-44", "45-49", "50-54", "55-59",
                                                    "60-64", "65&more"))
# Inmigración y Emigración por rango de edad (2021)

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
<br/><br/>

Como es evidente a partir de los datos, cerca de **30,000 inmigrantes** de **25 a 29 años** llegaron en **2021**. En términos de **emigración**, surgen **grupos de edad** distintos: uno entre **25 y 39 años**, y otro entre las personas de **65 años en adelante**. Esto sugiere un desafío para los **jóvenes** en la obtención de empleo en Madrid, mientras que los residentes mayores optan por **jubilación** en otros lugares.

<br/><br/>
<br/><br/>

En la siguiente **visualización**, analizaremos la **emigración por destino** (2021) utilizando un **diagrama de Sankey**. Un **diagrama de Sankey** muestra los **flujos** y sus **cantidades**, relativas entre sí, utilizando el **ancho de las flechas** o **líneas** para mostrar sus magnitudes. ¡Veamos!

---
```r
nodes <- as.data.frame(unique(immigrants_emigrants_by_destination2$from))
nodes$id <- 1:nrow(nodes)
nodes <- nodes[, c(2,1)]
names(nodes) <- c("id", "label")

# Emigrantes 
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


Es importante señalar que el **conjunto de datos** utilizado para esta **visualización** no especifica los **destinos fuera de España** (bajo la categoría "Exteriores"). Estos son algunos comentarios sobre el gráfico:

Se observa que la gran mayoría de los habitantes se trasladan a la **Comunidad de Madrid**. Esto puede involucrar a personas que buscan una mejor **calidad de vida** fuera de la ciudad de Madrid.

Otro grupo significativo de personas se traslada a zonas de **Castilla y León**, **Castilla-La Mancha**, **Extremadura** y **Andalucía**. Estos individuos podrían ser aquellos que emigraron hace años y están regresando a sus lugares de origen para **jubilarse**.

Por último, otra porción de la población se traslada al **extranjero** en busca de mejores **oportunidades laborales**.


<br/><br/>

En la siguiente **visualización**, se muestra cómo los **inmigrantes** se distribuyen por los diferentes **barrios** de la ciudad de **Madrid**, reflejando los movimientos y concentraciones más significativas de la población.

---

```r

# Nodes
nodes <- data.frame(label = unique(c(immigrants_emigrants_by_destination2$from, immigrants_emigrants_by_destination2$to)))
nodes$id <- 1:nrow(nodes)

# Inmigrantes
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

# sankeyNetwork - Destino emigrantes
sankeyNetwork(Links=edges_d3, Nodes=nodes_d3, Source="from", Target="to", 
              NodeID="label", Value="weight", fontSize=16, unit="Letter(s)")
```

<img width="1260" alt="9 immigrants_destination_district" src="https://github.com/BORJAMOME/Madrid_I/assets/19588053/b62ee49c-92b7-41fd-a883-a527f35c0360">

<br/><br/>

Como era de esperar, la gran mayoría de los **inmigrantes** provienen de fuera de **España**, con otra gran parte originaria de la **Comunidad de Madrid**. En la siguiente **visualización**, podemos ver de qué otros **países** provienen en el año **2020**.
<br/><br/>

---
### **Inmigrantes por nacionalidad**

```r
# Inmigrantes por nacionalidad

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

# Nacimientos

Analicemos los **nacimientos** por **año** de la **ciudad de Madrid** (2018-2019).

<br/><br/>

---
### **Nacimientos por año**

```r
# Nacimientos por año

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
<br/><br/>

El número de **nacimientos** ha mantenido una **estabilidad constante** en los últimos años. Se observa un ligero predominio de los **nacimientos masculinos** sobre los **femeninos**. A continuación, profundizaremos en el análisis de los **nacimientos** por **distrito** en **2019**.
<br/><br/>

---
### **Nacimientos por distrito (2017)**
```r
# Nacimientos por distrito (2017)
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
<br/><br/>

---
### **Nacimientos por barrio (2017)**
```r
# Nacimientos por barrio (2017)
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

<br/><br/>

---

# Muertes
En esta sección, analizaremos la **distribución de género** de los **fallecimientos** en la **ciudad de Madrid** en **2019**.
<br/><br/>

---
```r
# Muertes en 2019

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
<br/><br/>

Como era de esperar, el número de **fallecimientos masculinos** supera al de **fallecimientos femeninos**. Ahora, profundicemos en el análisis del número de **fallecidos** por **distrito** y **barrio** en la **ciudad de Madrid**.
<br/><br/>

---
### **Muertes por distrito (2019)**

```r
# Muertes por distrito (2019)

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
<br/><br/>

---
### **Muertes por barrio (2017)**
```r
# Muertes por barrio (2017)

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
<br/><br/>

Este gráfico muestra las **20 principales causas de fallecimiento** en el año **2019**, destacándose entre ellas las **enfermedades respiratorias**, la **vejez** y las **enfermedades cardíacas**.
<br/><br/>

---
### **Causas de muerte en Madrid (2019)**

```r
# CCausas de muerte en Madrid (2019)
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

# Desempleo 
El **desempleo** representa uno de los mayores desafíos para los ciudadanos de **Madrid**. Como se puede observar, ha habido una **reducción significativa** del desempleo desde **2021** hasta **2022**, resultado tanto de la disminución de la **pandemia** como de la **reestructuración económica**.

En el análisis mensual, se observa una ligera **disminución** del desempleo durante los meses de verano (**junio**, **julio**, **agosto** y **septiembre**) y en **diciembre**, probablemente influenciada por las **campañas navideñas**.

<br/><br/>

---
```r


# Desempleo por mes y año (2021-2023)

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

# Nombres más frecuentes entre los madrileños

En la bulliciosa **ciudad de Madrid**, los nombres más comunes entre **hombres** y **mujeres** incluyen **David**, **María**, **Javier**, **Carmen**, **Antonio** y **José**, cada uno de ellos con una rica **historia** y un sentido de **identidad** profundamente arraigado en la **cultura madrileña**. Estos nombres, llenos de **carácter** y **tradición**, han resonado en las calles de la **capital española** durante generaciones, formando parte de la **diversidad** y **vitalidad** de la comunidad madrileña.
<br/><br/>

---

```r
#Nombre del wordcloud

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

# Apellidos más frecuentes
<br/><br/>

---
```r

#Apellidos wordcloud

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

# Nombres de bebes más frecuentes
<br/><br/>

---
```r
#  Nombres de bebes más wordcloud

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

<br/><br/>

---

# References

This project is inspired by the work done by [Xavier](https://www.kaggle.com/xvivancos) on Kaggle about the city of Barcelona.
