# Cargar datos

## Used packages##
library(dplyr)
install.packages("readxl")
install.packages("readr")
install.packages("dplyr")
install.packages("openxlsx")
install.packages("sf")
install.packages("ggplot2")
library(ggplot2)
library(readxl)
library(readr)
library(tidyverse)
library(dplyr)
library(openxlsx)
library(sf)


## Find File##
file.choose()

## Upload data base##
data_csv<- "C:\\Users\\cdavi\\Desktop\\Portafolio 1 Alojamientos Madrid\\alojamientos_turisticos.csv"

accommodations <-read.csv(data_csv)

## Verify if it was succesfuly uploaded##

head(accommodations)
str(accommodations)



## join columns to generate just one address##

accommodations<-accommodations %>%
  unite(via, viano, numero, bloque, portal, escalera, planta, puerta, col= address, sep = "_", remove = TRUE)%>%
  print()

## Delete address 

accommodations_NA <- accommodations %>%
  select(-c(address))  

##

Accommodation_PC <- accommodations_NA %>%
  filter(!is.na(cdpostal)) 

view(Accommodation_PC)

## EXPORT IN CSV##

write.csv(Accommodation_PC, "C:/Users/cdavi/Desktop/Portafolio 1 Alojamientos Madrid/accommodations_pc.csv", row.names = FALSE, fileEncoding = "UTF-8")


# Filter records where the category is not "Uncategorized"
Accommodation_NC <- Accommodation_PC %>%
  filter(categoria != "SIN CATEGORIA")

Accommodation_NC <- Accommodation_NC %>%
  filter(categoria != "CASA HUESPEDES")

Accommodation_NC <- Accommodation_NC %>%
  filter(categoria != "APARTAMENTO RURAL")

Accommodation_NC <- Accommodation_NC %>%
  filter(categoria != "CASA RURAL")

Accommodation_NC <- Accommodation_NC %>%
  filter(categoria != "1-HOTEL-APART.")

Accommodation_NC <- Accommodation_NC %>%
  filter(categoria != "1-APART-TURISTICO")

Accommodation_NC <- Accommodation_NC %>%
  filter(categoria != "2-APART-TURISTICO")

Accommodation_NC <- Accommodation_NC %>%
  filter(categoria != "3-APART-TURISTICO")

Accommodation_NC <- Accommodation_NC %>%
  filter(categoria != "4-APART-TURISTICO")

Accommodation_NC <- Accommodation_NC %>%
  filter(categoria != "1-PENSION")

Accommodation_NC <- Accommodation_NC %>%
  filter(categoria != "2-PENSION")

Accommodation_NC <- Accommodation_NC %>%
  filter(categoria != "3-PENSION")

Accommodation_NC <- Accommodation_NC %>%
  filter(categoria != "1-HOJA DE ROBLE")

Accommodation_NC <- Accommodation_NC %>%
  filter(categoria != "2-HOJA DE ROBLE")

Accommodation_NC <- Accommodation_NC %>%
  filter(categoria != "3-HOJA DE ROBLE")

Accommodation_NC <- Accommodation_NC %>%
  filter(categoria != "5 estrellas L")

# Calculate density by category and zip code
density_hotels <- Accommodation_NC %>%
  group_by(cdpostal, categoria) %>%
  summarise(densidad = n(), .groups = "drop")


View(density_hotels)  

# # Calculate total density by zip code
density_total <- density_hotels %>%
  group_by(cdpostal) %>%
  summarise(densidad = sum(densidad, na.rm = TRUE), .groups = "drop")

view(density_total)

# Convert cdpostal to character
density_total$cdpostal <- as.character(density_total$cdpostal)

view(density_total)

# Read polygon file
file.choose()
polygon_cp <- st_read("C:\\Users\\cdavi\\Desktop\\Portafolio 1 Alojamientos Madrid\\MADRID.geojson")
view(polygon_cp)


## Joining density data to polygons by zip code
polygon_cp<- polygon_cp %>%
  left_join(density_tot, by = c("COD_POSTAL" = "cdpostal"))

###################
# Create the map ##
###################

ggplot(data = polygon_cp) +
  geom_sf(aes(fill = densidad), color = "black") +
  scale_fill_viridis_c(option = "C") +  # Escala de colores
  labs(title = "Densidad de Alojamientos por Código Postal en Madrid",
       fill = "Densidad") +
  theme_minimal()

install.packages("leaflet")
library(leaflet)

#####################
# interactive map ##
####################

leaflet(polygon_cp) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~colorNumeric("viridis", densidad,)(densidad),
    weight = 1,
    opacity = 1,
    color = "black",
    fillOpacity = 0.7,
    popup = ~paste("Código Postal:", COD_POSTAL, "<br>",
                   "Densidad de Alojamientos:", densidad)
  ) %>%
  addLegend(
    pal = colorNumeric("viridis", domain = polygon_cp$densidad),
    values = polygon_cp$densidad,
    title = "Density",
    position = "bottomright"
  )

################################################
#######   Bar chart of the 5 densest zones  ####
################################################

library(ggplot2)
install.packages("viridis")
library(viridis)
view(density_total)

# Filter the 5 zip codes with the highest density
top_5_density <- density_total %>%
  arrange(desc(densidad)) %>%  # Sort from highest to lowest density
  head(4) # Select the first 5 records
view(top_5_density)

# Create the graph with the 5 postal codes with the highest density

ggplot(top_5_density, aes(x = reorder(cdpostal, densidad), y = densidad)) +
  geom_bar(stat = "identity", aes(fill = densidad), color = "black") +  
  scale_fill_viridis(option = "D") +  
  coord_flip() + 
  labs(
    title = "Top 5 Postcodes with the Highest Density of Accommodations in Madrid",
    x = "Zip code",
      y = "Density of accommodation",
    caption = "Source: Accommodation data"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.text.y = element_text(size = 8)
  ) 