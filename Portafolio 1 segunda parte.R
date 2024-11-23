library(ggplot2)
library(readxl)
library(readr)
library(tidyverse)
library(dplyr)
library(openxlsx)
library(sf)
library(leaflet)
install.packages("tmap")
library(tmap)

####################################
# Summary of accommodation types ##
###################################

accommodation_category <- Accommodation_NC %>%
  group_by(categoria) %>%
  summarise(count = n()) %>%
  mutate(percentage = round(100 * count / sum(count), 2))

view(accommodation_category)

## visualization

ggplot(accommodation_category, aes(x = reorder(categoria, -percentage), y = percentage, fill = categoria)) +
  geom_bar(stat = "identity") +
  labs(title = "Proportion of Accommodation Types",
       x = "Type of Accommodation",
       y = "Percentage") +
  theme_minimal() +
  theme(legend.position = "right", 
        axis.text.x = element_blank())  

#######################################

# Group by zip code and category

category_distribution<- Accommodation_NC %>%
  filter(!is.na(cdpostal)) %>%
  group_by(cdpostal, categoria) %>%
  summarise(count = n()) %>%
  ungroup()

view(category_distribution)
##########

# Convert cdpostal to character

top_5_density$cdpostal <- as.character(top_5_density$cdpostal)

view(top_5_density)

# Load shapefile

choose.files()
madrid_shapefile <- st_read("C:\\Users\\cdavi\\Desktop\\Portafolio 1 Alojamientos Madrid\\MADRID.geojson")

view(madrid_shapefile)

# Rename the column in the shapefile

madrid_shapefile <- madrid_shapefile %>%
  rename ( cdpostal = COD_POSTAL)

# Filter the shapefile to only include postal codes in top_5_density

top_codigos_postales <- top_5_density$cdpostal  # Los códigos postales con mayor densidad

# Filter the shapefile to only contain these zip codes
madrid_shapefile_filtered <- madrid_shapefile %>%
  filter(cdpostal %in% top_codigos_postales)

# Check the first rows of the filtered shapefile
head(madrid_shapefile_filtered)

# join the data
top_shapefile <- madrid_shapefile_filtered %>%
  left_join(top_5_density, by = "cdpostal")

view(top_shapefile)

# Get the coordinates of the 5 postal codes 
top_codigos_centroids <- st_centroid(top_shapefile)  

# Get the limits of the bounding box of the centroids
bbox <- st_bbox(top_codigos_centroids)  # Crea el bounding box


#####



# Create the interactive map with leaflet

leaflet(data = top_shapefile) %>%
    addTiles() %>%  # Add base map (OpenStreetMap)
  addPolygons(
    fillColor = ~colorQuantile("YlOrRd", densidad)(densidad),  
    color = "white",  
    weight = 2,  
    opacity = 1,  
    fillOpacity = 0.7,  
    popup = ~paste("Código Postal: ", cdpostal, "<br>",
                   "Densidad: ", round(densidad, 2))  
  ) %>%
  addLegend(
    position = "bottomright",  # Posición de la leyenda
    pal = colorQuantile("YlOrRd", top_shapefile$densidad),  
    values = top_shapefile$densidad,
    title = "Density of accommodation"
  ) %>%
  leaflet::fitBounds(lng1 = -3.75, lat1 = 40.3, lng2 = -3.5, lat2 = 40.5)  
