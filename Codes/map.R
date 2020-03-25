
library("tidyverse")
library("leaflet")

locations <- read_csv("/Users/leeyeji/Desktop/MSBA_20Winter/Customer&Social Analytics /final project/tweets-during-cavaliers-vs-warriors/locations.csv")


#final interactive map 
#https://towardsdatascience.com/making-interactive-maps-in-r-with-less-than-15-lines-of-code-bfd81f587e12
#choose more maptiles from : 
# https://leaflet-extras.github.io/leaflet-providers/preview/
#examples using each maptile:  https://nbviewer.jupyter.org/github/python-visualization/folium/tree/master/examples/

map <-locations %>% 
  leaflet() %>%
  addProviderTiles(providers$Esri.WorldPhysical) %>%
  addMarkers(clusterOptions = markerClusterOptions(),lat=locations$lat, lng=locations$lon)

#original from kaggle notebook 

leaflet(data=locations) %>%
  addTiles() %>%
  addCircles (lat=locations$lat, lng=locations$lon)

#animation

install.packages("animation")
install.packages("htmlwidgets")

library(htmlwidgets)
library(webshot)

install.packages("webshot")
webshot::install_phantomjs()
saveWidget(map, file = "myFile.html")
webshot("myFile.html", file= sprintf('Rplot%02d.png'), cliprect = 'viewport' )







