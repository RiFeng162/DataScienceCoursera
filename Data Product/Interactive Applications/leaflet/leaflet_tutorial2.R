# Tutorial link:http://rstudio.github.io/leaflet/

library(leaflet)

m <- leaflet() %>% 
  setView(lng = -71.0589, lat = 42.3601, zoom = 12) # set the center of the view

m %>% addProviderTiles(providers$Stamen.Toner) # use 3rd-party map info
m %>% addProviderTiles(providers$Esri.NatGeoWorldMap) 
  # use names(providers) to see the list of maps provided

leaflet(data = quakes[1:20,]) %>% 
  addTiles() %>%
  addMarkers(~long, ~lat, 
             popup = ~as.character(mag),  # shown when you click the markers
             label = ~as.character(depth)) # shown when mouse hover the markers


###### Lines and Shapes ######
leaflet(neStates) %>%
  addPolygons(color = "#444444", # stroke color 轮廓线颜色
              weight = 1, # stroke width
              smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorQuantile("YlOrRd", ALAND)(ALAND), # 填充颜色
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) # emphasize the currently moused-hover polygon

cities <- read.csv(textConnection("
City,Lat,Long,Pop
Boston,42.3601,-71.0589,645966
Hartford,41.7627,-72.6743,125017
New York City,40.7127,-74.0059,8406000
Philadelphia,39.9500,-75.1667,1553000
Pittsburgh,40.4397,-79.9764,305841
Providence,41.8236,-71.4222,177994
")) # return a data.frame with 4 variables
leaflet(cities) %>% addTiles() %>%
  addCircles(lng = ~Long, lat = ~Lat, weight = 1,
             radius = ~sqrt(Pop) * 30, # need the centers and radii(units in meters)
             popup = ~City)

###### Color ######
  # colorNumeric, colorBin, colorQuantile for continuous inputs
  # colorFactor for catigorical inputs
pal <- colorNumeric(palette = c("red", "green", "blue"), 
                    domain = 1:10 # range of input values
                    ) # create a palette function


###### Choropleth ######
m <- leaflet(china) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))
m %>% addPolygons(weight = 1,
                  opacity = 1,
                  fillColor = "blue",
                  fillOpacity = 0.3,
                  highlight = highlightOptions(weight = 2,
                                               color = "white",
                                               bringToFront = TRUE),
                  label = ~name)

test <- SpatialPolygonsDataFrame(china, density, match.ID = "name")

















