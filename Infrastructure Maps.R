
library(sf)
setwd('../Project_Wythe_County')
getwd()
#child care centers
child <- st_read('Infrastructure Shapefiles/ccaffbf9-bd10-4681-b6f2-8459739b9ed32020313-1-96q5q8.426na.shp')
#powerplants
pplants <- st_read('Infrastructure Shapefiles/Power_Plants-shp/Power_Plants.shp')

#transmission lines
transmission_lines <- st_read('Infrastructure Shapefiles/Electric_Power_Transmission_Lines-shp/Transmission_Lines.shp')

#NCUA credit unions
credit_unions <- st_read('Infrastructure Shapefiles/NCUA_Insured_Credit_Unions-shp/NCUA_InsuredCreditUnions.shp')

#FDIC Insured banks
banks <- st_read('Infrastructure Shapefiles/FDIC_Insured_Banks-shp/FDIC_Insured_Banks.shp')

#cities and towns
cities_towns <- st_read('Infrastructure Shapefiles/Cities_and_Towns_NTAD-shp/Cities_and_Towns_NTAD.shp')

#railroads
#railroads <- st_read('Infrastructure Shapefiles/Railroads.shp')
rail_tigris <- tigris::rails(year=2018)

#roads
#roads_tigris <- tigris::roads(state = 51, county = surrounding_counties)
roads_tigris <- tigris::primary_roads(year=2018)

#pt all shape files .shp in just a single folder easy to access

#getting population and maps of surrounding of page from acs
surrounding_counties <- c("Wythe county", "Carroll county", "Grayson county","Bland county",
                          "Pulaski county", "Pittsylvania county", "Smyth county",
                          "Washington county", "Floyd county", "Patrick county", "Franklin county", "Montgomery county",
                          "Henry county", "Martinsville City", "Danville City", "Galax City", "Radford City")



myACSkey <- "2580514e97d888fe585f59b4e328fce92342fe8f"

# Get VA County Outlines
library(tidycensus)
va_sf<-get_acs(geography = "county",
               state="VA",
               county=surrounding_counties,
               variables = "B19058_002",
               survey = "acs5",
               key = myACSkey,
               year=2018,
               output = "wide",
               show_call = T,
               geometry = T,
               keep_geo_vars = T)%>%
  select(COUNTYFP,geometry)

# Get Page County outline
Wythe_outline<-get_acs(geography = "county",
                       state="VA",
                       county=c("Wythe county"),
                       variables = "B19058_002",
                       survey = "acs5",
                       key = myACSkey,
                       year=2018,
                       output = "wide",
                       show_call = T,
                       geometry = T,
                       keep_geo_vars = T)%>%
  select(COUNTYFP,geometry)

Wythe_area_outline<-get_acs(geography = "county",
                            state="VA",
                            county=surrounding_counties,
                            variables = "B19058_002",
                            survey = "acs5",
                            key = myACSkey,
                            year=2018,
                            output = "wide",
                            show_call = T,
                            geometry = T,
                            keep_geo_vars = T)%>%
  select(COUNTYFP,geometry)


mapVA_county <- st_read("cb_2018_us_county_5m/cb_2018_us_county_5m.shp",
                        stringsAsFactors = FALSE) %>% filter(STATEFP == "51")

mapVA_county$COUNTYFP <- as.numeric(mapVA_county$COUNTYFP)
names(cities_towns)[names(cities_towns) == 'countyfips'] <- 'COUNTYFP'
names(pplants)[names(pplants) == 'COUNTYFIPS'] <- 'COUNTYFP'

tr_power_plants <- st_transform(pplants %>% dplyr::filter(STATE == 'VA'), crs = 'NAD83')
tr_cities_towns <- st_transform(cities_towns %>% dplyr::filter(state_fips == '51'), crs="NAD83")
#tr_railroads <- st_transform(rail_tigris, crs="NAD83")
tr_child<- st_transform(child %>% dplyr::filter(STATE == 'VA'), crs = "NAD83")
tr_credit_unions <- st_transform(credit_unions %>% dplyr::filter(STATE == 'VA'), crs="NAD83")
tr_banks<- st_transform(banks %>% dplyr::filter(STALPBR == 'VA'), crs = "NAD83")
#tr_roads <- st_transform(roads_tigris, crs = "NAD83")
tr_transmission_lines <- st_transform(transmission_lines, crs = "NAD83")

#for each county, do count, then join

(Wythe_area_outline$child_care_count <- lengths(st_intersects(Wythe_area_outline, tr_child)))
(Wythe_area_outline$banks_count <- lengths(st_intersects(Wythe_area_outline, tr_banks)))
(Wythe_area_outline$credit_union_count <- lengths(st_intersects(Wythe_area_outline, tr_credit_unions)))
tr_cities_towns <- tr_cities_towns[st_intersection(tr_cities_towns, Wythe_area_outline),]
tr_power_plants <- tr_power_plants[st_intersection(tr_power_plants, Wythe_area_outline),]
tr_transmission_lines <- tr_transmission_lines[st_intersection(tr_transmission_lines, Wythe_area_outline),]

#plot(Wythe_area_outline[1, 1], reset = FALSE, col = "grey")
#plot(pts, add = TRUE)

#I've made the DataCamp course "Spatial Analysis with sf and raster in R" available again. It has some really good walk-throughs.

#install.packages("mapview")

map_and_data <- st_join(mapVA_county, tr_cities_towns, by = "COUNTYFP", left=FALSE)
map_and_data2a <- st_join(map_and_data, tr_cities_towns, by = "COUNTYFP", left=FALSE)
map_and_data2 <- st_join(tr_cities_towns, Wythe_area_outline, by = "COUNTYFP", left=FALSE)
map_and_data2 <- st_transform(map_and_data2, crs = 'WGS84')
#map_and_data3 <- st_join(tr_cities_towns, Wythe_area_outline, by = "COUNTYFP", left=FALSE)
library(mapview)

mapview_for_wythe <- mapview(map_and_data ,zcol = "name", layer.name = "Cities and Towns",alpha.regions= 1) +
  mapview(tr_child ,zcol = "NAME", layer.name = "tr_child",alpha.regions= 1)


all_outline <- mapview(va_sf, color="black", size=.5,legend=FALSE,alpha.regions= 0)

map_wythe_outline <- mapview(Wythe_outline, zcol= NULL, color="red", size=3, legend= FALSE,alpha.regions= 0)

map_wythe_surrounding_outline_child <- mapview(Wythe_area_outline, zcol= c("child_care_count"), color="black", size=3, legend= FALSE,alpha.regions= 0)
map_wythe_surrounding_outline_banks <- mapview(Wythe_area_outline, zcol= "banks_count", color="black", size=3, legend= FALSE,alpha.regions= 0)
map_wythe_surrounding_outline_creditu <- mapview(Wythe_area_outline, zcol= "credit_union_count", color="black", size=3, legend= FALSE,alpha.regions= 0)

library(leaflet)
map1 <- map_and_data %>%
  leaflet() %>%
  addProviderTiles("CartoDB") %>%
  addCircleMarkers(lng = ~LONGITUDE, lat = ~LATITUDE, label=~NAME.x,
                   radius = 3, color = 'child_care_count') %>%
  addLegend(colors = colors, labels = 'NAME.y')

leg_child <- c(2:42)
mypalette <- colorQuantile(palette="viridis", leg_child,n= length(leg_child)-1)

leaflet() %>%
  addTiles() %>%
  addPolygons(data=map_and_data2,color = mypalette(map_and_data2$child_care_count),
              smoothFactor = 0.2, fillOpacity=.6, weight = 1,stroke = F, label=paste(" county name ", map_and_data$NAME ,", Value: ",map_and_data$count))%>%
  addLegend(pal = mypalette,position = "topright",values = leg_child,labels = labels,
            labFormat = function(type, cuts, p) {  # Here's the trick
              paste0(labels)
            },
            opacity = .6) %>%
  addPolylines(data = Wythe_area_outline, color = "black", opacity = 1, weight = 1)       %>%
  addPolylines(data = Wythe_outline, color = "red", opacity = 2, weight = 1 )

leaflet() %>%
  addTiles() %>%
  addCircles(data=map_and_data2,color = mypalette(map_and_data2$child_care_count)
  ) %>% 
  addPolylines(data = Wythe_area_outline, color = "black", opacity = 1, weight = 1) %>%
  addPolylines(data = Wythe_outline, color = "red", opacity = 2, weight = 1 ) %>% 
  addLabelOnlyMarkers(lng = ~longitude, lat=~latitude)

map_and_data2 %>%
  leaflet() %>%
  addProviderTiles("CartoDB") %>% addPolygons(data=Wythe_area_outline, fillColor = ~colorQuantile("YlOrRd", child_care_count)) %>%
  addCircleMarkers(lng = ~longitude, lat = ~latitude, label=~name,
                   radius = 1, col="blue") %>% addCircleMarkers(data=tr_power_plants, lng = ~LONGITUDE, lat = ~LATITUDE, label=~NAME,
                                                                radius = 1, col="red") %>%
  addPolylines(data = Wythe_area_outline, color = "black", opacity = 1, weight = 1) %>%
  addPolylines(data = Wythe_outline, color = "red", opacity = 2, weight = 1 )


addPolygons(data=shapeData,weight=2,col = 'black',fillColor = shapeData$col,
            highlightOptions = highlightOptions(color='white',weight=1,
                                                bringToFront = TRUE)) 
  
  
  ddCircleMarkers(lng = ~longitude, lat = ~latitude, label=~name,
                                                    radius = 1, col="red")
a + tr_power_plants
  ) %>% 
  addPolylines(data = Wythe_area_outline, color = "black", opacity = 1, weight = 1) %>%
  addPolylines(data = Wythe_outline, color = "red", opacity = 2, weight = 1 ) %>% 
  addLabelOnlyMarkers(lng = ~longitude, lat=~latitude)


map_wythe_surrounding_outline <- mapview(tr_cities_towns, zcol= NULL, color="black", size=3, legend= FALSE,alpha.regions= 0)
(map_wythe <- all_outline+ map_wythe_surrounding_outline +map_wythe_outline)

map_wythe_surrounding_outline_cities <- mapview(tr_cities_towns, zcol= 'name', color="black", layer.name = "Cities and Towns", size=0.3, legend= FALSE,alpha.regions= 0, label=tr_cities_towns$name)
l_cities <- leafem::addStaticLabels(map_wythe_surrounding_outline_cities,
                label = tr_cities_towns$name)

map_wythe_surrounding_outline_child <- mapview(Wythe_area_outline, zcol= "child_care_count", color="red", size=3, legend= FALSE,alpha.regions= 0)
map_wythe_surrounding_outline_banks <- mapview(Wythe_area_outline, zcol= "banks_count", color="purple", size=3, legend= FALSE,alpha.regions= 0)
map_wythe_surrounding_outline_credit_unions <- mapview(Wythe_area_outline, zcol= "credit_union_count", color="red", size=3, legend= FALSE,alpha.regions= 0)

map_wythe_surrounding_outline_child + map_wythe_surrounding_outline_banks + map_wythe_surrounding_outline_credit_unions

library(htmltools)

leaflet(tr_cities_towns) %>% addTiles() %>% addLabelOnlyMarkers(lng=longitude, lat=latitude, label = ~htmlEscape(name))


https://github.com/jpbeverly/
  
  https://jpbeverly.github.io/Wythe_website
