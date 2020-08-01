setwd("~/git/Project_Wythe_County/isochrones/")
library(RColorBrewer)
library(traveltime)
library(tidyverse)
library(tidycensus)
library(sf)
library(osmdata)
library(leaflet)
library(sp)
library(purrr)
library(mapview)
library(osrm)
library(rmapzen)
library(rgdal)
library(ggplot2)
library(scales)
library(nycflights13)
library(readxl)
#webshot::install_phantomjs()


#getting population and maps of surrounding of page from acs




myACSkey <- "2580514e97d888fe585f59b4e328fce92342fe8f"

#show available variables for ACS survey
acs5 <- load_variables(2018, "acs5", cache=T)

# B19013_001 - MedianIncome
# B01003_001 - Total Population


#FUNCTIONS:

# 1. "acs_tables" calls "get_acs" (from tidycensus) on a vector of table names. It returns a dataframe of
# all the tables bound together.  The function requires a vector of table names,
# a census API key, and a geographical unit.  The user can add other parameters as well.

acs_tables<-function(tables,key,geography,...){
  acs_data<-NULL
  for(i in 1:length(tables)){
    data<-get_acs(geography = geography,
                  table = tables[i],
                  key = key,
                  show_call = T,
                  cache_table=T,
                  ...
    )
    acs_data<-rbind(acs_data,data.frame(data))
  }
  return(acs_data)
}

# 2. "acs_wide" cleans the data returned from a census API call.  More specifically,
# it separates the variable column into separate variables, and it separates "NAME" into
# different columns with pre-defined column names (NAME_col_names). The function also
# drops the "margin of error" column.

acs_wide<-function(data,NAME_col_names){
  data%>%
    select (-moe)%>%
    pivot_wider(names_from = variable,values_from=estimate)%>%
    separate(NAME, into=NAME_col_names, sep = ", ")
}


#3. acs_years retrieves individual variables (or a list of variables) across a series of years.
acs_years<-function(years,key,geography,...){
  acs_data<-NULL
  for(i in 1:length(years)){
    acs<-get_acs(geography = geography,
                 #variables = vars,
                 key = key,
                 year=years[i],
                 output = "wide",
                 show_call = T,
                 geometry = F,
                 ...)
    acs_data<-(rbind(acs_data,data.frame(acs)))
  }
  acs_data<-cbind(acs_data,year=rep((years),each=length(unique(acs_data$GEOID))))
  return(acs_data)
}


#4. "acs_years_tables" uses two previously defined functions (acs_tables and acs_wide) to return multiple
# variable tables across multiple years in one single tibble.  A couple of notes: the way that
# get_acs handles variables before 2013 varies, so this function only works for 2013 and after.
# For variable tables before 2013, use acs_tables to pull individual sets of tables.  Also, I have
# not included "geometry" in the function.  If the user includes geometry, he/she may need
# to modify the call to acs_wide.


acs_years_tables<-function(tables,years,key,geography,NAME_col_names,...){
  acs_data<-NULL
  for (j in 1:length(years)){
    acs<-acs_tables(tables=tables,year=years[j],key=key,geography = geography,...)
    year<-rep(years[j],times=length(acs$GEOID))
    acs_years2<-cbind(year,data.frame(acs))
    acs_data<-(rbind(acs_data,acs_years2))
  }
  acs_data<-acs_wide(acs_data,NAME_col_names = NAME_col_names)
  return(acs_data)
}


#NATIONAL AND Page DATA

# Variables
# B19013_001 - Median Income
# B01003_001 - Total Population

tables<-c("B01003","B19013")
years<-c(2018)
colnames=c("Census_tract","County","State")

# Pull ACS data for Page only
acs_Wythe<-acs_years_tables(tables=tables,
                            years=years,
                            key=myACSkey,
                            geography="tract",
                            state="VA",
                            county="Wythe",
                            NAME_col_names = colnames)
# Rename Variable Columns
acs_Wythe <- acs_Wythe %>%
  rename(
    Median_Income = B19013_001,
    Total_Population = B01003_001
  )

# Surrounding Counties

acs_Wythe_area<-acs_years_tables(tables=tables,
                                years=years,
                                key= myACSkey,
                                geography="tract",
                                state="VA",
                                county=c("Wythe county", "Carroll county", "Grayson county","Bland county",
                                          "Pulaski county", "Pittsylvania county", "Smyth county",
                                          "Washington county", "Floyd county", "Patrick county", "Franklin county",
                                          "Henry county", "Martinsville City", "Danville City", "Galax City"),
                                NAME_col_names = colnames)

# Rename Variable Columns
acs_Wythe_area <- acs_Wythe_area %>%
  rename(
    Median_Income = B19013_001,
    Total_Population = B01003_001
  )


# Get VA County Outlines
va_sf<-get_acs(geography = "county",
               state="VA",
               county=c("Wythe county", "Carroll county","Grayson county","Bland county",
                        "Pulaski county", "Pittsylvania county", "Smyth county",
                        "Washington county", "Floyd county", "Patrick county", "Franklin county",
                        "Henry county", "Martinsville City", "Danville City", "Galax City"),
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
                       county=c("Carroll county","Grayson county","Bland county",
                                "Pulaski county", "Pittsylvania county", "Smyth county",
                                "Washington county", "Floyd county", "Patrick county", "Franklin county",
                                "Henry county", "Martinsville City", "Danville City", "Galax City"),
                       variables = "B19058_002",
                       survey = "acs5",
                       key = myACSkey,
                       year=2018,
                       output = "wide",
                       show_call = T,
                       geometry = T,
                       keep_geo_vars = T)%>%
  select(COUNTYFP,geometry)




# Read in all files file

mapVA  <- st_read("VirginiaShapeFiles/tl_2019_51_tract.shp",
                  stringsAsFactors = FALSE)

map_and_data <- inner_join(mapVA, acs_Wythe_area, by = "GEOID")

Colleges_and_Universities <- read_csv("Colleges_and_Universities.csv")

filtered_college_university <- Colleges_and_Universities %>% filter( STATE == "VA") %>%
                    filter(COUNTY %in% c("WYTHE", "CARROLL", "PULASKI", "BLAND", "SMYTH",
                                         "GRAYSON","PITTSYLVANIA", "WASHINGTON", "FLOYD", "PATRICK","FRANKLIN",
                                         "HENRY", "MARTINSVILLE", "DANVILLE", "GALAX")) %>% filter( ZIP != 23851)



Workforce_Development_Centers <- read_csv("Workforce_Development_Centers.csv")
colnames(Workforce_Development_Centers)[1] <- "Longitude"
colnames(Workforce_Development_Centers)[2] <- "Latitude"

workforce_dev_center_data <- Workforce_Development_Centers %>%
  filter(Zip %in% c("24312" , "24313" , "24322" , "24323" , "24324" , "24350" , "24360" ,
                    "24368" , "24374" , "24382", # wythe
                    "24053" , "24120" , "24312" , "24317" , "24325" , "24328" , "24333" , "24343" ,
                    "24350" , "24351" , "24352" , "24380" , "24381", # Carroll
                    "24292", "24326", "24330", "24333", "24348", "24350", "24363", "24378", # Grayson
                    "24084", "24124", "24134", "24314", "24315", "24318", "24366", # Bland
                    "24084", "24141", "24301", "24324", "24347", # Pulaski
                    "24054", "24069", "24137", "24139", "24161", "24527", "24530", "24531", "24540", "24541",
                    "24549", "24557", "24563", "24565", "24566", "24569", "24586", "24592",
                    "24594", "24597",#Pittslvania
                    "24311", "24316", "24318", "24319", "24354", "24368", "24370", "24375", "24378", #Smyth
                    "24201", "24202", "24210", "24211", "24236", "24258", "24270", "24319", "24340",
                    "24361", "24370", #Washington
                    "24059", "24072", "24079", "24091", "24105", "24120", "24138", "24141", "24149", "24352",
                    "24380", #Floyd
                    "24076", "24082", "24133", "24171", "24185", #Patrick
                    "24055", "24065", "24067", "24088", "24092", "24101", "24102", "24112" , "24121", "24151",
                    "24176", "24184", #Franklin
                    "24078", "24089", "24148", "24165", "24168", #Henry
                    "24112", # Martinsville
                     "24541", #Danville
                    "24333" #Galax
  ))


hospitals <- st_read("Hospitals__Virginia_shp/Hospitals__Virginia_.shp")

hospitals <- hospitals %>% rename(Longitude = POINT_X)
hospitals <- hospitals %>% rename(Latitude = POINT_Y)

filtered_hospitals <- hospitals  %>%
  filter(Zip %in% c("24312" , "24313" , "24322" , "24323" , "24324" , "24350" , "24360" ,
                    "24368" , "24374" , "24382", # wythe
                    "24053" , "24120" , "24312" , "24317" , "24325" , "24328" , "24333" , "24343" ,
                    "24350" , "24351" , "24352" , "24380" , "24381", # Carroll
                    "24292", "24326", "24330", "24333", "24348", "24350", "24363", "24378", # Grayson
                    "24084", "24124", "24134", "24314", "24315", "24318", "24366", # Bland
                    "24084", "24141", "24301", "24324", "24347", # Pulaski
                    "24054", "24069", "24137", "24139", "24161", "24527", "24530", "24531", "24540", "24541",
                    "24549", "24557", "24563", "24565", "24566", "24569", "24586", "24592",
                    "24594", "24597",#Pittslvania
                    "24311", "24316", "24318", "24319", "24354", "24368", "24370", "24375", "24378", #Smyth
                    "24201", "24202", "24210", "24211", "24236", "24258", "24270", "24319", "24340",
                    "24361", "24370", #Washington
                    "24059", "24072", "24079", "24091", "24105", "24120", "24138", "24141", "24149", "24352",
                    "24380", #Floyd
                    "24076", "24082", "24133", "24171", "24185", #Patrick
                    "24055", "24065", "24067", "24088", "24092", "24101", "24102", "24112" , "24121", "24151",
                    "24176", "24184", #Franklin
                    "24078", "24089", "24148", "24165", "24168", #Henry
                    "24112", # Martinsville
                    "24541", #Danville
                    "24333" #Galax
  ))

CenterPop_county <- read.csv("CenPop2010_Mean_CO51.txt", header =T)

centerPop_wythe <- CenterPop_county %>% filter(COUNAME %in% c("Wythe")
) %>% filter(COUNTYFP != 620)



pop_centroid_wythe_iso_60 <- readRDS("pop_centroid_wythe_iso_60.RDS")
st_crs(pop_centroid_wythe_iso_60) = 4326

nearby_airport <- read_excel("Virginia airport.xlsx")
nearby_airport <- nearby_airport[c(1,3,4),]




mypalette <- colorNumeric(palette="viridis", map_and_data$Total_Population)

map_with_all_point <- leaflet() %>%
  addTiles() %>%
  addPolygons(data=map_and_data,color = mypalette(map_and_data$Total_Population),
              smoothFactor = 0.2, fillOpacity=.6, weight = 1,stroke = F, label=paste(" county name ", map_and_data$NAME ,", Value: ",map_and_data$Total_Population))%>%
  addLegend(pal = mypalette,position = "topright",values = map_and_data$Total_Population,

            opacity = .6,title= paste("Total Population")) %>%

  addPolylines(data = Wythe_area_outline, color = "black", opacity = 1, weight = 1)       %>%
  addPolylines(data = Wythe_outline, color = "red", opacity = 2, weight = 1 ) %>%
  addPolygons(data = pop_centroid_wythe_iso_60 , color = "white",
              opacity = 1, weight = 1, fillColor = "white",fillOpacity = .6)%>%
  addCircleMarkers(centerPop_wythe,lat = centerPop_wythe$LATITUDE, lng= centerPop_wythe$LONGITUDE,
                   radius =  4,
                   color = "red",
                   stroke = FALSE, fillOpacity = 1
  ) %>%
  addLegend(colors = "red", labels = "Wythe Population Centroid") %>%
  addCircleMarkers(nearby_airport,lat = nearby_airport$Lattitude, lng= nearby_airport$Longitude,
                   radius =  4,
                   color = "#55DDE0",
                   stroke = FALSE, fillOpacity = 1
  ) %>%
  addLegend(colors = "#55DDE0", labels = "Airports") %>%

  addCircleMarkers(filtered_college_university,lat = filtered_college_university$LATITUDE ,
                   lng = filtered_college_university$LONGITUDE,
                   radius =  3,
                   color = "orange",
                   stroke = FALSE, fillOpacity = 1
  ) %>%
  addLegend(colors = "orange", labels = "College and University") %>%
  addCircleMarkers(workforce_dev_center_data,lat = workforce_dev_center_data$Latitude ,
                 lng = workforce_dev_center_data$Longitude,
                 radius =  3,
                 color = "pink",
                 stroke = FALSE, fillOpacity = 1
) %>%
  addLegend(colors = "pink", labels = "Workforce Development")%>%
  addCircleMarkers(filtered_hospitals,lat = filtered_hospitals$Latitude ,
                   lng = filtered_hospitals$Longitude,
                   radius =  3,
                   color = "yellow",
                   stroke = FALSE, fillOpacity = 1
  ) %>%
  addLegend(colors = "yellow", labels = "Hospitals")
map_with_all_point


#traveltime_api <- "a98490dedef64cc7c347a3f103ab9836"
#traveltime_id <- "4096fbb8"

#indices = c(2,4,9)
#pop_centroid_wythe_iso_60 <- traveltime_map(appId= traveltime_id,
#                                        apiKey = traveltime_api,
#                                        location=c(centerPop_wythe$LATITUDE[1],centerPop_wythe$LONGITUDE[1]),
#                                        traveltime=3600,
#                                        type="driving",
#                                        departure="2020-08-07T08:00:00+01:00")
# we save each object as an RDS file because the api limits the amount of calls you can do in a given period of time and because we don't want to waste our time looping through this whenever we want to work.
#saveRDS(pop_centroid_wythe_iso_60, file = paste0('pop_centroid_wythe_iso_60','.RDS'))
# this particular isochrone is for a 10 minute travel window


