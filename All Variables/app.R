library(shiny)
library(shinydashboard)
library(sf)
library(readr)
library(mapview)
library(dplyr)
library(tidyverse)
library(tidycensus)
library(sp)
library(leaflet)
library(shinyWidgets)
library(readxl)
mapVA_county <- st_read("cb_2018_us_county_5m.shp",
                        stringsAsFactors = FALSE) %>% filter(STATEFP == "51")
mapVA_county$COUNTYFP <- as.numeric(mapVA_county$COUNTYFP)


dis_clg_uni <- read_csv("clg_uni_virginia_distance_from_county_centroid.csv")
dis_workforce <- read_csv("workforce_distance_from_county_centroid.csv")
dis_small_business<- read_csv("small_business_distance_from_county_centroid.csv")
dis_community <- read_csv("community_college_distance_from_county_centroid.csv")


High_schools <- read_excel("High schools.xlsx")
colnames(High_schools)[3] <- "count"


Internet_Education <- read_csv("Internet_Education.csv") %>%
  rename(COUNTYFP = Code)
Internet_Education$COUNTYFP <- as.numeric(Internet_Education$COUNTYFP)

#High_schools_count <- read_excel("High schools.xlsx")


# Get VA County Outlines
va_sf<-readRDS("va_sf.rds")

# Get Page County outline
Wythe_outline<-readRDS("Wythe_outline.rds")


Wythe_area_outline<-readRDS("Wythe_area_outline.rds")

body <- dashboardBody(
  fluidRow(
    tabBox(
      title = NULL , width = 16,
      # The id lets us use input$tabset1 on the server to find the current tab
      id = "tabset1", height = "250px",
      tabPanel("Spacial_Variable", sidebarLayout(
        sidebarPanel(
          selectInput("spatial_variable", "Spatial Variable:",
                      c("Colleges and Universities",
                        "Community Colleges",
                        "Workforce Development Centers",
                        "Colleges - All types")),
          selectInput("time_variable", "Time Variable:",
                      c("60 minutes" = "60",
                        "45 minutes" = "45",
                        "30 minutes" = "30"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tableOutput("label_1"),
          leafletOutput("mapplot_1"),
          #mapview:::plainViewOutput("test")
        )
      )






               ),
      tabPanel("County_Comparison",
               sidebarPanel(
                 selectInput("variable", "Comparison Variable:",
                             c("Number of High Schools",
                               "Percentage of Broadband Access",
                               "Percentage of People having Computer"))

                 ),

               # Show a plot of the generated distribution
               mainPanel(
                 tableOutput("label_2"),
                 leafletOutput("mapplot_2")
                 #mapview:::plainViewOutput("test")
               )




       )
    )
  ),

)

shinyApp(
  ui = dashboardPage(
    dashboardHeader(title = "Variable Visualization"),
    dashboardSidebar(disable = TRUE),
    body
  ),
  server = function(input, output) {
    # The currently selected tab from the tab box

    output$label_1 <- renderText({
        paste( input$spatial_variable, input$time_variable, " minute radius")
      })
      output$mapplot_1 <- renderLeaflet({

        distance_measure = NULL
        leg= NULL
        labels = NULL
        colors= NULL
        if(input$spatial_variable == "Colleges and Universities"){
          distance_measure = dis_clg_uni
        }else if(input$spatial_variable == "Community Colleges"){
          distance_measure = dis_community
        }else if(input$spatial_variable == "Workforce Development Centers"){
          distance_measure = dis_workforce

        }else{
          distance_measure= merge(dis_clg_uni,dis_community, by = "X1")
        }



        num_col <- ncol(distance_measure)
        filtered_distance_measure <- distance_measure <=as.numeric(input$time_variable)
        distance_measure$count <- rowSums(filtered_distance_measure[, 2:num_col])

        selected_distance_measure <- distance_measure %>% rename ( COUNTYFP = X1) %>% select(COUNTYFP, count)

        map_and_data <- inner_join(mapVA_county, selected_distance_measure, by = "COUNTYFP")


        if(input$spatial_variable == "Colleges and Universities"){
          if(input$time_variable == "60" ){
            leg = c(0,1,2,3,4,5,14)
            labels = c("0","1","2","3","4","5 or more")
            mypalette <- colorQuantile(palette="viridis", leg,n= length(leg)-1)
            colors <- mypalette(map_and_data$count)
          }
          if(input$time_variable == "45" ){
            leg = c(0,1,2,3,4,5,11)
            labels = c("0","1","2","3","4","5 or more")
            mypalette <- colorQuantile(palette="viridis", leg,n= length(leg)-1)
            colors <- mypalette(map_and_data$count)
          }
          if(input$time_variable == "30" ){
            leg = c(0,1,2,3,4)
            labels =c("0","1","2","3","4")
            colors <- c("#440154", "#414487", "#2A788E", "#22A884", "#7AD151")
            mypalette <- colorFactor(colors,leg)

          }
        }else if(input$spatial_variable == "Community Colleges"){
          if(input$time_variable == "30" ){
            leg = c(0,1)
            labels =c("0","1")
            colors <- c("#440154", "#21908D")
            mypalette <- colorFactor(colors,leg)

          }
          if(input$time_variable == "45" ){
            leg = c(0,1,2,3)
            labels =c("0","1","2")
            mypalette <- colorQuantile(palette="viridis", leg,n= length(leg)-1)
            colors <- mypalette(map_and_data$count)

          }
          if(input$time_variable == "60" ){
            leg = c(0,1,2,3)
            labels =c("0","1","2")
            mypalette <- colorQuantile(palette="viridis", leg,n= length(leg)-1)
            colors <- mypalette(map_and_data$count)

          }
        }else if(input$spatial_variable == "Workforce Development Centers"){
          if(input$time_variable == "30" ){
            leg = c(0,1,2,3,4,5,8)
            labels = c("0","1","2","3","4","5 or more")
            mypalette <- colorQuantile(palette="viridis", leg,n= length(leg)-1)
            colors <- mypalette(map_and_data$count)

          }
          if(input$time_variable == "45" ){
            leg = c(0,1,2,3,4,5,11)
            labels = c("0","1","2","3","4","5 or more")
            mypalette <- colorQuantile(palette="viridis", leg,n= length(leg)-1)
            colors <- mypalette(map_and_data$count)

          }
          if(input$time_variable == "60" ){
            leg = c(0,1,2,3,4,5,11)
            labels = c("0","1","2","3","4","5 or more")
            mypalette <- colorQuantile(palette="viridis", leg,n= length(leg)-1)
            colors <- mypalette(map_and_data$count)

          }
        }else{
          if(input$time_variable == "60" ){
            leg = c(0,1,2,3,4,5,19)
            labels = c("0","1","2","3","4","5 or more")
            mypalette <- colorQuantile(palette="viridis", leg,n= length(leg)-1)
            colors <- mypalette(map_and_data$count)
          }
          if(input$time_variable == "45" ){
            leg = c(0,1,2,3,4,5,16)
            labels = c("0","1","2","3","4","5 or more")
            mypalette <- colorQuantile(palette="viridis", leg,n= length(leg)-1)
            colors <- mypalette(map_and_data$count)
          }
          if(input$time_variable == "30" ){
            leg = c(0,1,2,3,4,5,8)
            labels =c("0","1","2","3","4","5 or more")
            mypalette <- colorQuantile(palette="viridis", leg,n= length(leg)-1)
            colors <- mypalette(map_and_data$count)
          }

        }

        #mypalette <- colorQuantile(palette="viridis", leg,n= length(leg)-1)

        #mypalette <- colorQuantile(palette="viridis", c(0,max_eviction),n=12)

        #construct map
        leaflet() %>%
          addTiles() %>%
          addPolygons(data=map_and_data,color = mypalette(map_and_data$count),
                      smoothFactor = 0.2, fillOpacity=.6, weight = 1,stroke = F, label=paste(" county name ", map_and_data$NAME ,", Value: ",map_and_data$count))%>%
          addLegend(pal = mypalette,position = "topright",values = leg,labels = labels,
                    labFormat = function(type, cuts, p) {  # Here's the trick
                      paste0(labels)
                    },
                    opacity = .6,title= paste("Number of ", input$spatial_variable)) %>%

          addPolylines(data = Wythe_area_outline, color = "black", opacity = 1, weight = 1)       %>%
          addPolylines(data = Wythe_outline, color = "red", opacity = 2, weight = 1 )


      })



      output$label_2 <- renderText({
        paste( input$variable)
      })


      output$mapplot_2 <- renderLeaflet({
        if(input$variable == "Number of High Schools"){

          dataset <- High_schools
          map_and_data <- inner_join(mapVA_county, dataset, by = "COUNTYFP")

          leg <- c(1,2,3,4,5)
          mypalette <- colorQuantile(palette="viridis", leg,n= 4)
          labels <- c("1","2","3","4")
          leaflet() %>%
            addTiles() %>%
            addPolygons(data=map_and_data,color = mypalette(map_and_data$count),
                      smoothFactor = 0.2, fillOpacity=.6, weight = 1,stroke = F, label=paste(" county name ", map_and_data$NAME ,", Value: ",map_and_data$count))%>%
            addLegend(pal = mypalette,position = "topright",values = leg,labels = labels,
                    labFormat = function(type, cuts, p) {  # Here's the trick
                      n = length(cuts)
                      paste0(labels)
                    },
                    opacity = .6,title= paste("Number of High School")) %>%

            addPolylines(data = Wythe_area_outline, color = "black", opacity = 1, weight = 1)       %>%
            addPolylines(data = Wythe_outline, color = "red", opacity = 2, weight = 1 )
        }
        else if(input$variable == "Percentage of Broadband Access"){
          dataset <- Internet_Education
          map_and_data <- inner_join(mapVA_county, Internet_Education, by = "COUNTYFP")

          mypalette <- colorNumeric(palette="viridis", map_and_data$PerBroadband)
          leaflet() %>%
            addTiles() %>%
            addPolygons(data=map_and_data,color = mypalette(map_and_data$PerBroadband),
                        smoothFactor = 0.2, fillOpacity=.6, weight = 1,stroke = F, label=paste(" county name ", map_and_data$NAME ,", Value: ",map_and_data$PerBroadband))%>%
            addLegend(pal = mypalette,position = "topright",values = map_and_data$PerBroadband,

                      opacity = .6,title= paste("Percentage of Broadband Access")) %>%

            addPolylines(data = Wythe_area_outline, color = "black", opacity = 1, weight = 1)       %>%
            addPolylines(data = Wythe_outline, color = "red", opacity = 2, weight = 1 )


        }else{
          dataset <- Internet_Education
          map_and_data <- inner_join(mapVA_county, Internet_Education, by = "COUNTYFP")
          leg <- c(70,75,80,85,90,95,100)
          #mypalette <- colorNumeric(palette="viridis", map_and_data$PerHasComputer)
          mypalette <- colorNumeric(palette="viridis", leg)

          leaflet() %>%
            addTiles() %>%
            addPolygons(data=map_and_data,color = mypalette(map_and_data$PerHasComputer),
                        smoothFactor = 0.2, fillOpacity=.6, weight = 1,stroke = F, label=paste(" county name ", map_and_data$NAME ,", Value: ",map_and_data$PerHasComputer))%>%
            addLegend(pal = mypalette,position = "topright",values = leg,

                      opacity = .6,title= paste("Percentage of People having Computer ")) %>%

            addPolylines(data = Wythe_area_outline, color = "black", opacity = 1, weight = 1)       %>%
            addPolylines(data = Wythe_outline, color = "red", opacity = 2, weight = 1 )
        }
      })


    }

)
