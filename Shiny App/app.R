#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(shiny)
library(leaflet)
library(sf)

# |-------------- Data reading & processing ------------|

population <- read.csv("~/git/Training_DSPG_ISU/Shiny-Web-Apps/Day 2/data/co-est2019-alldata.csv")

library(readr)
Virginia_Business_Data <- read_csv("~/git/Project_Wythe_County/Virginia_Business_Data.csv")
View(Virginia_Business_Data)
(unique(Virginia_Business_Data$`2 Digit Title`))
subset <- Virginia_Business_Data %>% filter(Virginia_Business_Data$`2 Digit Title` ==  "Manufacturing") %>%
group_by(`County/Ind City`) %>%
summarise(number = n()) %>%
mutate(COUNTY = `County/Ind City`)


#print(sub)


# |-------------- Setting up the UI ------------|

header <- dashboardHeader(title = "County Population Maps")
choice <- unique(Virginia_Business_Data$`2 Digit Title`)
sidebar <- dashboardSidebar(
selectInput("selected", "Select industry:",
choices =choice,
selected = "Iowa",
selectize=TRUE),
sidebarMenu(id="menua
menuItem("industry", tabName = "map", icon = icon("draw-polygon")),
menuItem("Histogram", tabName = "histogram", icon = icon("bar-chart"))
)
)

body <- dashboardBody(
#withMathJax(),
tabItems(
tabItem(tabName = "map",
box(width = 12, status = "primary",
leafletOutput("map"))
),
tabItem(tabName = "histogram",
box(width = 12, status = "primary",
plotOutput("histogram"))
)
)
)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
    output$map <- renderLeaflet({
        selected <- input$selected
        
        states <- USAboundaries::us_boundaries(type="county")
        states <- states %>% filter(state_name == "Virginia") %>%
        mutate(
        COUNTY = name,
        STATE = as.numeric(statefp)
        )
        
        sub <- Virginia_Business_Data %>% filter(Virginia_Business_Data$`2 Digit Title` ==  selected) %>%
        group_by(`County/Ind City`) %>%
        summarise(number = n()) %>%
        mutate(COUNTY = `County/Ind City`)
        
        plotdat <- states %>% left_join(sub,
        by="COUNTY") %>% filter( !is.na(number))
        # COUNTY and STATE are fips codes
        
        # range of numbers in the color palette
        scale_range <- c(0,max(plotdat$number))
        # missing values are bright green, so we can see them and fix them :)
        pal <- colorNumeric("Reds", scale_range, na.color = "#aaff56", reverse=FALSE)
        
        
        leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(data = plotdat,
        color = "#000000", # outline of polygons
        fillColor = ~pal(plotdat$number), # color mapping
        fillOpacity = 0.9,
        weight = 0.2,
        smoothFactor = 0.2,
        # text to be shown on click is in html
        popup = ~ paste0(name,", ", str_extract(state_name, "^([^,]*)"), "<br>", number)) %>%
        addLegend(pal = pal,
        values = scale_range,
        position = "topright",
        title = "number <br>Estimate 2019"
        )
    })
    
    output$histogram <- renderPlot({
        selected <- input$selected
        plotdat <- population %>% filter(STNAME == selected)
        
        plotdat %>%
        ggplot(aes(x = POPESTIMATE2019)) + geom_histogram(bins = 15) +
        ggtitle(sprintf("Histogram of estimated 2019 population by county in %s", selected))
    })
}

shinyApp(ui, server)
