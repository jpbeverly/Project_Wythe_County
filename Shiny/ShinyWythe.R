library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(readr)
library(shinyWidgets)
library(magrittr)
library(tidyverse)
library(sf)
library(plotly)


FIPS<-read_csv("data/FIPS")
acs_VA_geom<-readRDS("data/acs_VA_geom")
VirginiaCountyGeometry<-readRDS("data/VirginiaCountyGeometry")
acs_VA_county_wide<-read_csv("data/acs_VA_county_wide")
#VAdata<-readRDS("data/VAdata")

key=c("Percent of Households with Broadband", 
      "Percent of Workers in Healthcare", 
      "Median Income", 
      "Percent of Population with Associate's or Greater")
value=c("PerBroadband","PerHealthcare", "MedianIncome", "PropAssocPlus")
names<-as.data.frame(cbind(key,value))

sidebar <- dashboardSidebar(
  selectInput("variable", "Select a variable", choices=c("Percent of Households with Broadband"="PerBroadband", 
                                                         "Percent of Workers in Healthcare"="PerHealthcare", 
                                                         "Median Income" = "MedianIncome", 
                                                         "Percent of Population with Associate's or Greater"="PropAssocPlus"), 
              selected = "MedianIncome"),
  pickerInput("county", "Select a county",choices=c(FIPS$County), selected = "Wythe",pickerOptions(actionsBox = TRUE,multipleSeparator=", "),multiple=T)
)

body <- dashboardBody(
  plotOutput("myplot"),
  dataTableOutput("mytable")
)

ui <- dashboardPage(
  dashboardHeader(title = "Virginia Demographics"),
  sidebar = sidebar,
  body = body
)

server <- function(input, output, session) {
  
  output$myplot <- renderPlot({
    
    Countyfp<-as.character(FIPS%>%
      filter(County%in%input$county)%>%
      select(Code))
    
    
   #  p<-ggplotly(
   #    ggplot()+
   #      geom_sf(data=VAdata,aes(fill=PerBroadband,color=PerBroadband,geometry=geometry,Census_tract=Census_tract,County=County))+
   #      geom_sf(data=VirginiaCountyGeometry,inherit.aes=F,fill="transparent",color="black",size=0.25,show.legend = F)+
   #      geom_sf(data=VirginiaCountyGeometry%>%filter(COUNTYFP%in%c(FIPS$Code[FIPS$County%in%input$county])), inherit.aes = F, fill="transparent",color="white",size=0.5,show.legend = F)+
   #      labs(title="Virginia",subtitle=as.character(names$key[names$value%in%input$variable]))+
   #      theme(legend.title = element_blank())+
   #      scale_fill_viridis()+
   #      scale_color_viridis(),tooltip = c("County","Census_tract"))
   # print(p)
    

ggplot(acs_VA_geom, aes_string(fill = input$variable, color = input$variable),show.legend="fill") +
  geom_sf(aes(geometry=geometry)) +
  geom_sf(data=VirginiaCountyGeometry,inherit.aes=F,fill="transparent",color="black",size=0.5,show.legend = F)+
  geom_sf(data=VirginiaCountyGeometry%>%filter(COUNTYFP%in%c(FIPS$Code[FIPS$County%in%input$county])), inherit.aes = F, fill="transparent",color="white",size=0.5,show.legend = F)+
  labs(title="Virginia",subtitle=as.character(names$key[names$value%in%input$variable]))+
  theme(legend.title = element_blank())+
  scale_fill_viridis_c()+
  scale_color_viridis_c()
    
  })
  output$mytable <- renderDataTable({
   acs_VA_county_wide%>%
      filter(Code%in%c(FIPS$Code[FIPS$County%in%input$county]))%>%
      select(County,input$variable)%>%
      set_colnames(c("County",as.character(names$key[names$value%in%input$variable])))
  })
  
}

shinyApp(ui, server)

