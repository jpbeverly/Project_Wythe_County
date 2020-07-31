library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(leaflet)
library(dashboardthemes)
library(readr)
library(collapsibleTree)
library(tidyverse)
library(viridis)
library(sf)
library(mapview)
library(dplyr)
library(tidycensus)
library(sp)
library(readxl)

source("theme.R")
#Get Data
Tree<-read_csv("Tree")
Tree_Ed<-read_csv("Tree_Ed")
Tree_Ex<-read_csv("Tree_Ed")
Tree_Job<-read_csv("Tree_Ed")
Tree_Site<-read_csv("Tree_Ed")
Wythe_long<-read_csv("Wythe_long")
mapVA_county <- st_read("data/cb_2018_us_county_5m.shp",
                        stringsAsFactors = FALSE) %>% filter(STATEFP == "51")
mapVA_county$COUNTYFP <- as.numeric(mapVA_county$COUNTYFP)

dis_clg_uni <- read_csv("data/clg_uni_virginia_distance_from_county_centroid.csv")
dis_workforce <- read_csv("data/workforce_distance_from_county_centroid.csv")
dis_small_business<- read_csv("data/small_business_distance_from_county_centroid.csv")
dis_community <- read_csv("data/community_college_distance_from_county_centroid.csv")

High_schools <- read_excel("data/High schools.xlsx")
colnames(High_schools)[3] <- "count"

Internet_Education <- read_csv("data/Internet_Education.csv") %>%
  rename(COUNTYFP = Code)
Internet_Education$COUNTYFP <- as.numeric(Internet_Education$COUNTYFP)

#High_schools_count <- read_excel("data/High schools.xlsx")

# Get VA County Outlines
va_sf<-readRDS("data/va_sf.rds")
# Get Page County outline
Wythe_outline<-readRDS("data/Wythe_outline.rds")
Wythe_area_outline<-readRDS("data/Wythe_area_outline.rds")


shinyApp(
  ui = dashboardPagePlus(
    title = "DashboardPage",
    header = dashboardHeaderPlus(
      title = "DSPG 2020"
    ),
    
    # SIDEBAR (LEFT) ----------------------------------------------------------
    sidebar = dashboardSidebar(
      sidebarMenu(
        id = "tabs",
        menuItem(
          tabName = "overview",
          text = "Project Overview",
          icon = icon("info circle")
        ),
        menuItem(
          tabName = "capital",
          text = "Human and Built Capital",
          icon = icon("map-marked-alt")
        ),
        menuItem(
          tabName = "jobs",
          text = "Jobs of Tomorrow",
          icon = icon("map-marked-alt")
        ),
        menuItem(
          tabName = "both",
          text = "Accessibility",
          icon = icon("map-marked-alt")
        ),
        menuItem(
          tabName = "data",
          text = "Regional Comparisons",
          icon = icon("database")
        ),
        menuItem(
          tabName = "data",
          text = "Data & Methodology",
          icon = icon("database")
        ),
        menuItem(
          tabName = "findings",
          text = "Findings",
          icon = icon("chart-pie")
        ),
        menuItem(
          tabName = "team",
          text = "Team",
          icon = icon("user-friends")
        )
      )
    ),
    
    # BODY --------------------------------------------------------------------
    body = dashboardBody(
      customTheme,
      fluidPage(
        tabItems(
          tabItem(tabName = "overview",
                  fluidRow(
                    boxPlus(
                      title = "Project Overview",
                      closable = FALSE,
                      width = NULL,
                      status = "warning",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      h1("Industry and Workforce Attraction and Retention in Wythe County"),
                      h2("Project Description"),
                      p("The VT-DSPG used publicly available data to identify and classify the unique community, work force, and industry amenities that support economic development initiatives in Wythe county."),
                      img(src = 'Wythe.png', height = "150", width = "300", style="display: block; margin-left: auto; margin-right: auto;"),
                      h2("Project Goals"),
                      p("Contextualize industry and workforce factors at levels that are actionable for stakeholders and that promote informed policy and investment in Wythe"),
                      h2("Our Approach"),
                      p("Our research team used the community capital framework to identify the built and human capital available in Wythe
                        and surrounding counties that directly relate to industry location factors. We created composite industry and 
                        workforce attractiveness measures to compare Wythe County to other counties in the region."),
                      img(src = 'Approach.png', height = "300", width = "500", style="display: block; margin-left: auto; margin-right: auto;")
                      )
                    )),
          tabItem(tabName = "capital",
                  fluidRow(
                    boxPlus(
                      title = "Wythe County",
                      closable = FALSE,
                      status = "warning",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      width = NULL,
                      enable_dropdown = TRUE,
                      dropdown_icon = "",
                      dropdown_menu = tagList(selectInput("var","Select a Variable",choices = c("Level of Education","Industry","Home Values","Household Income","Household Size"))),
                      plotOutput("myplot")
                    ),
                    p("Add facts/figures, graphs, maps to represent capital in other ways.  Would this be a place for Afrina's location data? Or should that have its own tab?"),
                    br()
                  )),
          tabItem(tabName = "jobs",
                  fluidRow(
                    boxPlus(
                      title = "Industries and Knowledge Areas",
                      closable = FALSE,
                      status = "warning",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      width = "100%",
                      enable_sidebar = FALSE,
                      enable_dropdown = TRUE,
                      dropdown_icon = "",
                      dropdown_menu = tagList(selectInput("var1","Select a Variable",choices = c("Skills","Education", "Experience Needed", "On-Site Training", "On-the-Job Training"))),
                      collapsibleTreeOutput("mytree",width = "100%")
                    ),
                    p("This graph maps the occupations with highest projected openings (2018-2028) onto required areas of knowledge."),
                    br()
                  )),
          tabItem(tabName = "both",
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
                  )),
          tabItem(tabName = "data",
                  fluidRow(
                    boxPlus(
                      title = "Data & Methodology",
                      closable = FALSE,
                      width = NULL,
                      status = "warning",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      h2("Data Sources"),
                      img(src = "data_sets.png", width = "450px", align = "right"),
                      h3("Data Source 1"),
                      p("Example text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante."),
                      h3("Data Source 2"),
                      p("Example text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante."),
                      h3("Data Source 3"),
                      p("Example text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante."),
                      h2("Methodology"),
                      h3("Data Preparation"),
                      p("Example text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante."),
                      h3("Data Modeling"),
                      p("Example text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante.")
                    )
                  )),
          tabItem(tabName = "findings",
                  fluidRow(
                    boxPlus(
                      title = "Findings",
                      closable = FALSE,
                      width = NULL,
                      status = "warning",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      h2("Summary of Findings"),
                      p("Example text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante."),
                      h3("Results Section One"),
                      img(src = "irrational_venn_diagram.png", width = "360px", align = "right"),
                      p("Example text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante."),
                      h3("Results Section Two"),
                      p("Example text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante."),
                      h3("Results Section Three"),
                      img(src = "food_reality_chart.png", width = "400px", align = "right"),
                      p("Example text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante.")
                    )
                  )),
          tabItem(tabName = "team",
                  fluidRow(
                    boxPlus(
                      title = "Findings",
                      closable = FALSE,
                      width = NULL,
                      status = "warning",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      h2("DSPG Team Members"),
                      img(src = 'Josh.Beverly.VT.jpg', height = "150", width = "140", align = "center"),
                      img(src = 'Dylan.Glover.VT.jpg', height = "150", width = "140", align = "center"),
                      img(src = 'Afrina.Tabassum.VT.jpg', height = "150", width = "140", align = "center"),
                      img(src = 'Adam.Wells.VT.jpg', height = "150", width = "140", align = "center"),
                      p("Josh Beverly, Fellow (Ph.D. Student at Virginia Tech, Agricultural and Applied Economics)"),
                      p("Dylan Glover, Intern (Undergraduate Student at Virginia Tech, Mathematics)"),
                      p("Afrina Tabassum, Intern (Ph.D. Student at Virginia Tech, Computer Science)"),
                      p("Adam Wells, Intern (Master Student at Virginia Tech, Data Analysis and Applied Statistics)"),
                      h2("Virginia Tech Faculty Team Members"),
                      img(src = 'Susan.Chen.VT.jpg', height = "150", width = "140", align = "center"),
                      img(src = 'Conaway.Haskins.VT.jpg', height = "150", width = "140", align = "center"),
                      img(src = 'Matt.Holt.VT.jpg', height = "150", width = "140", align = "center"),
                      img(src = 'Ford.Ramsey.VT.jpg', height = "150", width = "140", align = "center"),
                      p("Susan Chen (Associate Professor, Food and Health Economics, DSPG Project Co-Lead)"),
                      p("Conaway Haskins (Extensions Specialist, Rural & Regional Development)"),
                      p("Matt Holt (Department Head, Professor, Agribusiness, Applied Econometrics, Principal Investigator)"),
                      p("Ford Ramsey (Assistant Professor, Agribusiness, DSPG Project Co-Lead)"),
                      h2("Project Sponsors"),
                      img(src = 'VCE.Logo.png', height = "150", width = "200", align = "center", style="display: block; margin-left: auto; margin-right: auto;"),
                      p("Matthew Miller (Unit Coordinator and Extension Agent, Agriculture and Natural Resources - Farm Business Management)"),
                      
                      h2("Acknowledgements"),
                      p("We would like to thank:"),
                      p("Stephen Bear (Wythe County Administrator),"),
                      p("David Manely (Joint Industrial Development Authority of Wythe County)"),
                      p("John Matthews (Economic Developer & Associate Director at the Joint IDA of Wythe County)")
                    )
                  ))
          )
                  ))
    ),
  
  
  # SERVER ------------------------------------------------------------------
  server = function(input, output) {
    # Render Plot 1
    output$mytree <- renderCollapsibleTree({
      if(input$var1%in%"Skills"){
        Tree%>%
          filter(Job_Openings>5300)%>%
          mutate_if(is.numeric, ~replace(., is.na(.), 0))%>%
          filter(Importance>=2.88)%>%
          group_by(Occupation)%>%
          collapsibleTree(hierarchy = c("Career_Cluster","Career_Pathway","Occupation","Element_Name"),
                          root="Industries",
                          attribute = "Job_Openings",
                          width=1800,
                          zoomable=F)
      }
      
      
      else if(input$var1%in%"Education"){
        Tree_Ed%>%
          filter(Job_Openings>5300)%>%
          mutate_if(is.numeric, ~replace(., is.na(.), 0))%>%
          filter(Importance>=2.88)%>%
          group_by(Occupation)%>%
          collapsibleTree(hierarchy = c("Career_Cluster","Career_Pathway","Occupation","Category_Description"),
                          root="Industries",
                          attribute = "Job_Openings",
                          width=1800,
                          zoomable=F)
        
      }
      else if(input$var1%in%"Experience Needed"){
        Tree_Ex%>%
          filter(Job_Openings>5300)%>%
          mutate_if(is.numeric, ~replace(., is.na(.), 0))%>%
          filter(Importance>=2.88)%>%
          group_by(Occupation)%>%
          collapsibleTree(hierarchy = c("Career_Cluster","Career_Pathway","Occupation","Category_Description"),
                          root="Industries",
                          attribute = "Job_Openings",
                          width=1800,
                          zoomable=F)
        
      }
      else if(input$var1%in%"On-Site Training"){
        Tree_Site%>%
          filter(Job_Openings>5300)%>%
          mutate_if(is.numeric, ~replace(., is.na(.), 0))%>%
          filter(Importance>=2.88)%>%
          group_by(Occupation)%>%
          collapsibleTree(hierarchy = c("Career_Cluster","Career_Pathway","Occupation","Category_Description"),
                          root="Industries",
                          attribute = "Job_Openings",
                          width=1800,
                          zoomable=F)
        
      }
      else {
        Tree_Job%>%
          filter(Job_Openings>5300)%>%
          mutate_if(is.numeric, ~replace(., is.na(.), 0))%>%
          filter(Importance>=2.88)%>%
          group_by(Occupation)%>%
          collapsibleTree(hierarchy = c("Career_Cluster","Career_Pathway","Occupation","Category_Description"),
                          root="Industries",
                          attribute = "Job_Openings",
                          width=1800,
                          zoomable=F)
        
      }
    })
    
    
    # Render Plot 2
    output$myplot <- renderPlot({
      if(input$var%in%"Household Size"){
        Wythe_long%>%filter(Variable%in%input$var)%>%
          ggplot(mapping=aes(x=name,y=value,fill=name))+
          geom_col()+
          scale_fill_viridis(discrete = T)+
          labs (title="Household Size",y="Households",x="")+
          theme_minimal()+
          theme(legend.position = "none")
      }
      else if (input$var%in%"Level of Education"){
        Wythe_long%>%filter(Variable%in%input$var)%>%
          ggplot(mapping=aes(x=fct_inorder(name),y=value,fill=fct_inorder(name)))+
          geom_col()+
          scale_fill_viridis(discrete = T)+
          labs (title="Highest Level of Education (Age > 25)",y="Population",x="")+
          theme_minimal()+
          theme(legend.position = "none")
      }
      else if (input$var%in%"Household Income"){
        Wythe_long%>%filter(Variable%in%input$var)%>%
          ggplot(mapping=aes(x=fct_inorder(name),y=value,fill=fct_inorder(name)))+
          geom_bar(stat = "identity")+
          scale_fill_viridis(discrete = T)+
          labs (title="Household Income",y="Households",x="")+
          theme_minimal()+
          theme(legend.position = "none",axis.text.x =element_text(angle=45,hjust=1,vjust=1))
      }
      else if (input$var%in%"Home Values"){
        Wythe_long%>%filter(Variable%in%input$var)%>%
          ggplot(mapping=aes(x=fct_inorder(name),y=value,fill=fct_inorder(name)))+
          geom_bar(stat = "identity")+
          scale_fill_viridis(discrete = T)+
          labs (title="Home Values",y="Number of Homes",x="")+
          theme_minimal()+
          theme(legend.position = "none",axis.text.x =element_text(angle=45,hjust=1,vjust=1))
      }
      else{
        Wythe_long%>%filter(Variable%in%input$var)%>%
          ggplot(mapping=aes(x=name,y=value,fill=name))+
          geom_bar(stat = "identity")+
          scale_fill_viridis(discrete = T)+
          labs (title="Employment by Industry",y="Population",x="")+
          theme_minimal()+
          theme(legend.position = "none",axis.text.x =element_text(angle=45,hjust=1,vjust=1))
      }
    })
    
    # Create Map Points 1
    points <- eventReactive(input$recalc, {
      cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
    }, ignoreNULL = FALSE)
    
    # Render Map 1
    output$mymap <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)) %>%
        addMarkers(data = points())
    })
    
    # Create Map Points 2
    points2 <- eventReactive(input$recalc2, {
      cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
    }, ignoreNULL = FALSE)
    
    # Render Map 2
    output$mymap2 <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)) %>%
        addMarkers(data = points2())
    })
    #Spatial Variable Maps
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
    
    
    #Spatial Map 2
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
