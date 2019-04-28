## Packages

packages.used <- 
  c("geosphere", # For spatial methods  
    "threejs",   # threejs is used for 3-D interactive Earth Visualization
    "rworldmap", # For creating earth map
    "leaflet",   # Leaflet for R provides functions to control and integrate Leaflet, a JavaScript library for interactive maps, within R.
    "rgeos",      # Provides functions for handling operations on topologies.
    "raster",     # For raster image
    "DT",         # For creating interactive tables
    "ggplot2",
    "sp"   ,       # For Spatial processing of data
    "ggmap",       # To reverse geocode Long/Lat
    "knitr",        # TO enable 3-D visualization embedding in the HTML page
    "rglwidget",
    "rgl",
    "plyr",
    "reshape2",
    "maptools",
    "shiny",
    "googleVis",
    "dplyr",
    "plotly",
    "RColorBrewer",
    "treemap",
    "gplots"
  )

# check packages that need to be installed.
packages.needed=setdiff(packages.used, 
                        intersect(installed.packages()[,1] , 
                                  packages.used))
# install additional packages
if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE)
}

#load the packages
library("gplots")
library("plyr")
library("dplyr")
library("reshape2")
library("geosphere")
library("threejs")
library("rworldmap")
library("leaflet")
library("rgeos")
library("raster")
library("DT")
library("ggplot2")
library("sp")
library("ggmap")
library("knitr")
library("rglwidget")
library("rgl")
library("maptools")
library("shiny")
library("googleVis")
library("plotly")
library("grid")
library("gtable")
library("treemap")
library("RColorBrewer")
library("forcats")
library("tm")
library("tidytext")
library("corpus")
library("stringr")


##read data
df <- readr::read_csv("data_with_lon_lat.csv")
All_vio<-df%>%filter(ACTION!="Not yet received an inspection" & ACTION!= "No violations were recorded at the time of this inspection."& `CRITICAL FLAG`=="Critical")
uniq_style<-unique(All_vio$`CUISINE DESCRIPTION`)


No_vio <-df%>%filter(ACTION== "No violations were recorded at the time of this inspection.")%>%distinct(CAMIS,.keep_all = TRUE) %>% filter(!(CAMIS %in% unique(All_vio$CAMIS)))
#uniq_DBA







## UI Function

ui<- navbarPage(
  
  ##link to css.file
  theme = "bootstrap2.css",
  
  ##Project Title
  "New York City Restaurant - Inspect with US",

  tabPanel("Home",
           htmlOutput("blankspace"),
           titlePanel("New York City Restaurant Inspection"),
           h4(htmlOutput("text")),
           htmlOutput("teammates")
           ),
  
  
 
  
  ## 2D Map tab
  navbarMenu("2D Map",
             
  tabPanel("Violation",
           titlePanel("Violation Restaurant"),
           
           leafletOutput("vio_map",width = "100%", height = 600),
           
           absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                         draggable = TRUE, 
                         top = 180, left = 60, right = "auto", bottom = "auto",
                         width = 350, height = "auto",
                         
                         h2("2D Explorer"),
                         
                         selectInput(inputId = "style_2D",
                                     label  = "Select the Style",
                                     choices =c("American", "Chinese","Mexican", "Italian", "Greek" ,"French","Thai","Spanish","Indian","Mediterranean","Japanese"),
                                     selected ='American'),
                         
                         selectInput(inputId = "rest_2D",
                                     label  = "Select the Restaurant",
                                     choices =c("happy ppp","no"),
                                     selected ='no'),
                         
                         sliderInput(inputId = "Score_2D",
                                     label = "Score",
                                     value = 10,min = 1,max = 50),
                         
                         checkboxGroupInput(inputId="Grade_2D",
                                            label = "Grade of Restaurant",
                                            choices = c("A","B","C"),
                                            selected = "NULL"
                           
                           
                         )
                        
                         
           )
  ),
  
  
  tabPanel("No violation",
           titlePanel("Coffee ,tea, and others traded between US and the world"),
           
           leafletOutput("no_vio_map",width = "100%", height = 500),
           
           absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                         draggable = TRUE, 
                         top = 180, left = 60, right = "auto", bottom = "auto",
                         width = 300, height = "auto",
                         
                         h2("2D Explorer"),
                         
                         radioButtons(inputId = "type_2D",
                                      label  = "Choose import/export",
                                      choices = c('Export','Import'),
                                      selected ='Import'),
                         sliderInput(inputId = "year_2D",
                                     label = "Select a year",
                                     value = 2016, min =1996, max =2016),
                         sliderInput(inputId = "num_countries",
                                     label = "Top Countries in Trade",
                                     value = 20,min = 1,max = 50),
                         selectInput(inputId = "commodity_2D",
                                     label  = "Select the commodity",
                                     choices = c('Annual Aggregate','Chocolate', 'Coffee','Cocoa','Spices','Tea'),
                                     selected ='Coffee')
                         
           )
  )
  
  
  
  ),
  
  
  
  
  
   

  ## end 2D Map tab
  
  ## Summary Statistics tab
  navbarMenu("Summary Statistics",
                      tabPanel("fancy plot",
             
             
                      tabsetPanel(
                        
                        ##violation bubble plot
                        tabPanel("bubble plot",
                                 titlePanel("Critical Violation"),
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput(inputId = "cuisine_style_buble",
                                                 label  = "choose the cuisine style",
                                                 choices = c("ALL",uniq_style),
                                                 selected ="ALL" ),
                                     
                                     sliderInput(inputId = "top_number_bubble",
                                                 label = "Select top ",
                                                 value = 20, min =5, max =50),
                                     width = 3
                                     
                                   ),
                                   
                                   mainPanel(
                                     plotlyOutput("bubble",height = "700px")

                                   )
                                   
                                 )
                        ),
                        
                        
                        ##bar chart
                        tabPanel("bar chart",
                                 titlePanel("Critical Violation"),
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput(inputId = "cuisine_style_bar",
                                                 label  = "choose the cuisine style",
                                                 choices = c("ALL",uniq_style),
                                                 selected ="ALL" ),
                                     
                                     sliderInput(inputId = "top_number_bar",
                                                 label = "Select top ",
                                                 value = 20, min =5, max =50),
                                     width = 3
                                     
                                   ),
                                   
                                   mainPanel(
                                     plotlyOutput("bar",height = "500px")
                                     
                                   )
                                   
                                 )
                        ),
                        
                        
                        ##bar chart
                        tabPanel("box plot",
                                 titlePanel("Critical Violation"),
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput(inputId = "cuisine_style_box",
                                                 label  = "choose the cuisine style",
                                                 choices = c("ALL",uniq_style),
                                                 selected ="ALL" )
                                     
                                     #sliderInput(inputId = "top_number_box",
                                          #       label = "Select top ",
                                          #       value = 20, min =5, max =50),
                                   #  width = 3
                                     
                                   ),
                                   
                                   mainPanel(
                                     plotlyOutput("box",height = "500px")
                                     
                                   )
                                   
                                 )
                        ),
                        tabPanel("hist plot",
                                 titlePanel("Critical Violation"),
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput(inputId = "cuisine_style_hist",
                                                 label  = "choose the cuisine style",
                                                 choices = c("ALL",uniq_style),
                                                 selected ="ALL" )
                                     
                                    # sliderInput(inputId = "top_number_hist",
                                           # label = "Select top ",
                                           # value = 20, min =5, max =50),
                                      # width = 3
                                     
                                   ),
                                   
                                   mainPanel(
                                     plotlyOutput("hist",height = "500px")
                                     
                                   )
                                   
                                 )
                        )
                        
                        
                        
                        
                        
                        
                      )
                      )
                      
             )
             
             
             
             ## end Summary Statistics tab
             
)
