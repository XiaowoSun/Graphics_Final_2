## Packages
packages.used <- 
  c("leaflet",   # Leaflet for R provides functions to control and integrate Leaflet, a JavaScript library for interactive maps, within R.
    "shiny",
    "dplyr",
    "plotly",
    "stringr",
    "forcats"
  )
# check packages that need to be installed.
packages.needed=setdiff(packages.used, 
                        intersect(installed.packages()[,1], 
                                  packages.used))
# install additional packages
if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE)
}

#load the packages
library("dplyr")
library("leaflet")
library("shiny")
library("plotly")
library("forcats")
library("stringr")

#load plot functions
source("functions.R")


##read data & clean
df <- readr::read_csv("./data/cleaned_lon_lat.csv")
unique_location <- df%>%select(CAMIS,lon,lat)%>%distinct(CAMIS,.keep_all = TRUE)

###critical violation stores
All_vio<-df%>%filter(ACTION!= "No violations were recorded at the time of this inspection."& CRITICAL.FLAG=="Critical")


## Server function
server<- function(input, output){
  
  ##Introduction
  output$blankspace = renderUI({
    HTML("<br/><br/><br/>")
  })
  output$text = renderUI({
    HTML("<br/><br/><br/>Our project looks into the violation of each restaurant that may potential cause healthy problems in New York City<br/><br/><br/><br/> Xiaowo Sun, Xin Xia")
  })

## 2D map
  output$vio_map<-renderLeaflet({
    
   leaflet() %>%setView(-73.98, 40.75, zoom = 10)%>%
      addProviderTiles("Esri")
    
  })
  
  observeEvent(input$result_2D,output$vio_map<- renderLeaflet({
    
    
    show_vio_map(df = All_vio,cuisine_style = input$style_2D,grade = input$grade_2D,rest_search = input$search2D,borough = input$Borough_2D,zipcode = input$zip_2D)
    
    
  }))
  
  observeEvent(input$reset_2D,output$vio_map<- renderLeaflet( {
    leaflet() %>%setView(-73.98, 40.75, zoom = 10)%>%
      addProviderTiles("Esri")
    
      }))
  
  
  
  
  ##statistics summary
  output$bubble <- renderPlotly({

    bubble_plot(df = All_vio ,cuisine_style = input$cuisine_style_buble , num_top = input$top_number_bubble )

  })
  
  output$bar <- renderPlotly({
    
    bar_plot(df = All_vio,cuisine_style = input$cuisine_style_bar , num_top = input$top_number_bar)
    
  })

  output$box<- renderPlotly({
    
    box_plot(df = All_vio,cuisine_style = input$cuisine_style_box)
    
  })
  
  output$hist<- renderPlotly({
    
    hist_plot(df = All_vio,score = input$Score_hist)
    
  })

}

