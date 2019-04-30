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
library("shinythemes")


#load plot functions
source("functions.R")


##read data & clean
df <- readr::read_csv("./data/grade_lon_lat.csv")

###violation stores
All_vio<-df%>%filter(ACTION!= "No violations were recorded at the time of this inspection.")
Crit_vio <-All_vio%>%filter(CRITICAL.FLAG =="Critical")


## Server function
server<- function(input, output){
  
  ##Introduction
  output$blankspace = renderUI({
    HTML("")
  })
  output$text = renderUI({
    HTML("<br/><br/><br/>
New York City is a heaven for food-loving people, who will find many opportunities to sample cuisine from all cultures and at every price level, from street vendors to the finest high-end restaurants.However, the major concern is the food safety and health issues while choosing a restaurant.
With our app,users will inspect each restaurant of their interest and make their wise decision when they consider dining out in New York City.<br/><br/><br/><br/>Xiaowo Sun, Xin Xia")
  })

## 2D map
  output$vio_map<-renderLeaflet({
    
   leaflet() %>%setView(-73.98, 40.75, zoom = 10)%>%
      addProviderTiles("Esri")
    
  })
  
  observeEvent(input$result_2D,output$vio_map<- renderLeaflet({
    
    
    show_vio_map(df = Crit_vio,cuisine_style = input$style_2D,grade = input$grade_2D,rest_search = input$search2D,zipcode = input$zip_2D)
    
    
  }))
  
  observeEvent(input$reset_2D,output$vio_map<- renderLeaflet( {
    leaflet() %>%setView(-73.98, 40.75, zoom = 10)%>%
      addProviderTiles("Esri")
    
      }))
  
  
  ##statistics summary
  
  output$bar_desc<- renderPlotly({
    
    bar_vio_plot(df = All_vio,cuisine_style = input$cuisine_style_bar_desc,num_top = input$top_number_bar_desc,critical_flag = input$bar_desc_criti)
    
  })
  
  output$bar <- renderPlotly({
    
    bar_plot(df = All_vio,cuisine_style = input$cuisine_style_bar , num_top = input$top_number_bar)
    
  })

  output$box<- renderPlotly({
    
    box_plot(df = All_vio,cuisine_style = input$cuisine_style_box)
    
  })
  
###############  
####Not Using#### 
##  output$hist<- renderPlotly({
    
#    hist_plot(df = All_vio)#,score = input$Score_hist)
    
 # })

}

#output$bubble <- renderPlotly({

# bubble_plot(df = All_vio ,cuisine_style = input$cuisine_style_buble , num_top = input$top_number_bubble )

#})
