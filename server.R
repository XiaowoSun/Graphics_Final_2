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
                        intersect(installed.packages()[,1], 
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



##read data & clean
df <- readr::read_csv("data_with_lon_lat.csv")

###critical violation stores
All_vio<-df%>%filter(ACTION!="Not yet received an inspection" & ACTION!= "No violations were recorded at the time of this inspection."& `CRITICAL FLAG`=="Critical")

#### unique No violation stores
No_vio <-df%>%filter(ACTION== "No violations were recorded at the time of this inspection.")%>%distinct(CAMIS,.keep_all = TRUE) %>% filter(!(CAMIS %in% unique(All_vio$CAMIS)))
uniq_DBA<- unique(No_vio$DBA)


### unique critical violation stores
unique_vio<- All_vio%>%distinct(CAMIS,.keep_all = TRUE)

uniq_style<- unique_vio$`CUISINE DESCRIPTION`




####plot function 
#### bubble plot  violation description for each/all cusine style
bubble_plot <- function(df, cuisine_style, num_top) {
  if (cuisine_style == "ALL") {
      df<- df%>%count(`VIOLATION DESCRIPTION`)%>%top_n(num_top)%>%rename(vio=`VIOLATION DESCRIPTION`, freq = n)
  }
  else {
    df<- df%>% filter(`CUISINE DESCRIPTION` ==cuisine_style )%>%count(`VIOLATION DESCRIPTION`)%>%top_n(num_top)%>%rename(vio=`VIOLATION DESCRIPTION`, freq = n)
    
  }
  
  
  bubble_size <- (df$freq-min(df$freq))/(max(df$freq)-min(df$freq))
  
  
  p<-  plot_ly(data = df,x = ~vio, y = ~freq, type = 'scatter', mode = 'markers',color = ~freq,colors = 'Reds',marker = list(size = ~bubble_size*75, opacity = 0.77)) %>%
    layout(title = paste('Top','violation desc'),
           xaxis = list(showgrid = FALSE),
           yaxis = list(showgrid = FALSE))
  
  return(p)
  
}


#### bar chart violation restaurant for each/all cusine style

bar_plot <- function(df, cuisine_style, num_top){
  
  if (cuisine_style == "ALL") {
    df<-df%>%select(DBA)%>%
      count(DBA,sort = TRUE)%>%
      mutate(DBA = fct_reorder(DBA, n))%>%
      top_n(num_top)
  }
  else {
    df<-df%>%filter(`CUISINE DESCRIPTION`==cuisine_style)%>%
      select(DBA)%>%
      count(DBA,sort = TRUE)%>%
      mutate(DBA = fct_reorder(DBA, n))%>%
      top_n(num_top)
    
  }
  
  
 p <-  plot_ly(data=df, x = ~n, y = ~DBA ,type = 'bar', orientation = 'h')
  
  return(p)
  
  
}

##### box plot Grade vs Score 
box_plot <- function(df, cuisine_style) {
  
  
  if (cuisine_style != "ALL") {
df<- df%>% filter(`CUISINE DESCRIPTION` ==cuisine_style )
    
  }

p<-plot_ly( data =df , x = ~GRADE, y = ~SCORE, color = ~GRADE,type = "box")
  
return(p)
  
}


##### hist plot 
hist_plot<- function(df,cuisine_style){
 if (cuisine_style != "ALL") {

    df<- df%>% filter(`CUISINE DESCRIPTION`==cuisine_style)
    
  }
  
  f <- list(
    family = "Courier New, monospace",
    size = 18,
    color = "#7f7f7f"
  )
  x <- list(
    title = "",
    titlefont = f
  ) 
  
  

  p <- plot_ly(data = df,x = ~`VIOLATION DESCRIPTION`,alpha = 0.6,type = "histogram")%>%layout(xaxis = x)  
  


return(p)



}






### 2D map
show_vio_map <- function(df,cuisine_style,restaurant,score,garde) { 
  
  
  df<-df%>%filter(str_detect(string = `CUISINE DESCRIPTION`, pattern = cuisine_style) )%>%select(CAMIS,DBA,BUILDING,STREET,ZIPCODE,PHONE,SCORE,`CUISINE DESCRIPTION`,`VIOLATION DESCRIPTION`,lon,lat)
  
  df<- df%>%filter(SCORE<=score )
  
  
    #rest_each_count<- df%>%filter(DBA == restaurant)%>%group_by(CAMIS)%>%count()
  #uniq_rest <- df%>%filter(DBA == restaurant)%>%inner_join(rest_each_count,by = "CAMIS")%>%distinct(CAMIS,.keep_all = TRUE)
  
  style_each_count<- df %>%group_by(CAMIS)%>%count()
  uniq_style <- df %>% inner_join(style_each_count,by = "CAMIS")%>%distinct(CAMIS,.keep_all = TRUE)
  
  
  desc<- df%>%group_by(CAMIS)%>%count(`VIOLATION DESCRIPTION`,sort = TRUE)%>%top_n(1)%>%distinct(CAMIS,.keep_all = TRUE)

  
  m = leaflet(uniq_style) %>%setView(-73.98, 40.75, zoom = 10)%>%
    addProviderTiles("Esri") %>% 
    #addTiles()%>%
    addMarkers(lng = uniq_style$lon,lat = uniq_style$lat,
               popup=paste(uniq_style$DBA,"Vio Count:",uniq_style$n,"<br>","desc:",desc$`VIOLATION DESCRIPTION`),
              clusterOptions = markerClusterOptions())
    
    
  return(m)
  
}










## Server function

server<- function(input, output){
  
  ##Introduction
  output$blankspace = renderUI({
    HTML("<br/><br/><br/><br/><br/><br/><br/><br/>")
  })
  output$text = renderUI({
    HTML("<br/><br/><br/>Our project looks into the trade of coffee, tea, chocolate, cocoa and spices<br/>
         between the United States and the rest of the world<br/><br/><br/><br/> Xiaowo Sun, Xin Xia")
  })


  
  ##2D map

  output$vio_map <- renderLeaflet(
    
    show_vio_map(df = All_vio,cuisine_style = input$style_2D,restaurant = input$rest_2D,score = input$Score_2D,garde = input$Grade_2D)
    
  )
  
  
  
  
  

  ##critical analysis
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
    
    hist_plot(df = All_vio,cuisine_style = input$cuisine_style_hist)
    
  })

}

