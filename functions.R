
####plot function 

#### bubble plot  violation description for each/all cusine style
bubble_plot <- function(df, cuisine_style, num_top) {
  if (cuisine_style == "ALL") {
    df<- df%>%na.omit%>%count(VIOLATION.DESCRIPTION)%>%top_n(num_top)%>%rename(vio=VIOLATION.DESCRIPTION, freq = n)
  }
  else {
    df<- df%>% filter(CUISINE.DESCRIPTION ==cuisine_style )%>%count(VIOLATION.DESCRIPTION)%>%top_n(num_top)%>%rename(vio=VIOLATION.DESCRIPTION, freq = n)
    
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
    df<-df%>%distinct(CAMIS,.keep_all = TRUE)%>%
      count(DBA,sort = TRUE)%>%
      mutate(DBA = fct_reorder(DBA, n))%>%
      ungroup()%>%
      top_n(num_top)
  }
  else {
    df<-df%>%distinct(CAMIS,.keep_all = TRUE)%>%filter(CUISINE.DESCRIPTION==cuisine_style)%>%
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
    df<- df%>% filter(CUISINE.DESCRIPTION ==cuisine_style )
    
  }
  p<-plot_ly( data =df , x = ~GRADE, y = ~SCORE, color = ~GRADE,type = "box")
  
  return(p)
  
}

##### hist plot 
hist_plot<- function(df,score){

  df<- df%>%filter(SCORE <= score & SCORE>=score-10)
  
  f <- list(
    family = "Courier New, monospace",
    size = 18,
    color = "#7f7f7f"
  )
  x <- list(
    title = "",
    titlefont = f
  ) 
  
  p <- plot_ly(data = df,x = ~CUISINE.DESCRIPTION,alpha = 0.6,type = "histogram")%>%layout(xaxis = x)  
  return(p)
  
}



### 2D map
show_vio_map <- function(df,cuisine_style,grade,rest_search,borough,zipcode) { 
  
  if (!is.null(rest_search)) {
    
    rest_search<- toupper(rest_search)
    
    df<- df%>%filter(str_detect(string = DBA, pattern = rest_search) )
  }
  
  
  if (borough!= "All") {
    df<- df%>%filter(BORO == borough)
  }
  
  if (cuisine_style != "All") {
    df<- df%>%filter(CUISINE.DESCRIPTION== cuisine_style)
  }
  

  if (grade!="NONE") {
    df<- df%>%filter(GRADE==grade)
  }
  
  
  if(!is.null(zipcode)){
    df<- df%>%filter(str_detect(string = ZIPCODE,pattern = zipcode))
  }

  ##get unique rest with number of violaiton counts
  uniq_rest<- df %>%group_by(CAMIS)%>%count()%>%rename(num_violatinos = n)
  
  ##join back
  uniq_style <- df %>% inner_join(uniq_rest,by = "CAMIS")%>%distinct(CAMIS,.keep_all = TRUE)
  
  ##find out top desc for a unique rest
  Top_1_desc<- df%>%group_by(CAMIS)%>%count(VIOLATION.DESCRIPTION,sort = TRUE)%>%top_n(1)%>%distinct(CAMIS,.keep_all = TRUE)%>%rename(num_desc = n)
  
  ##join desc
  uniq_style<- uniq_style%>%select(-VIOLATION.DESCRIPTION)%>%inner_join(Top_1_desc,by="CAMIS")
  
  
  m = leaflet(uniq_style) %>%setView(-73.98, 40.75, zoom = 10)%>%
    addProviderTiles("Esri") %>% 
    #addTiles()%>%
    addMarkers(lng = uniq_style$lon,lat = uniq_style$lat,
               popup=paste(uniq_style$DBA,"<br>","***Top Violation***Counts:",uniq_style$num_desc,"<br>",uniq_style$VIOLATION.DESCRIPTION),
               clusterOptions = markerClusterOptions())
  
  return(m)
  
}

