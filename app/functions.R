
####plot function 

#########################################################################
########################## Not Using##################
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
    layout(title = paste('Top','violation description'),
           xaxis = list(showgrid = FALSE),
           yaxis = list(showgrid = FALSE))
  return(p)
  
}
#########################################################################

###bar for violation description for each/all cusine style
bar_vio_plot <- function(df,cuisine_style,num_top,critical_flag) {
  
  if (cuisine_style == "ALL") {
  df<-df%>%filter(CRITICAL.FLAG==critical_flag)%>%group_by(VIOLATION.CODE)%>%
    count(VIOLATION.DESCRIPTION)%>%ungroup()%>%slice(1:num_top)%>%mutate(VIOLATION.CODE = fct_reorder(VIOLATION.CODE, desc(n)))
  
  
    }
  else {
    df<-df%>%filter(CRITICAL.FLAG==critical_flag & CUISINE.DESCRIPTION==cuisine_style)%>%group_by(VIOLATION.CODE)%>%
      count(VIOLATION.DESCRIPTION)%>%ungroup()%>%slice(1:num_top)%>%mutate(VIOLATION.CODE = fct_reorder(VIOLATION.CODE, desc(n)))
  
    }
  
  ##str_wrap!! 666
 p<- plot_ly(data=df, x = ~VIOLATION.CODE, y = ~n ,type = 'bar',hovertext = str_wrap( df$VIOLATION.DESCRIPTION,width = 25))%>%
    layout(title = "Top Violation Description",
           xaxis = list(title = "Violation Code"),
           yaxis = list(title = "Counts"))
  
  return(p)
  
}




#### bar chart violation restaurant for each/all cusine style
bar_plot <- function(df, cuisine_style, num_top){
  
  
  ##num of vio for distinct store
  num_per_store_vio<-df%>%group_by(CAMIS)%>%count()%>%rename(num_vio_per_store = n)
  
  ##join num of vio per store back 
  df<-df%>%inner_join(num_per_store_vio,by = "CAMIS")
  ##total num of vio per brand
  total_num_vio_per_brand<- df%>%group_by(DBA)%>%summarise(sum(num_vio_per_store))
  total_num_vio_per_brand<-total_num_vio_per_brand%>%rename(num_vio_per_store=`sum(num_vio_per_store)`)
  
  
  ## get distinct store and count num of the same brand and SCALE IT!!
  num_per_brand <- df%>%distinct(CAMIS,.keep_all = TRUE)%>%count(DBA)%>%
    rename(num_per_brand = n)%>%mutate(num_per_brand = as.numeric(ceiling(abs(scale(num_per_brand)))))
  
  
  ##join back 
  total_num_vio_per_brand<-total_num_vio_per_brand%>%inner_join(num_per_brand,by = "DBA")
  
  ## division
  total_num_vio_per_brand$store_div_brand <- total_num_vio_per_brand$num_vio_per_store/total_num_vio_per_brand$num_per_brand
  
  total_num_vio_per_brand<-total_num_vio_per_brand%>%
    mutate(store_div_brand = round(num_vio_per_store/num_per_brand))%>%select(DBA,store_div_brand)
  
  df<- df%>%inner_join(total_num_vio_per_brand,by = "DBA")%>%
    select(DBA,store_div_brand,CUISINE.DESCRIPTION)%>%distinct(DBA,.keep_all = TRUE)
  
  
  

  if (cuisine_style == "ALL") {
    
    df<- df%>%arrange(desc(store_div_brand))%>%slice(1:num_top)%>%mutate(DBA = fct_reorder(DBA, store_div_brand))
    
  }
  else {
    
    df<-df%>%filter(CUISINE.DESCRIPTION==cuisine_style)%>%
      arrange(desc(store_div_brand))%>%slice(1:num_top)%>%mutate(DBA = fct_reorder(DBA, store_div_brand))
    
  }
  
  p <-  plot_ly(data=df, x = ~store_div_brand, y = ~DBA ,type = 'bar', orientation = 'h')%>%
    layout(title = "Top Violation Restaurant",
             xaxis = list(title = "counts"),
             yaxis = list(title = "restaurant"))
  
  return(p)
  
}


##### box plot Grade vs Score 
box_plot <- function(df, cuisine_style) {
  
  
  if (cuisine_style != "ALL") {
    df<- df%>% filter(CUISINE.DESCRIPTION ==cuisine_style )
    
  }
  p<-plot_ly( data =df , x = ~GRADE, y = ~SCORE, color = ~GRADE,type = "box")%>%
    layout(title = "Summary of Score for Each Grade",
           xaxis = list(title = "Grade"),
           yaxis = list(title = "Score"))
  
  
  return(p)
  
}

##### hist plot 
hist_plot<- function(df){ #,score){

 #df<- df%>%filter(SCORE <= score & SCORE>=score-10)
  
  f <- list(
    family = "Courier New, monospace",
    size = 18,
    color = "#7f7f7f"
  )
  x <- list(
    title = "",
    titlefont = f
  ) 
  df <- df%>%group_by(CUISINE.DESCRIPTION)%>%count()
  
  
  p <- plot_ly(data = df,x = ~CUISINE.DESCRIPTION,alpha = 0.6,type = "histogram")%>%layout(xaxis = x)%>%
  layout(title = "Top violation Cuisine Style",
         xaxis = list(title = "Cuisine Style"),
         yaxis = list(title = "Counts"))
  
  return(p)
  
}



### 2D map
show_vio_map <- function(df,cuisine_style,grade,rest_search,zipcode) { 
  
  if (!is.null(rest_search)) {
    
    rest_search<- toupper(rest_search)
    rest_search<-str_trim(gsub("\\s+", " ",rest_search))
    
    df<- df%>%filter(str_detect(string = DBA, pattern = rest_search) )
  }
  
  
 # if (borough!= "All") {
 #   df<- df%>%filter(BORO == borough)
 # }
  
  if (cuisine_style != "All") {
    df<- df%>%filter(CUISINE.DESCRIPTION== cuisine_style)
  }
  

  if (grade!="All") {
    df<- df%>%filter(GRADE==grade)
  }
  
  
  if(!is.null(zipcode)){
    df<- df%>%filter(str_detect(string = ZIPCODE,pattern = zipcode))
  }

  ##get unique rest with number of violaiton counts
  num_uniq_rest<- df %>%group_by(CAMIS)%>%count()%>%rename(num_violatinos = n)
  
  ##join back
  uniq <- df %>% inner_join(num_uniq_rest,by = "CAMIS")%>%distinct(CAMIS,.keep_all = TRUE)
  
  ##find out top desc for a unique rest
  Top_1_desc<- df%>%group_by(CAMIS)%>%count(VIOLATION.DESCRIPTION,sort = TRUE)%>%top_n(1)%>%distinct(CAMIS,.keep_all = TRUE)%>%rename(num_desc = n)
  
  ##join desc
  uniq<- uniq%>%select(-VIOLATION.DESCRIPTION)%>%inner_join(Top_1_desc,by="CAMIS")
  
  
  m = leaflet(uniq) %>%setView(-73.98, 40.75, zoom = 10)%>%
    addProviderTiles("Esri") %>% 
    #addTiles()%>%
    addMarkers(lng = uniq$lon,lat = uniq$lat,
               popup=paste(uniq$DBA,"<br> Contact:",uniq$PHONE,"<br>",uniq$Address,"<br>***Top Recent Violation***<br>",uniq$INSPECTION.DATE,"<br>",uniq$VIOLATION.DESCRIPTION),
               clusterOptions = markerClusterOptions())
  
  return(m)
  
}

