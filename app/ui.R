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

##read data
df <- readr::read_csv("./data/grade_lon_lat.csv")
All_vio<-df%>%filter(ACTION!= "No violations were recorded at the time of this inspection.")
uniq_style<-unique(All_vio$CUISINE.DESCRIPTION)


## UI Function

ui<- navbarPage(
  theme = shinytheme("united"),
  ##link to css.file
  
  #theme = "bootstrap2.css",
  
  ##Project Title
  "New York City Restaurant - Inspect with US",

  tabPanel("Home",
           htmlOutput("blankspace"),
           titlePanel("New York City Restaurant Inspection"),
           h4(htmlOutput("text")),
           htmlOutput("teammates")
           ),

  ## 2D Map tab
  tabPanel("2D map",
           titlePanel("Violation Restaurant"),
      
                  sidebarLayout(
             
                      sidebarPanel(
                         h2("2D Explorer"),

                         textInput(inputId ='search2D', "Search your Restaurant"),                         
                         
                       selectInput(inputId = "style_2D",
                                     label  = "Cuisine Style",
                                     choices = c("All",uniq_style),
                                     selected ='All'),
                         
                        # selectInput(inputId = "Borough_2D",
                                #     label  = "Borough",
                                  #   choices = c("All","BRONX","BROOKLYN","MANHATTAN","QUEENS","STATEN ISLAND"),
                                    # selected ='All'),

                        radioButtons(inputId = "grade_2D",
                                     label  =  "Grade of Restaurant",
                                     choices = c("A","B", "C","Grade Pending","All"),
                                     selected ="All"),
                        textInput(inputId ='zip_2D', "ZIPCODE"), 
                        
                        actionButton(inputId = "result_2D", label = "Result"),
                        actionButton(inputId = "reset_2D", label = "Clear")
                        
           ),
           mainPanel(
             leafletOutput("vio_map",width = "100%", height = "500px")
             
           )
           
  
           )
  ),

  ## end 2D Map tab
  
  ## Summary Statistics tab
                      tabPanel("Summary Statistics",
             
             
                      tabsetPanel(
                        
                        ##violation bar plot
                        tabPanel("Description",
                              #   titlePanel("Violation Description"),
                                 sidebarLayout(
                                   sidebarPanel(
                                     radioButtons(inputId = "bar_desc_criti",
                                                  label  =  "Critical or Not Critical",
                                                  choices = c("Critical","Not Critical"),
                                                  selected ="Critical"),
                                     
                                     selectInput(inputId = "cuisine_style_bar_desc",
                                                 label  = "choose the cuisine style",
                                                 choices = c("ALL",uniq_style),
                                                 selected ="ALL" ),
                                     
                                     sliderInput(inputId = "top_number_bar_desc",
                                                 label = "Select top ",
                                                 value = 20, min =5, max =50),
                                     width = 3
                                   ),
                                   
                                   mainPanel(
                                     plotlyOutput("bar_desc",height = "500px")

                                   )
                                   
                                 )
                        ),
                        
                        ##bar chart
                        tabPanel("Restaurant",
                                # titlePanel("Top Violation Restaurant"),
                                 sidebarLayout(
                                   sidebarPanel(

                                     
                                     selectInput(inputId = "cuisine_style_bar",
                                                 label  = "choose the cuisine style",
                                                 choices = c("ALL",uniq_style),
                                                 selected ="ALL" ),
                                     
                                     sliderInput(inputId = "top_number_bar",
                                                 label = "Select top ",
                                                 value = 10, min =5, max =20),
                                     width = 3
                                   ),
                                   mainPanel(
                                     plotlyOutput("bar",height = "500px")
                                     
                                   )
                                   
                                 )
                        ),
                        
                        
                        ##box plot
                        tabPanel("Score",
                               #  titlePanel("Score of Grade"),
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput(inputId = "cuisine_style_box",
                                                 label  = "choose the cuisine style",
                                                 choices = c("ALL",uniq_style),
                                                 selected ="ALL" ),
                                     
                                     #sliderInput(inputId = "top_number_box",
                                          #       label = "Select top ",
                                          #       value = 20, min =5, max =50),
                                     width = 3
                                     
                                   ),
                                   
                                   mainPanel(
                                     plotlyOutput("box",height = "500px")
                                     
                                   )
                                   
                                 )
                        )
                       # tabPanel("Country",
                               #  titlePanel("Violation Cuisine Style"),
                                 #sidebarLayout(
                                   #sidebarPanel(
                                    #sliderInput(inputId = "Score_hist",
                                         #       label = "Score",
                                        #        value = 10,min = 0,max = 50)
                                   #),
                                  # mainPanel(
                                   #  plotlyOutput("hist",height = "700px")
                                     
                                  # )
                                   
                                 #)
                       # )
    
                      )
                      )
                      

             ## end Summary Statistics tab
             
)
