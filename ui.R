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



##read data
df <- readr::read_csv("./data/cleaned_lon_lat.csv")
All_vio<-df%>%filter(ACTION!= "No violations were recorded at the time of this inspection."& CRITICAL.FLAG=="Critical")
uniq_style<-unique(All_vio$CUISINE.DESCRIPTION)
#No_vio <-df%>%filter(ACTION== "No violations were recorded at the time of this inspection.")%>%distinct(CAMIS,.keep_all = TRUE) %>% filter(!(CAMIS %in% unique(All_vio$CAMIS)))
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
  tabPanel("Violation",
           titlePanel("Violation Restaurant"),
      
                  sidebarLayout(
             
                      sidebarPanel(
                         h2("2D Explorer"),
                         
                         
                         textInput(inputId ='search2D', "Search your restaurant"),                         
                         
                         
                         selectInput(inputId = "style_2D",
                                     label  = "Cusinie Style",
                                     choices = c("All",uniq_style),
                                     selected ='All'),
                         
                         selectInput(inputId = "Borough_2D",
                                     label  = "Borough",
                                     choices = c("All","BRONX","BROOKLYN","MANHATTAN","QUEENS","STATEN ISLAND"),
                                     selected ='All'),

                        radioButtons(inputId = "grade_2D",
                                     label  =  "Grade of Restaurant",
                                     choices = c("A","B", "C", "Not Yet Graded","P","NONE"),
                                     selected ="NONE"),
                        textInput(inputId ='zip_2D', "ZIPCODE"), 
                        
                        actionButton(inputId = "result_2D", label = "Result"),
                        actionButton(inputId = "reset_2D", label = "Clear")
                        
                        
                        

           ),
           mainPanel(
             leafletOutput("vio_map",width = "100%", height = 600)
             
           )
           
  
           )
  ),
 

  
  
  
  
  
   

  ## end 2D Map tab
  
  ## Summary Statistics tab
                      tabPanel("Summary Statistics",
             
             
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
                                   #  selectInput(inputId = "cuisine_style_hist",
                                    #             label  = "choose the cuisine style",
                                     #            choices = c("ALL",uniq_style),
                                      #           selected ="ALL" ),
                                     
                                    # sliderInput(inputId = "top_number_hist",
                                           # label = "Select top ",
                                           # value = 20, min =5, max =50),
                                      # width = 3
                                    
                                    sliderInput(inputId = "Score_hist",
                                                label = "Score",
                                                value = 10,min = 0,max = 50)
                                     
                                   ),
                                   
                                   mainPanel(
                                     plotlyOutput("hist",height = "500px")
                                     
                                   )
                                   
                                 )
                        )
                        
                        
                        
                        
                        
                        
                      )
                      )
                      

             ## end Summary Statistics tab
             
)
