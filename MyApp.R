library(ggplot2)
library(sqldf)
library(shiny)
library(shinydashboard)
library(dplyr)
library(leaflet)
require(gsubfn)
require(proto)
require(RSQLite)


#data1 <- read.csv("plant_type_location.csv",header = T,stringsAsFactors = F)
data <- read.csv("flower_season_location.csv",header = T,stringsAsFactors = F)
pdata <- read.csv("plant_type_location.csv",header = T,stringsAsFactors = F)


ui <- shinyUI(fluidPage(
  titlePanel("Bee Friendly Plants - Map Distibution"),
  
  # Create a new Row in the UI for selectInputs
  fluidRow(
    column(4,
           selectInput("season",
                       "Season:", choices = unique(as.character(data$Flowering_Time)), selected = "Summer")
           
           
    ),
    column(4,
           selectInput("plantType",
                       "Plant Type:",
                       c("All",
                         unique(as.character(pdata$Plant_type))))
    )
    
    
    
  ),
  
  #p("Below map shows the distribution of bee friendly plants across Victoria in different seasons")),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Plants Locations", leafletOutput("map11"))
      #tabPanel("Table Output", tableOutput('viewData'))
    )
  )))

server <- function(input, output) { 
  
  # Filter data based on selections
  subsetData <- reactive({
    
    
    if (input$season != "All") {
      data <- data[data$Flowering_Time == input$season,]
    }
    
    data
    #new_data <- data %>% filter(data$Flowering.Time_x == input$season & data$colour == input$color & data$Plant_type == input$plantType)
    return(data)
  })
  
  subset_plant_Data <- reactive({
    
    
    if (input$plantType != "All") {
      pdata <- pdata[pdata$Plant_type == input$plantType,]
    }
    
    pdata
    #new_data <- data %>% filter(data$Flowering.Time_x == input$season & data$colour == input$color & data$Plant_type == input$plantType)
    return(pdata)
  })
  
  
  
  
  output$map11 <- renderLeaflet({ 
    
    
    df = subsetData()
    
    plant_df = subset_plant_Data()
    
    new_df = data.frame(sqldf('SELECT a.Botanical_name, b.Plant_type, a.Flowering_Time, a.Common_name, a.Latitude, a.Longitude, COUNT(a.Botanical_name) AS B FROM df a, plant_df b where a.Botanical_name = b.Botanical_name GROUP BY a.Botanical_name order by B desc'))
    
    
    icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      #markerColor = pal[1:length(levels(as.factor(unique(df$Flowering_Time))))]
      if (input$season == "Winter") {markerColor = "blue"}
      else if (input$season == "Summer") {markerColor = "orange"}
      else if (input$season == "Autumn") {markerColor = "red"}
      else if (input$season == "Fall") {markerColor = "yellow"}
      else if (input$season == "Spring") {markerColor = "green"}
    )
    
    
    rmap = leaflet(new_df) %>% addTiles() %>% addAwesomeMarkers(~Longitude, ~Latitude, icon=icons, popup = paste("Plant Type :", new_df$Plant_type,"<br>","Flowering Season :", new_df$Flowering_Time,"<br>","Plant Name :", new_df$Botanical_name,"<br>","Common Name :", new_df$Common_name,"<br"))
    
    rmap
  })
  
}


shinyApp(ui, server)
