library(plotly)
library(shiny)
library(shinydashboard)
library(dplyr)
library(jsonlite)
library(httr)
library(tidyr)
library(DT)
library(ggplot2)
library(plotly)
library(gtools)
library(reshape2)

wearableChart <- function(DATE1,DATE2,CITY){
  
  url <- paste0("http://159.89.172.254:8090/feedback?from=",as.character(DATE1),"&to=",as.character(DATE2),"&city=",as.character(CITY))
  
  response<-GET(url)
  wr_raw <- content(response, "text")
  wr_json <- fromJSON(wr_raw, flatten = TRUE)
  wrChart <- data.frame(wr_json) 
  wrResult <- wrChart %>%
    mutate(type = paste0(gotHairnet,gotHelmet))%>%
    select(city,orderDate,type)
  a<-table(wrResult$type)
  a<-as.data.frame(a)
  p<-plot_ly(a, labels = ~Var1, values = ~Freq, type = 'pie') %>%
    layout(title = paste0('Pie Chart of Wearables for ',CITY),
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  return(p)
}

ui <- dashboardPage(
  dashboardHeader(title = "Wearable Tracker"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Helmet and Cap", tabName = "helmet_cap", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    
    tabItems(
      tabItem(
        tabName = "helmet_cap",
        
        fluidRow(
          box(width = 4,dateRangeInput('wearabledateRange',label = 'Date range input: YYYY-MM-DD',
                                       start =Sys.Date()-2,end = Sys.Date())),
          box(width = 4,selectInput("city", "Choose a City:",
                                    choices = c("Bangalore","Gurgaon","Hyderabad","Chennai","Mysore","Vijayawada",
                                                "Vishakapatnam","Kochi",
                                                "Coimbatore","Trichy",
                                                "Madurai","Indore",
                                                "Patna","Bhopal"))),
          actionButton("button","Update Pie Chart")
          ),
        
        fluidRow(
          box(width = 12, heigth = "500px", plotlyOutput("pie_chart")))
        
        )    
      )
    )
    
  )

server <- function(input, output) {
    
  observeEvent(input$button,{
    pc <- wearableChart(input$wearabledateRange[1],input$wearabledateRange[2],input$city)
    output$pie_chart <-renderPlotly({pc})
  })
  
  }

shinyApp(ui, server)