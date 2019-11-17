library(shiny)

# incoporate other code and data into the app
source("helpers.R")
counties <- readRDS("data/counties.rds")
library(maps)
library(mapproj)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  titlePanel("censusVis"),
  
  sidebarLayout(
    
    sidebarPanel(
           helpText("Create demographic maps with information from 
           2010 USA Census"),
           
           selectInput("var", strong("Choose a variable to display"),
                       choices = list("Percent White" ,
                                      "Percent Black" ,
                                      "Percent Asian" ,
                                      "Percent Hispanic"),
                       selected = "Percent White"),
           
           sliderInput("range", strong("Range of Interest"),
                       min = 0, max = 100, value = c(0,100))),
    
    mainPanel(
      plotOutput("map")
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  
  
  output$map <- renderPlot({
    args <- switch(input$var,
                   "Percent White" = list(counties$white, "darkgreen", "% White"),
                   "Percent Black" = list(counties$black, "black", "% Black"),
                   "Percent Hispanic" = list(counties$hispanic, "darkorange", "% Hispanic"),
                   "Percent Asian" = list(counties$asian, "darkviolet", "% Asian"))
    args$min <- input$range[1]
    args$max <- input$range[2]
    
    do.call(percent_map, args)
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
