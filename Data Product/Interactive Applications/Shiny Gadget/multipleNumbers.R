library(shiny)
library(miniUI)

multipleNumbers <- function(number1, number2) {
  
  ui <- miniPage(
    
    gadgetTitleBar("Multiple two numbers"),
    
    miniContentPanel(
      
      selectInput("num1", "First Number", choices = number1),
      
      selectInput("num2", "Second Number", choices = number2)
    )
  )
  
  server <- function(input, output, session) {
    
    observeEvent(input$done, {
      
      num1 <- as.numeric(input$num1)
      num2 <- as.numeric(input$num2)
      stopApp(num1*num2) # return the value
    })
  }
  
  runGadget(ui, server)
}