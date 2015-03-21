library(shiny)

shinyServer(function(input, output) {
  
  # general R code here: load libraries, set variables/functions/etc.
  
  # output$name has to match ui.R's plotOutput("name")
  output$plot <- renderPlot({
    
    # code to make a plot goes here
    
  })
})
