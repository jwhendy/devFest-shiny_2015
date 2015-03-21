library(shiny)
# page format
shinyUI(pageWithSidebar(
  # title
  headerPanel("Hello Shiny!"),
  
  sidebarPanel(
    # user inputs go here
  ),
  
  mainPanel(
    plotOutput("plot") # what you're going to output, e.g. a plot
  )
))
