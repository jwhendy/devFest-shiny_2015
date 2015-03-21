library(shiny)
library(rCharts)
library(markdown)

shinyUI(fluidPage(  
  
  titlePanel("rCharts example"),  
  
  sidebarLayout(
    sidebarPanel(
    
    checkboxGroupInput(inputId = "type", label = "type",
                choices = list("constant" = "c",
                           "ramp" = "r"),
                selected = "c"),
    
    checkboxGroupInput(inputId = "rate", label = "rate",
                       choices = list("slow" = "slow",
                                      "med" = "med",
                                      "fast" = "fast",
                                      "fastest" = "fastest"),
                       selected = "slow")
    ),
                                          
  mainPanel(
    tabsetPanel(
      tabPanel("Plot", showOutput(outputId = "plot", "nvd3")),
      tabPanel("Code", includeMarkdown("code.md"))
    )
))))