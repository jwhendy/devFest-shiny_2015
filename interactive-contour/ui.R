shinyUI(pageWithSidebar(
  
  headerPanel("Interactive Contour Plot"),
  
  sidebarPanel(
    
    selectInput("type", "Plot type", list("Lines only" = "lines", 
                                          "Colored lines" = "col_lines",
                                          "Tile map background" = "tile")),
    
    sliderInput(inputId = "x1", label = "x1 range",
                min = -3, max = 3, value = c(-1, 1), step = 0.5),
    
    sliderInput(inputId = "x1_hold", label = "x1 hold value",
                min = -3, max = 3, value = 0.2, step = 0.2),
    
    sliderInput(inputId = "x2", label = "x2 range",
                min = 0, max = 1, value = c(0.25, 0.75), step = 0.05),
    
    sliderInput(inputId = "x2_hold", label = "x2 hold value",
                min = 0, max = 1, value = 0.5, step = 0.05),
    
    sliderInput(inputId = "x3", label = "x3 range",
                min = 5, max = 20, value = c(7, 15), step = 1),
    
    sliderInput(inputId = "x3_hold", label = "x3 hold value",
                min = 5, max = 20, value = 10, step = 1),
    
    sliderInput(inputId = "x4", label = "x4 range",
                min = 0, max = 7, value = c(2, 5), step = 1),
    
    sliderInput(inputId = "x4_hold", label = "x4 hold value",
                min = 0, max = 7, value = 3, step = 0.5)), 
  
  mainPanel(
    tabsetPanel(
      tabPanel("Plot", plotOutput(outputId = "line_plot", width = "100%", height = 600)),
      tabPanel("Code", includeMarkdown("code.md"))
    )
)))