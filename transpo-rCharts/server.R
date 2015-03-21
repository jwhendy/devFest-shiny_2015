library(shiny)
library(rCharts)

data <- read.csv("public-transpo.csv")

# need a color to use when none is chosen
data$none <- rep("", nrow(data))

shinyServer(function(input, output) {
    
  output$plot <- renderChart({
    
    # initialize ggplot; aes_string is necessary since I don't know how to 
    # pass objects from the input object. Instead, intpu$var returns a string
    # so we use the handy aes_string() method instead of regular 'ol aes()
    
    p <- nPlot(y = input$y, x = input$x, data = data[1:100, ], group = "mode", 
               type = "scatterChart")
    p$addParams(dom = "plot")
    
    # there were issues using range(min, max) in scale_size below, so I opted to
    # just run a check to see if no size variable had been selected.
    # If not, we just use a constant value.
    #if(input$size == "none") {
    #  p <- p + geom_point(aes_string(colour = input$color), size = 4)
    #}
    
    # if size is not "none", add the aesthetic
    #else {
    #  p <- p + geom_point(aes_string(size = input$size,
    #                                 colour = paste0("as.factor(", input$color, ")")))
    #}
    
    # ggplot takes trings for transformations, so we can handily just pass
    # the appropriate name right to the scale!
    
    #p$xAxis(tickFormat = "#!function (x) {    
    #tickformat = [1,10,100,1000,10000,'100k'];
    #return tickformat[x];}!#")    

    
    #p <- p + scale_x_continuous(trans = input$x_trans)
    #p <- p + scale_y_continuous(trans = input$y_trans)
    
    # the range() option is nice so we can make sure the min and max bubble
    # sizes are reasonable. We can also apply a transformation here.
    #p <- p + scale_size_continuous(input$size,
    #                               trans = input$size_trans, range = c(5, 25))
    
    # just unifying the color name so we don't get as.factor()
    #p <- p + scale_colour_discrete(input$color)
    
    # some theme options to pretty things up and make it more readable
    #p <- p + theme_bw() + theme(text = element_text(size = 18))
    
    return(p)
    
  })
})
