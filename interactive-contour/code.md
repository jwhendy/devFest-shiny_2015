This is the result of inquiring about [creating a facetted contour plot on StackOverflow](http://stackoverflow.com/questions/17958730/faceting-a-set-of-contour-plots-in-ggplot-r). The gist of the situation is:

- One is fitting a model using four input variables and a response
- Once the model is fitted (or trained, perhaps, if using machine learning or the like), you want to create a grid of contour plots to examine the effect of your input variable interactions on the response
- To do this, you need a grid of plots in which two inputs vary over a range while the other two variables remain fixed
- Using `expand.grid` turned out to be tricky, as if you simply use it on all combinations of your `min`, `max` (range), and hold/fixed values, you end up with a lot of redundant combinations

The question on SO led to a successful answer (for a static visualization), so I'm reproducing the gist of what I'm doing here. I created sliders to set the `min` and `max` values for the ranges; these are used whenever a variable is featured on a contour plot axis. The hold values are used anywhere the variable is *not* pictured. Unique combinations are fed into the `predict()` function for the model and then combined into one big data set for plotting after adding a column used for facetting.

Please install the following packages if you wish to reproduce: `shiny`, `ggplot2`, and `markdown` (if including a `code` tab in your shiny app).

ui.R
===

```{r}
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
```

server.R
===

```{r}
library(shiny)
library(ggplot2)
library(markdown)
library(plyr)

shinyServer(function(input, output) {
  set.seed(42)
  x1 <- rnorm(20)
  x2 <- runif(20)
  x3 <- rpois(20,10)
  x4 <- rexp(20)
  y <- 10 + 2*x1 + 3*x2^2 + 4*x3 +5*x4 + rnorm(20, sd=0.1)

  dat <- data.frame(x1, x2, x3, x4, y)
  
  # vector of variable names for easy data.frame column naming later
  var_names <- c("x1", "x2", "x3", "x4")
  
  # possible combinations of four variables
  combis <- combn(1:4, 2)
  combis <- rbind(combis, combis[, 6:1])
  
  # steps to generate for the contour using `rep(min, max, length.out = n)`
  n <- 50
  
  #fit the model
  fit <- lm(y~x1+I(x2^2)+x3+x4, data=dat)
  
  output$line_plot <- renderPlot({
    
    # get values based on UI input for ranges
    mins_maxs <- data.frame(x1 = c(min(input$x1), max(input$x1)),
                            x2 = c(min(input$x2), max(input$x2)),
                            x3 = c(min(input$x3), max(input$x3)),
                            x4 = c(min(input$x4), max(input$x4)))
    
    # also get the current hold values
    holds <- c(input$x1_hold, input$x2_hold, input$x3_hold, input$x4_hold)
        
    # create simulated new data to feed into model
    # for each two variables used in a given contour, we take their min/max values
    # for the other two variables not shown in the contour, we need their hold values
    sim_data <- list()
    sim_data <- lapply(seq_len(ncol(combis)), function (i) {
      sim_data[[i]] <- expand.grid(seq(mins_maxs[1, combis[1, i]], 
                                       mins_maxs[2, combis[1, i]], length.out = n),
                                   seq(mins_maxs[1, combis[2, i]], 
                                       mins_maxs[2, combis[2, i]], length.out = n),
                                   holds[combis[3, i]],
                                   holds[combis[4, i]])
    } )
    
    # we create a new data frame for the data set we want to plot
    # for each contour of interest, we want the first two columns, as the second two
    # were only created for the sake of feeding into our fit lm 
    # we'll also create a vector for facetting using paste()
    plot_data <- list()
    plot_data <- lapply(1:ncol(combis), function(i) {
      plot_data[[i]] <- sim_data[[i]][, 1:2]
      plot_data[[i]]$var1 <- rep(paste("x = ", var_names[combis[1, i]], sep = ""),
                                 nrow(plot_data[[i]]))
      plot_data[[i]]$var2 <- rep(paste("y = ", var_names[combis[2, i]], sep = ""), 
                                 nrow(plot_data[[i]]))
      return(plot_data[[i]])
    } )
    
    # now we rename the columns of plot_data
    plot_data <- lapply(1:length(plot_data), function(i) {
      names(plot_data[[i]]) <- c("x", "y", "var1", "var2")
      return(plot_data[[i]])
    } )
    
    # similarly, we need to re-name the sim_data columns so we can rbind them
    # and predict new values using our fit lm
    sim_data <- lapply(1:length(sim_data), function(i) {
      names(sim_data[[i]]) <- var_names[combis[, i]]
      return(sim_data[[i]])
    } )
    
    
    # collapse the separate lists into one data.frame
    sim_data <- do.call(rbind, sim_data)
    plot_data <- do.call(rbind, plot_data)
  
    # create a vector of predictions using sim_data
    plot_data$resp <- predict(fit, sim_data)
    
    plot_data$x <- round(plot_data$x, 5)
    plot_data$y <- round(plot_data$y, 5)
    
    hw <- ddply(plot_data, .(var1, var2), summarize,
                height = (max(y) - min(y))/n, width = (max(x) - min(x))/n,
                binwidth = (max(resp) - min(resp))/10)
    
    plot_data <- merge(plot_data, hw, by = c("var1", "var2"), all.x = T)
    
    p <- ggplot(plot_data, aes(x = x, y = y, z = resp))
    
    if(input$type == "lines") {
      p <- p + stat_contour(aes(binwidth = binwidth)) }
    
    if(input$type == "col_lines") {
      p <- p + stat_contour(aes(colour = ..level.., binwidth = binwidth))
      p <- p + scale_colour_gradient(low = "darkblue", high = "red")
    }
    if(input$type == "tile") {
      p <- p + geom_tile(aes(fill = resp, height = height, width = width))
      p <- p + stat_contour(aes(binwidth = binwidth), colour = "white")
      p <- p + scale_fill_gradient(low = "darkblue", high = "red")
    }
    
    p <- p + facet_wrap(var1 ~ var2, scales = "free", ncol = 3) + theme_bw()
    p <- p + scale_y_continuous(expand = c(0, 0))
    p <- p + scale_x_continuous(expand = c(0, 0))
    p <- p + theme(text = element_text(size = 15))
    
    print(p)
    
  }, height = 600)
  
  
})
```
