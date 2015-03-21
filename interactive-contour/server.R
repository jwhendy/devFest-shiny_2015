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