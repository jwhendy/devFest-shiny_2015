library(rCharts)
library(plyr)
library(shiny)

# read in data; only use one replicate to reduce size
data <- read.csv("./all-profiles-munged_2015-03-19.csv")
plot <- data[data$rep == 1, ]
plot$v <- round(plot$v / 25.4, 2)
plot$t <- round(plot$t, 2)

# rCharts can be really slow and this is a decent amount of data
# for each rate we have data for, extract only the (n * 250)th point
plot_list <- lapply(unique(plot$rate), function(rate) {
  
  temp <- plot[plot$rate == rate, ]
  temp <- temp[seq(1, nrow(temp), by = 250), ]
  return(temp)
  
})

plot <- do.call(rbind, plot_list)

# reduce to just the columns we need for the plot
plot <- plot[, c("type", "rate", "t", "v")]

# define which rates for which input$rate groups
rate_list <- list(slow = c("0.5", "0.7", "0.8", "1", "0.5-1-1.5-2", "0.5-2-3.5-5"),
                  med = c("2", "2.5", "3", "4", "5", "0.5-1-1.5-16", "0.5-1.5-2.5-16"),
                  fast = c("7.5", "10", "12.5", "15", "1-6-11-16", "2-4-6-8", "2-6-10-14"),
                  fastest = c("20", "25", "30", "35", "3-6-9-12", "4-5-6-7", "4-6-8-10", "4-8-12-16"))

shinyServer(function(input, output) {
  
  output$plot <- renderChart({
    
    # subset to just chosen rate groups, then by ramp/constant
    rates <- do.call(c, rate_list[input$rate])
    sub <- plot[plot$rate %in% rates, ]
    sub <- sub[sub$type %in% input$type, ]
    
    # make the plot
    n <- nPlot(v ~ t, data = sub, group = "rate", type = "lineChart")
    n$addParams(dom = "plot")
    return(n)
    
  })
})