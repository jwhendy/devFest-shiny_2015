**Note:**

The main things I'm looking for input on are all the checks for is.null and
having to set all of these initialized values in the event that
the input object *is* null so that R can do something other than
flash a red error on the screen while it doesn't its first scan
and then pop out the result on the second go-around
I've commented the parts I'm curious about with respect to efficiency
so don't worry about figuring it all out; there's only a few sections



**ui.R**

```{r}
# Pretty happy with ui.R, so unless there's a glaring issue,
# I'd skip it for the sake of your precious time!

library(shiny)
library(markdown)

load("./plans.rda")

shinyUI(pageWithSidebar(
  
  headerPanel("2014 Benefit Plan Comparison"),
  
  sidebarPanel(
    
    selectInput(inputId = "class", label = "Choose plan type:",
                list("Employee only" = "emp", "Employee and spouse" = "emp_spouse",
                     "Employee and child" = "emp_child", "Employee and family" = "emp_fam")),
    
    conditionalPanel(
      condition = "input.class != 'emp'",
      sliderInput(inputId = "members", label = "Number of individuals on plan:",
                  min = 1, max = 15, value = 1, step = 1)
    ),
    
    sliderInput(inputId = "max_pred", label = "Maximum for expense range:",
                min = 5000, max = 100000, value = 5000, step = 5000),
    
    includeMarkdown("./best-worst.md"),
    
    uiOutput("sliders")
    
  ),
  
  mainPanel(
    tabsetPanel(  
      tabPanel("Side by Side", 
               plotOutput(outputId = "main_plot", width = "100%"),
               htmlOutput(outputId = "heading_best"),
               tableOutput(outputId = "bar_summ_best"),
               htmlOutput(outputId = "heading_worst"),
               tableOutput(outputId = "bar_summ_worst"),
               textOutput(outputId = "test")),
      tabPanel("ui.R", includeMarkdown("./code.md"))
      #tabPanel("Monthly cash flow (averaged)", tableOutput(outputId = "summary")),
      #tabPanel("Monthly cash flow (all in January)", tableOutput(outputId = "summary"))
    )
  )))
```

**server.R**
```{r}
library(shiny)
library(ggplot2)
library(markdown)


# this function creates a unique binary outcome depending on if the
# various plan limits are met. It depends on a value for `expenses`,
# which is defined below, but `expenses` depends on reactive inputs, so on
# the first run, it doesn't exist and thus I have to dance a bit

condition <- function(expenses, class) {
  
  compare <- plans[plans$class == class, ]

# initialize values in case expenses doesn't exist
  exp_ind <- 0
  exp_rem <- 0
  
# if it doesn't, I have to create a data frame like would *would* be 
# returned, but store 0's in all the columns that would be calculated
  if(is.null(expenses)) {
    result <- cbind(compare[, c("ded_ind", "ded_tot", "oop_ind", "oop_tot", "prem", "hsa")],
                    exp_ind, exp_rem, bin = 0)
    names(result)[ncol(result)] <- "bin"
    return(result)
  }
  
  else{
    exp_ind <- max(expenses)
    exp_rem <- sum(expenses[-which(expenses == exp_ind)[1]])
    test_case <- c(rep(c(exp_ind, exp_rem, exp_ind + exp_rem), each = 2))
    test_case <- rbind(test_case, test_case, test_case)
    
    limits <- cbind(compare$ded_ind, compare$exp_max_ind,
                    compare$ded_ind, compare$exp_max_ind, 
                    compare$ded_tot, compare$exp_max_tot)
    
    result <- cbind(compare[, c("ded_ind", "ded_tot", "oop_ind", "oop_tot", "prem", "hsa")],
                    exp_ind, exp_rem, (test_case > limits) %*% (2^(0:5)))
    names(result)[ncol(result)] <- "bin"
    return(result)
  }
}

# Don't bother with this, but if you're interested, it's either and awful, awful way
# to translate a conditional check to an element in a "truth table" of outcomes,
# or it's really, really clever. I haven't decided.

map_funcs <- list(
  "0" = function(binary) { binary$exp_ind + binary$exp_rem }, 
  "1" = function(binary) { binary$ded_ind + (0.1* (binary$exp_ind - binary$ded_ind)) + binary$exp_rem }, 
  "4" = function(binary) { binary$exp_ind + binary$exp_rem }, 
  "16" = function(binary) { binary$ded_tot + (0.1 * (binary$exp_ind + binary$exp_rem - binary$ded_tot)) },
  "17" = function(binary) { binary$ded_ind + (0.1* (binary$exp_ind - binary$ded_ind)) + binary$exp_rem },
  "19" = function(binary) { binary$oop_ind + binary$exp_rem }, 
  "20" = function(binary) { binary$ded_tot + (0.1 * (binary$exp_ind + binary$exp_rem - binary$ded_tot)) }, 
  "21" = function(binary) { binary$ded_tot + (0.1 * (binary$exp_ind + binary$exp_rem - binary$ded_tot)) }, 
  "23" = function(binary) { binary$oop_ind + binary$ded_ind + (0.1 * (binary$exp_rem - binary$ded_ind)) },
  "28" = function(binary) { binary$ded_tot + (0.1 * (binary$exp_ind + binary$exp_rem - binary$ded_tot)) },
  "29" = function(binary) { binary$ded_tot + (0.1 * (binary$exp_ind + binary$exp_rem - binary$ded_tot)) },
  "48" = function(binary) { binary$oop_tot },   
  "51" = function(binary) { binary$oop_ind + binary$exp_rem }, 
  "55" = function(binary) { binary$oop_ind + binary$ded_ind + (0.1 * (binary$exp_rem - binary$ded_ind)) }, 
  "60" = function(binary) { binary$oop_tot }, 
  "61" = function(binary) { binary$oop_tot }, 
  "63" = function(binary) { binary$oop_tot }
)

load("./plans.rda")

# per your lead, I initialized some defaults for reactive input sliders
global_min <- 0
global_max <- 500
global_members <- 1

shinyServer(function(input, output) {
  
  output$sliders <- renderUI({
    
# I had some trouble here. If the plan changes from a multi-person person plan
# it's not enough to hide the slider since the number of members seems to stay
# in the object even though the sliders that change those values are hidden
# So, anytime the slider changes back to "emp" (employee only), I have to manually
# set the # of members to 1

    class <- input$class
    members <- input$members
    if(class == "emp") {
      members <- global_members
    }
    
    max_pred <- as.integer(input$max_pred)

# reasonably happy with this bit, based on your example
    individuals <- lapply(1:members, function(i) {
      
      input_name <- paste0("ind", i)
      label_name <- paste("Individual", i)
      
      existing_min <- isolate(input[[input_name]][1])
      existing_max <- isolate(input[[input_name]][2])
      
      if(is.null(existing_min)) {
        existing_min <- global_min
        existing_max <- global_max  
      }
      
      sliderInput(inputId = input_name, label = label_name, min = 0, max = max_pred,
                  value = c(existing_min, existing_max), step = 100)
      
    })
    
    do.call(tagList, individuals)
    
  })
  
# this is where all my trouble started. I need to take the min/max slider values for
# all individuals and create a data frame with the mins in one col and maxs in another
# When shiny first runs, input[[paste0("ind", i)]] doesn't exist, so I was getting errors
# about trying to apply a non-function
  expenses <- reactive({
    
    class <- input$class
    members <- as.numeric(input$members)

# again, had the issue that though the slider for `input$members` was removed,
# the number of members was still set to what it was before they changed it to
# employee only, so it was still calculating expenses for individuals that
# should exist on an employee-only plan
    if(class == "emp") {
      members <- global_members
    }
    
    if(is.null(input[[paste0("ind", members)]])) {
      return()
    }
    
    else{
      
      mins <- sapply(1:members, function(i) {
        as.numeric(input[[paste0("ind", i)]])[1]
      })
      
      maxs <- sapply(1:members, function(i) {
        as.numeric(input[[paste0("ind", i)]])[2]
      })
      
      expenses <- as.data.frame(cbind(mins, maxs))
    }
    
  })
  
  
  
  best_case <- reactive({
  
  # do I have to do this inside every reactive function?
    expenses <- expenses()
    
    binary <- condition(expenses$mins, input$class)
    
    costs <- sapply(1:nrow(binary), function(i) {
      costs <- map_funcs[[as.character(binary[i, "bin"])]](binary[i, ])
      return(costs)
    } )
    
    binary$cost <- costs
    
    binary$plan <- c("A", "B", "C")
    return(binary)
    
  } )
  
  # probably inefficient to call essentially the same function twice, each
  # with one column of the `expenses` data frame, but I didn't know another way!
  worst_case <- reactive({
    
    expenses <- expenses()
    
    binary <- condition(expenses$maxs, input$class)
    
    names(binary)[ncol(binary)] <- "bin"
    
    costs <- sapply(1:nrow(binary), function(i) {
      costs <- map_funcs[[as.character(binary[i, "bin"])]](binary[i, ])
      return(costs)
    } )
    
    binary$cost <- costs
    
    binary$plan <- c("A", "B", "C")
    return(binary)
    
  } )
  
  output$main_plot <- renderPlot({

# while I can avoid red errors while shiny runs through twice, I was having issues
# here becasue this thing depends on the `binary` object, which didn't exist the
# first time through
    if(is.null(input[[paste0("ind", input$members)]])) {
      return()
    }
    
    else {

# this creates my data in ggplot-friendly form
      generate_plot_data <- function(binary) {
        
        plot <- lapply(1:nrow(binary), function(i){
          temp <- binary[i, ]
          delta <- temp$cost - temp$hsa
          plot <- data.frame(plan = rep(temp$plan, 4),
                             start = c(min(delta, 0), temp$prem, max(0, delta),
                                       c(temp$cost, temp$hsa)[(delta > 0)+1]))
          offsets <- c(0, cumsum(plot$start[2:4]))
          plot$end <- offsets - abs(plot$start)
          plot$start <- offsets
          plot$fill <- factor(c("c", "b", "a", "a"))
          plot$alpha <- factor(c(1, 1, 1, 0))
          return(plot)
        } )
        
      }
      

# now I generate the two data frames for plotting
      plot_min <- generate_plot_data(best_case())
      plot_max <- generate_plot_data(worst_case())
      
      plot <- rbind(do.call(rbind, plot_min), do.call(rbind, plot_max))
      plot$case <- c(rep("Best case", nrow(plot)/2), rep("Worst case", nrow(plot)/2))
      
      plot$fill <- factor(plot$fill, levels = c(as.character(unique(plot$fill)), "d"))
      
      p <- ggplot(plot, aes(x = plan, xend = plan, y = start, yend = end, colour = fill, alpha = alpha))
      p <- p + geom_segment(size = 35) + theme_bw() + coord_flip() + facet_wrap(~case, ncol = 2)
      p <- p + scale_alpha_discrete(range = c(0.35, 1), guide = F)
      p <- p + scale_colour_manual("Annual Cost", limits = c("a", "b", "c", "d"),
                                   labels = c("Expenses", "Premiums", "Carry-over HSA", "Expenses covered by \n 3M HSA contribution"),
                                   values = hcl(c(15, 255, 135, 15), l=65, c=100, alpha = c(1, 1, 1, 0.35)))
      p <- p + scale_y_continuous(limits = c(min(c(plot$start, plot$end)), max(c(plot$start, plot$end))),
                                  breaks = c(seq(-1000, max(plot$end, plot$start), by = 500)))
      p <- p + theme(axis.title = element_blank(), text = element_text(size = 20),
                     axis.text.x = element_text(angle = 315, hjust = 0))
      p <- p + guides(colour = guide_legend(override.aes = list(size = 7)))
      print(p)
    }
  })

# below are summary tables. Do I get penalized calling `best_case()` every time I reference a
# column like that? Should I be just doing `best_case <- best_case()` to make a copy and then
# referring to the copy from then on? (I should just try, but figured I might as well ask
# if you made it this far!)

  output$heading_best <- renderText({
    
    includeMarkdown("./best-table-header.md")
    
  })
  
  output$bar_summ_best <- renderTable({
    
    summary <- best_case()[, c("plan", "exp_ind", "exp_rem")]
    summary$cost <- best_case()$cost + best_case()$prem - best_case()$hsa
    summary$relative <- summary$cost - max(summary$cost)
    names(summary) <- c("Plan", "Max spender", "Others",
                        "Expenses + premiums - HSA", "Delta relative to max")
    print(summary)
    
  }, include.rownames = F)
  
  output$heading_worst <- renderText({
    
    includeMarkdown("./worst-case-header.md")
    
  })
  
  output$bar_summ_worst <- renderTable({
    
    summary <- worst_case()[, c("plan", "exp_ind", "exp_rem")]
    summary$cost <- worst_case()$cost + worst_case()$prem - worst_case()$hsa
    summary$relative <- summary$cost - max(summary$cost)
    names(summary) <- c("Plan", "Max spender", "Others",
                        "Expenses + premiums - HSA", "Delta relative to max")
    print(summary)
    
  }, include.rownames = F)
  
})
```