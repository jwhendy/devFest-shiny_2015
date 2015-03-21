library(shiny)
library(markdown)

shinyUI(fluidPage(
  # title
  titlePanel("Transportation exploration"),
  
  sidebarLayout(
    sidebarPanel(
    
      selectInput(inputId = "x",
                  label = "Select x-axis variable", 
                  list("Population" = "population", "Service area" = "service_area_sq_mi",
                       "VOMS" = "voms", "Annual miles" = "annual_vehicle_miles",
                       "Vechicle hours" = "annual_vehicle_hours", "Unlinked trips" = "unlinked_passngr_trips",
                       "Passenger miles" = "passenger_miles", "Total BTUs" = "btus_total",
                       "BTUs/passenger-mile" = "btus_pmile", "Ave. occupancy" = "average_occupancy")),
    
      selectInput(inputId = "y",
                  label = "Select y-axis variable", 
                  list("Population" = "population", "Service area" = "service_area_sq_mi",
                       "VOMS" = "voms", "Annual miles" = "annual_vehicle_miles",
                       "Vechicle hours" = "annual_vehicle_hours", "Unlinked trips" = "unlinked_passngr_trips",
                       "Passenger miles" = "passenger_miles", "Total BTUs" = "btus_total",
                       "BTUs/passenger-mile" = "btus_pmile", "Ave. occupancy" = "average_occupancy")),
    
      selectInput(inputId = "color",
                  label = "Select color variable", 
                  list("None" = "none", "City" = "city", "State" = "state", "Org" = "org_type",
                  "Mode" = "mode", "Service" = "service", "Energy type" = "primary_energy")),
      
      selectInput(inputId = "size",
                  label = "Select size variable", 
                  list("None" = "none", "Population" = "population", "Service area" = "service_area_sq_mi",
                       "VOMS" = "voms", "Annual miles" = "annual_vehicle_miles",
                       "Vechicle hours" = "annual_vehicle_hours", "Unlinked trips" = "unlinked_passngr_trips",
                       "Passenger miles" = "passenger_miles", "Total BTUs" = "btus_total",
                       "BTUs/passenger-mile" = "btus_pmile", "Ave. occupancy" = "average_occupancy")),
    
      selectInput(inputId = "x_trans", label = "x variable transformation",
                  list("None" = "identity", "log" = "log", "sqrt" = "sqrt")),
    
      selectInput(inputId = "y_trans", label = "y variable transformation",
                  list("None" = "identity", "log" = "log", "sqrt" = "sqrt")),
    
      selectInput(inputId = "size_trans", label = "Size variable transformation",
                  list("None" = "identity", "log" = "log", "sqrt" = "sqrt"))
    ),
  
    mainPanel(
      tabsetPanel(
        tabPanel("Plot",
                 showOutput("plot", "nvd3")) #width = "100%", height = "600px"))#,
        #tabPanel("Code",
        #         includeMarkdown("./code.Rmd"))
    )
))))