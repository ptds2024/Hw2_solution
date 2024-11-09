#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
# shinythemes is used to set up nice style of the interface
library(shinythemes) 
# owmr is used to access from OpenWeatherMap website
library(owmr)
library(dplyr)
library(ggplot2)
# httr is used to check correctness of the entered city names
library(httr)


# Do not forget to enter your API key in this code, otherwise the program will not work correctly!!!!

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("united"),
                
  # Application title
  titlePanel("Climatic factors today and in the next 5 days."),
  
  # to better control location of input and output objects we use fluidRow         
  fluidRow(
    column(3,  # corresponds to the width of the input panel
      # we enter a city name
      textInput("City", label = "City", value = ""),
      # we select climatic factor
      selectInput("letter", label = "Parameter", choices = 
        c("Choose one" = "", list("Temperature", "Humidity", "Pressure")
        )
      ),
      # we add "submit" button
      actionButton("button", "submit")
    ),
    column(8,
           # using span() function we can control of size and color of the text output
           span(textOutput("name"), style = "font-size: 18px;", style = "color:grey"),
           # we add free space between text output
           HTML("<br><br>"),
           # we plot weather forecasts
           plotOutput("plot")
    ),
  ),
)

# Define server logic required to draw a histogram

server <- function(input, output) {
  
  # here you enter your API key
  
  mykey = "..."
  
  
  # this function calculates the message regarding the current weather to be printed
  
  current_weather <- eventReactive(input$button, {
    
    # to start with, determine the column to which we should have access
    
    i <- translation(input$letter)
    
    # we check that we have already chosen an option
    
    if (i != "Choose one"){
      
      # we set up API key to be able to access the data
      
      owmr_settings(api_key = mykey)
      
      # we check whether the city name was written correctly
      
      is_valid <- check_city_validity(input$City, mykey)
    
      if (is_valid){
        
        # we read the dataset with current weather
        
        data = get_current(city=input$City, units = "metric")
        
        # we extract the desired value
        
        value <- data$main[[i]]
        
        # we create the message to be printed
        
        paste("Welcome to ", input$City, "! The ", input$letter ," in ", input$City, " is ", value, " ", measurement(input$letter), ".", sep="")
      }
      else{
        paste("Invalid city name. Please check the city argument.")
      }
    }
    else{
      paste("Please, choose one of the selected options.")
    }
  })
  
  # this function plots the weather forecasts and its output is a ggplot object
  
  forecast_weather <- eventReactive(input$button, {
    
    i <- translation(input$letter)
    
    if (i != "Choose one"){
      
      # we determine the string to be printed next to the Oy axe
      
      y <- paste(input$letter, " (", measurement(input$letter) ,")", sep="")
      
      owmr_settings(api_key = mykey)
      
      is_valid <- check_city_validity(input$City, mykey)
      
      if (is_valid){
        
        # we read the data with weather forecast information 
        
        forecast_data <- get_forecast(city = input$City, units = "metric")
        
        # we trasform dataset to tibble format
        
        forecast_df <- owmr_as_tibble(forecast_data)
        
        # we parse datetime
        
        forecast_df$datetime <- as.POSIXct(forecast_df$dt_txt, tz = "UTC")
        
        # we create class for forecast_df
        tmp_class <- class(forecast_df)
        class(forecast_df) <- c("forecast", tmp_class)
        
        # we write the title for our plot
        
        title <- paste("5-Day ", input$letter," Forecast for ", input$City, sep="")
        
        # we plot temperature over time
        
        # my_plot <- plot(forecast_df, i, title, y)
        # my_plot <- ggplot(forecast_df, aes(x = forecast_df$datetime, y = forecast_df[[i]])) +
        #   geom_line(color = "blue") +
        #   labs(title = title,
        #        x = "Date and Time",
        #        y = y) +
        #   theme_minimal()
        my_plot <- plot(forecast_df, i, title, y)
        
        # we return the ggplot
        
        my_plot
      }
    }
  })
  
  # we write the message about the current weather
  
  output$name <- renderText({
    current_weather()
  })
  
  # we plot the weather forecast
  
  output$plot <- renderPlot({
    forecast_weather()
  })
}

# plot method for forecast
plot.forecast <- function(obj, i, title, y, ...){
  ggplot(obj, aes(x = obj$datetime, y = obj[[i]])) +
    geom_line(color = "blue") +
    labs(title = title,
         x = "Date and Time",
         y = y) +
    theme_minimal()
}


# the following function checks whether the entered city exists in the datasets

check_city_validity <- function(city_name, api_key) {
  
  # Call GET function to check the availability of city_name in the dataset.
  # The response object contains the result of the API request.
  
  response <- GET(
    "http://api.openweathermap.org/data/2.5/weather",
    query = list(q = city_name, appid = api_key)
  )
  
  # Determine the status of the HTTP request using the status_code function.
  status <- status_code(response)
  
  # Check if the status code indicates success (200).
  # Return TRUE if successful, otherwise return FALSE.
  
  if (status == 200) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# this function translates parameters into the column names in the datasets

translation <- function(parameter) {
  if (parameter == "Temperature"){
    "temp"
  }
  else if (parameter == "Humidity"){
    "humidity"
  }
  else if (parameter == "Pressure"){
    "pressure"
  }
  else{
    "Choose one"
  }
}

# To not forget units of measurement we create a function
# that maps the climatic factors to the appropriate units

measurement <- function(parameter) {
  if (parameter == "Temperature"){
    "°C"
  }
  else if (parameter == "Humidity"){
    "%"
  }
  else if (parameter == "Pressure"){
    "hPa"
  }
}

# Run the application

shinyApp(ui = ui, server = server)
