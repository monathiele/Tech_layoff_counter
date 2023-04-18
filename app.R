#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# UPDATE CURRENT R VERSION 
install.packages("installr")
install.packages("devtools")
install.packages("plotly")
devtools::install_github("andreacirilloac/updateR")
library(updateR)
updateR()

R.Version()$version.string


# LOAD Packages
install.packages(c("shiny", "readxl", "dplyr", "tidyverse", "lubridate"))
library(shiny)
library(ggplot2)
library(readxl)
library(dplyr)
library(plotly)
library(RColorBrewer)
library(tidyverse)
library(lubridate)

# read data (from Kaggle: https://www.kaggle.com/datasets/dc0e09e5c6a808eedf06d57474263925202d84f9c35fabd6a447eab52fb1957f?resource=download )
layoffs <- read.csv("layoffs.csv")
head(layoffs)

# Convert total_laid_off and percentage_laid_off to numeric
layoffs$total_laid_off <- as.numeric(layoffs$total_laid_off)
layoffs$percentage_laid_off <- as.numeric(layoffs$percentage_laid_off)
layoffs$date <- as.Date(layoffs$date , format = "%m/%d/%Y")



ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("industry", "Select an Industry:",
                  choices = c("All", unique(layoffs$industry)))
    ),
    mainPanel(
      plotOutput("industry_plot", height = "300px"),
      plotOutput("time_plot", height = "300px")
    )
  )
)

server <- function(input, output) {
  
  # Filter data by selected industry
  filtered_layoffs <- reactive({
    if (input$industry == "All") {
      return(layoffs)
    } else {
      return(layoffs %>% filter(industry == input$industry))
    }
  })
  
  # Bar plot of percentages laid off by industry
  output$industry_plot <- renderPlot({
    ggplot(filtered_layoffs(), aes(x = industry, y = percentage_laid_off, fill = industry)) +
      geom_bar(stat = "identity") +
      labs(x = "Industry", y = "Percentage Laid Off", title = "Percentage Laid Off by Industry") +
      theme_minimal()
  })
  
  # Line plot of total layoffs over time
  output$time_plot <- renderPlot({
    filtered_data <- filtered_layoffs() %>% 
      filter(!is.na(date)) %>% # Remove rows with missing dates
      mutate(date = as.Date(date, format = "%m/%d/%Y")) # Convert date to proper format
    ggplot(filtered_data, aes(x = date, y = total_laid_off)) +
      geom_line() +
      labs(x = "Date", y = "Total Laid Off", title = "Total Layoffs over Time") +
      theme_minimal()
  })
  
  
}

shinyApp(ui, server)


anyNA(layoffs$date)
which(is.na(layoffs$date))