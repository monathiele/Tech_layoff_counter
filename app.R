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
#layoffs$date <- as.POSIXct(layoffs$date, format = "%Y-%m-%d")
layoffs$date <- as.Date(layoffs$date, format="%Y-%m-%d")


ui <- fluidPage(
  titlePanel(div("Layoff Counter 😢", style = "text-align: center; padding: 20px;")),
  tags$head(tags$style(HTML("body {background-color: #FF69B4;} .navbar-brand {font-weight: bold; font-size: 24px;}"))),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("industry", "Select Industries:",
                  choices = sort(unique(layoffs$industry)),
                  multiple = TRUE,
                  selected = "All")
    ),
    mainPanel(
      plotOutput("industry_plot", height = "400px"),
      plotOutput("time_plot", height = "300px")
    )
    
  )
)

server <- function(input, output) {
  
  # Filter data by selected industry
  filtered_layoffs <- reactive({
    if (is.null(input$industry) || "All" %in% input$industry) {
      return(layoffs %>% arrange(desc(percentage_laid_off)))
    } else {
      return(layoffs %>% filter(industry %in% input$industry) %>% arrange(desc(percentage_laid_off)))
    }
  })
  
  # Bar plot of percentages laid off by industry
  output$industry_plot <- renderPlot({
    sorted_layoffs <- filtered_layoffs() %>% arrange(desc(percentage_laid_off))
    ggplot(sorted_layoffs, aes(x = industry, y = percentage_laid_off, fill = industry)) +
      geom_bar(stat = "identity") +
      labs(x = "Industry", y = "Percentage Laid Off", title = "Percentage Laid Off by Industry") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  })
  
  # Time plot of layoffs over time
  output$time_plot <- renderPlot({
    sorted_layoffs <- filtered_layoffs() %>% arrange(date)
    ggplot(sorted_layoffs, aes(x = date, y = total_laid_off, color = industry)) +
      geom_point() +
      labs(x = "Month", y = "Total Laid Off", title = "Total Layoffs over Time") +
      scale_x_date(date_labels = "%b") +
      theme_minimal()
  })
  
}

shinyApp(ui, server)


anyNA(layoffs$date)
which(is.na(layoffs$date))