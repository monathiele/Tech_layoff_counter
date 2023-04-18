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
devtools::install_github("andreacirilloac/updateR")
library(updateR)
updateR()

R.Version()$version.string


# LOAD Packages
install.packages(c("shiny", "readxl", "dplyr"))
library(shiny)
library(ggplot2)
library(readxl)
library(dplyr)

#read data (from Kaggel: https://www.kaggle.com/datasets/dc0e09e5c6a808eedf06d57474263925202d84f9c35fabd6a447eab52fb1957f?resource=download )
layoffs <- read.csv("layoffs.csv")
head(layoffs)

# Convert total_laid_off and percentage_laid_off to numeric
layoffs$total_laid_off <- as.numeric(layoffs$total_laid_off)
layoffs$percentage_laid_off <- as.numeric(layoffs$percentage_laid_off)


# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Layoffs by Industry"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "industry",
                  label = "Choose an Industry:",
                  choices = sort(unique(layoffs$industry)),
                  selected = NULL)
    ),
    mainPanel(
      h3("Total Laid Off:"),
      verbatimTextOutput("total_laid_off"),
      h3("Percentage Laid Off:"),
      verbatimTextOutput("percentage_laid_off")
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  industry_data <- reactive({
    filter(layoffs, industry == input$industry)
  })
  
  output$total_laid_off <- renderPrint({
    sum(industry_data()$total_laid_off, na.rm = TRUE)
  })
  
  output$percentage_laid_off <- renderPrint({
    paste0(round(sum(industry_data()$percentage_laid_off, na.rm = TRUE), 2), "%")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
