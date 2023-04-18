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

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
