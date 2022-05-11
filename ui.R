# Game of Life

library(shiny)
library(plot.matrix)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Play the Game of Life"),
    p("This app plays the Game of Life invented by John Conway"),
    p("First, choose one of the 3 possible starting configurations from the Choose Starting Configuration pull-down list"),
    p("Next, click the Set Initial Game Board button to set the game board to the chosen configuration"),
    p("Finally, repeatedly click the Next Generation button to advance to the next generation"),
    selectInput("firstGeneration", "Choose Starting Configuration", choices=c("Block of Five", "Glider", "Glider Cannon")),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(actionButton("setInitialConfiguration", "Set Initial Game Board"),
                     actionButton("nextGeneration", "Next Generation"),
                     textOutput("nGenerationLabel"),
                     textOutput("nGeneration")),
        # Show a plot
        mainPanel(
            plotOutput("distPlot")
        )
    )
))
