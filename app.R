# Load packages ----------------------------------------------------------------

library(shiny)
library(ggplot2)
install.packages('rsconnect')
rsconnect::setAccountInfo(name='yrdbi0-bereamanuel',
                          token='CBE9817D71BEEA1F27991C7FF62C6C7B',
                          secret='xCoITBpa6xwswp1nS1oWF+OP1C1Ui4slLmYxgRV0')

library(rsconnect)
rsconnect::deployApp(getwd())

# Load data --------------------------------------------------------------------

load("movies.RData")

# Define UI --------------------------------------------------------------------

ui <- fluidPage(
  
  sidebarLayout(
    
    # Inputs: Select variables to plot
    sidebarPanel(
      
      # Select variable for y-axis
      selectInput(
        inputId = "y",
        label = "Y-axis:",
        choices = c("imdb_rating", "imdb_num_votes", "critics_score", "audience_score", "runtime"),
        selected = "audience_score"),
      # Select variable for x-axis
      selectInput(
        inputId = "x",
        label = "X-axis:",
        choices = c("imdb_rating", "imdb_num_votes", "critics_score", "audience_score", "runtime"),
        selected = "critics_score"),
      # Select variable for color
      selectInput(inputId = "color", 
                  label = "Dots colour",
                  choices = c("red","blue","black"),
                  selected = "color_plot")
    
    ),
    # Output: Show scatterplot
    mainPanel(
      plotOutput(outputId = "scatterplot")
    )
  )
)

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  output$scatterplot <- renderPlot({
    ggplot(data = movies, aes_string(x = input$x, 
                                     y = input$y)) +
      geom_point(color = input$color)
  })
}

# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)
