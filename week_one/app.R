#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
ui <- dashboardPage(dashboardHeader(
                      title="Old Faithful"
                    ),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem(
                          tabName = "data",
                          text = "Data",
                          icon = icon("chart-bar")
                        ),
                        menuItem(
                          tabName = "history",
                          text = "History",
                          icon = icon("folder-open")
                          
                        )
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(
                          tabName = "data",
                              fluidRow(
                                box(
                                  width = 12,
                                  title = "Slider",
                                  status = "primary",
                                  plotOutput("distPlot")
                                )
                              ),
                              fluidRow(
                                box(
                                  width = 12,
                                  title = "Slider",
                                  status = "primary",
                                  sliderInput(
                                      "bins",
                                      "Number of bins:",
                                      min = 1,
                                      max = 50,
                                      value = 30
                                  )
                                )
                              )
                          ),
                          tabItem(
                            tabName = "history",
                            box(
                              width = 12,
                              title = "History",
                              status = "primary",
                              "Old Faithful is a cone geyser in Yellowstone National Park in Wyoming, United States. It was named in 1870 during the Washburn–Langford–Doane Expedition and was the first geyser in the park to be named.[3][4] It is a highly predictable geothermal feature and has erupted every 44 minutes to two hours since 2000.[5] The geyser and the nearby Old Faithful Inn are part of the Old Faithful Historic District."
                            )
                          )
                      )
                    ),
                    title = "Dashboard example")

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(
      x,
      breaks = bins,
      col = 'darkgray',
      border = 'white',
      xlab = 'Waiting time to next eruption (in mins)',
      main = 'Histogram of waiting times'
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
