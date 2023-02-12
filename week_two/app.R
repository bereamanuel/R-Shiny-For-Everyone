

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(echarts4r)
library(tidyr)
library(dplyr)
library(scales)
library(leaflet)
# UI ----
ui <- dashboardPage(
  dashboardHeader(
    title = "Week Two"
  ),
  dashboardSidebar(
    actionBttn(
      "actionBttn",
      label = "Button",
      style = "pill",
      size = "lg",
      block = F,
      no_outline = T
    )
  ),
  dashboardBody(
    fluidRow(
      box(width = 6,
          box(width = 6,
              selectInput("x", h3("Select X-Axis"), 
                          choices = list("Sepal.Length" = "Sepal.Length", "Sepal.Width" = "Sepal.Width",
                                         "Petal.Length" = "Petal.Length", "Petal.Width" = "Petal.Width"), selected = "Petal.Length") 
          ),
          br(),
          box(width = 6,
              selectInput("y", h3("Select Y-Axis"), 
                          choices = list("Sepal.Length" = "Sepal.Length", "Sepal.Width" = "Sepal.Width",
                                         "Petal.Length" = "Petal.Length", "Petal.Width" = "Petal.Width"), selected = "Petal.Width") 
          )
      ), 
      tabBox(
             tabPanel(
             title = "Scatter Plot",
             echarts4rOutput("scatter")
             ),
           tabPanel(
             title = "Bar Char",
             echarts4rOutput("bar"))
        )
    ), 
    fluidRow(
      box(width = 5,
          DTOutput("datatable")
          )
      , box(width = 5,
            leafletOutput("map"))
    )
  ),
  title = "Dashboard example"
)

# Server ----
server <- function(input, output) {
  #Table
  output$datatable <- renderDT({
      datatable(iris)
    }
  )
  # Scatter plot and Bar char
  output$scatter <- renderEcharts4r({
      iris %>% 
        select(x = input$x,
               y = input$y,
               z = Petal.Length,
               Species) %>% 
        group_by(Species) %>% 
        e_charts(x) %>% 
        e_scatter(y, z)%>%
        e_title("Scatter Plot")
    }
  )
  output$bar <- renderEcharts4r({
    iris %>% 
      select(x = input$x,
             y = input$y,
             z = Petal.Length,
             Species) %>% 
      e_charts(x) %>% 
      e_bar(y) %>%
      e_title("Bar charts") %>% 
      e_legend(FALSE)
    }
  )
  # Map
  output$map <-  renderLeaflet({
      m <- leaflet() %>% 
        addTiles() %>% 
        addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R") 
      m
    } 
  )
  observeEvent(
    input$actionBttn, {
      showNotification(
        "Action Bttn Clicked!"
      )
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
