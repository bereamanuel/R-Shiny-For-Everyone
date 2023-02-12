#
# This is a template shinydashboard application for Week Three in the EdX R Shiny
# For Everyon Course
#
# Author: Owen Bezick
# 27 June 2021

library(shiny)
library(shinydashboard)
library(DT)
library(echarts4r)
library(leaflet)
library(shinyWidgets)
library(googlesheets4)
library(dplyr)

# Define UI for template application
ui <- dashboardPage(
  dashboardHeader(
    title = "Week Three"
  ),
  dashboardSidebar(
    column(width = 12, align = "center"
           , actionBttn(
             "actionBttn",
             label = "Button",
             style = "pill",
             color = "default",
             size = "lg",
             block = F,
             no_outline = TRUE
           )
    )
  ),
  dashboardBody(
    fillPage(
      fluidRow(
        box(width = 6, height = "40vh", title = "Inputs"
            , column(width = 6
                     , uiOutput("propertyType")
                     , br()
                     , uiOutput("propertyPrice")
                     , br()
                     , uiOutput("countBedroom")
            )
            , column(width = 6
                     , br()
                     , verbatimTextOutput("type_value")
                     , br()
                     , br()
                     , br()
                     , verbatimTextOutput("price_value")
                     , br()
                     , br()
                     , br()
                     , verbatimTextOutput("bedroom_value")
            )
        )
        , tabBox(width = 6, height = "40vh"
                 , tabPanel(title = "Scatter Plot"
                            , echarts4rOutput("scatter", height = "30vh"))
                 , tabPanel(title = "Bar Chart"
                            , echarts4rOutput("bar", height = "30vh"))
        )
      )
      , fluidRow(
        box(width = 6, height = "40vh"
            , DTOutput("property_data")
        )
        , box(width = 6, height = "40vh"
              , leafletOutput("map", height = "38vh")
        )
      )
    )
  ),
  title = "Dashboard example"
)

# Server logic
server <- function(input, output) {
  
  gs4_auth(email = "mbaberearellano@gmail.com")
  property_data <- range_read(ss = "https://docs.google.com/spreadsheets/d/1zOg6HBagyjiYNTOqqBo885NvuHXSP7iLJXnuT8Juu00/edit#gid=0")
  
  
  reactive_property_data <- reactive({
      req(input$type_value, input$price_value, input$countBedroom)
      property_data %>% 
        filter(
          property_type %in% input$type_value
          , property_price <= input$price_value
          , count_bed >= input$countBedroom
        )
    }
  )
  
  output$property_data <- renderDT({
    datatable(reactive_property_data()
              , colnames = names(property_data)
              , options = list(scrollY = "25vh", scrollX = "100%"))
  })
  
  
  output$propertyType <- renderUI({
      choices <- unique(property_data$property_type)
      selectInput("type_value", label = "Type", 
                   choices = choices, 
                   selected = choices,
                   multiple = T)
    }
  )
  
  output$propertyPrice <- renderUI({
      min <- min(property_data$property_price) 
      max <- max(property_data$property_price)
      
      sliderInput("price_value", label = "Price", min = min, 
                  max = max, value = max)
      
    }
  )
  
  output$countBedroom <- renderUI({
        min <- min(property_data$count_bed) 
        max <- max(property_data$count_bed)
        
        numericInput("countBedroom", label = "Count Bedroom", min = min, 
                     max = max, value = min)

    }
  )
  
  df <- data.frame(
    x = seq(50),
    y = rnorm(50, 10, 3),
    z = rnorm(50, 11, 2),
    w = rnorm(50, 9, 2)
  )
  
  output$scatter <- renderEcharts4r({
    reactive_property_data() %>%  
      e_charts(property_id) %>% 
      e_scatter(property_price, symbol_size = 15) %>% 
      e_tooltip()
  })
  output$bar <- renderEcharts4r({
    reactive_property_data() %>%
      e_charts(property_id) %>% 
      e_bar(count_bed) %>% 
      e_tooltip()
  })
  
  output$map <- renderLeaflet({
      reactive_property_data() %>% 
        mutate(popup = paste(
            "<center> <b> Address </b>",
            "<br>",
            address,
            "</center>"
          )
        ) %>% 
        leaflet() %>% 
        addTiles() %>% 
        addMarkers(lng = ~long,
                   lat = ~lat,
                   label = ~property_id,
                   popup = ~popup)
  })
  
  output$type_value <- renderPrint({ input$type_value })
  output$price_value <- renderPrint({ input$price_value })
  output$bedroom_value <- renderPrint({ input$countBedroom })
  
  observeEvent(input$actionBttn, {
    # showNotification("Action Bttn Clicked!")
    showModal(
      modalDialog( title = "Action Button Clicked", easyClose = T, footer = NULL
                   , fluidRow(
                     box(width = 12, status = "primary"
                         , "Hello World!"
                     )
                   )
                   
      )
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
