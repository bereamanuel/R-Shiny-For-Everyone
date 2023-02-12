# Dependences -----
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(readr)
library(DT)
library(echarts4r)
library(leaflet)
library(data.tree)
library(tibble)
# Define UI ------
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Biodiversity in National Parks"),
  dashboardSidebar(sidebarMenu(
    menuItem(
      tabName = "history",
      text = "Main",
      icon = icon("folder-open")
      
    ),
    menuItem(
      tabName = "data",
      text = "Data",
      icon = icon("chart-bar")
      
    ),
    menuItem(
      tabName = "ml",
      text = "Models",
      icon = icon("flask-vial")
      
    )
  )),
  dashboardBody(
    includeCSS("www/style.css"),
    tabItems(
      tabItem(tabName = "history"
              ,
              fluidPage(
                box(
                  width = 24,
                  title = "Biodiversity in National Parks",
                  status = "primary"
                  ,
                  includeMarkdown("history.md")
                ),
                box(width = 24,
                    fluidRow(
                      box(wigth = 12,
                          echarts4rOutput("park_count_bar")),
                      box(wigth = 12,
                          leafletOutput("map", height = "43vh"))
                    ))
              )),
      tabItem(
        tabName = "data",
        fluidPage(
          fluidRow(box(
            wigth = 12,height = "25vh",
            box(wigth = 6,height = "20vh" , uiOutput("park_picker")),
            box(wigth = 6,height = "20vh" , uiOutput("cat_check_box"))
          ),
          box(wigth = 12, 
            box(wigth = 6,height = "20vh" , uiOutput("nativeness_picker")),
            box(wigth = 6,height = "20vh" , uiOutput("occurrence_check_box"))
          )),
          fluidRow(
            box(wigth = 12, height = "70vh",
                fluidRow(uiOutput("vb_present")),
                fluidRow(
                  box(width = 6, uiOutput("vb_family")),
                  box(wigth = 6, uiOutput("vb_order"))
                ),
                fluidRow(echarts4rOutput("record_status_bar"))),
            box(
              wigth = 12, height = "70vh",
              echarts4rOutput("sunburst_char",width = "100%", height = "700px")
            )
          ),
          fluidRow(tabBox(
            width = 24,
            height = "30vh",
            tabPanel(
              title = "Species",
              icon = icon("tree"),
              DTOutput("data_species_table")
            ),
            tabPanel(
              title = "Parks",
              icon = icon("binoculars"),
              DTOutput("data_parks_table")
            )
          ))
        )
      ),
      tabItem(tabName = "ml"
              , fluidPage(
                box(
                  width = 12,
                  title = "Models",
                  status = "primary"
                  ,
                  ""
                )
              ))
    )
  ),
  title = "Biodiversity in National Parks"
)

# Define server --------
server <- function(input, output) {
  # Load Data
  ## Parks ---------
  data_parks <-
    read_delim(paste0(getwd(), "/data/parks.csv"), delim  = ",") %>%
    rename(
      park_code = `Park Code`,
      park_name = `Park Name`,
      state = State,
      acres = Acres,
      lat = Latitude,
      lng = Longitude
    )
  
  output$data_parks_table <- renderDT({
    datatable(
      data_parks,
      rownames = F,
      options = list(
        paging = T,
        scrollY = "20vh",
        scrollX = "100%"
      )
    )
  })
  
  output$park_picker <- renderUI({
    choices <- data_parks %>%
      pull(park_name) %>%
      unique()
    pickerInput("park_picker",
                "Select Park",
                choices = choices)
  })
  
  ## Species ---------
  data_species <-
    read_delim(paste0(getwd(), "/data/species.csv"), delim  = ",") %>%
    rename(
      species_ID = `Species ID`,
      park_name = `Park Name`,
      category = Category,
      order = Order,
      family = Family,
      scientific_name = `Scientific Name`,
      common_name = `Common Names`,
      record_status = `Record Status`,
      occurrence = Occurrence,
      nativeness = Nativeness,
      abundance = Abundance,
      seasonality = Seasonality,
      conservation_status = `Conservation Status`,
      unknown = `...14`
    ) %>%
    select(-unknown)
  
  output$cat_check_box <- renderUI({
    req(input$park_picker)
    categories <- data_species %>%
      filter(park_name == input$park_picker) %>%
      select(category) %>% pull() %>% unique()
    categories <- categories[categories != ""]
    
    checkboxGroupButtons(
      "cat_check_box",
      label = "Species Category",
      choices = categories,
      selected = categories,
      status = "default",
      size = "normal",
      direction = "vertical",
      width = "100%"
    )
  })
  
  output$nativeness_picker <- renderUI({
    req(input$park_picker)
    req(input$cat_check_box)
    nativeness <- data_species %>%
      filter(park_name == input$park_picker) %>%
      filter(category %in% input$cat_check_box) %>%
      pull(nativeness) %>%
      unique()
    nativeness <- nativeness[nativeness != ""]
    nativeness <-
      nativeness[!is.na(nativeness) & nativeness != "NA"]
    
    checkboxGroupButtons(
      "nativeness_picker",
      "Nativiness",
      choices = nativeness,
      selected = nativeness,
      status = "default",
      size = "normal",
      direction = "vertical",
      width = "100%"
    )
  })
  
  output$occurrence_check_box <- renderUI({
    req(input$park_picker)
    req(input$cat_check_box)
    req(input$nativeness_picker)
    occurrence <- data_species %>%
      filter(park_name == input$park_picker) %>%
      filter(category %in% input$cat_check_box) %>%
      filter(nativeness %in% input$nativeness_picker) %>%
      select(occurrence) %>% pull() %>% unique()
    occurrence <- occurrence[occurrence != ""]
    checkboxGroupButtons(
      "occurrence_check_box",
      label = "Occurrence Category",
      choices = occurrence,
      selected = occurrence,
      status = "default",
      size = "normal",
      direction = "vertical",
      width = "100%"
    )
  })
  
  data_species_reactive <- reactive({
    req(input$park_picker)
    req(input$cat_check_box)
    req(input$occurrence_check_box)
    req(input$nativeness_picker)
    data_species %>%
      filter(park_name == input$park_picker) %>%
      filter(nativeness %in% input$nativeness_picker) %>%
      filter(category %in% input$cat_check_box) %>%
      filter(occurrence %in% input$occurrence_check_box)
  })
  
  output$data_species_table <- renderDT({
    req(input$park_picker)
    req(input$cat_check_box)
    req(input$occurrence_check_box)
    req(input$nativeness_picker)
    datatable(
      data_species_reactive(),
      rownames = F,
      options = list(
        paging = F,
        scrollY = "30vh",
        scrollX = "100%"
      )
    )
  })
  
  # Value boxes -----
  output$vb_present <- renderValueBox({
    present <- data_species_reactive() %>%
      nrow()
    valueBox(value = format(present, big.mark = ","),
             subtitle = "Species")
  })
  
  output$vb_order <- renderValueBox({
    present <- data_species_reactive() %>%
      pull(order) %>% unique() %>% length()
    valueBox(value = format(present, big.mark = ","),
             subtitle = "Order")
  })
  
  output$vb_family <- renderValueBox({
    present <- data_species_reactive() %>%
      pull(family) %>% unique() %>% length()
    valueBox(value = format(present, big.mark = ","),
             subtitle = "Families")
  })
  
  # Families bar char -----
  output$sunburst_char <-  renderEcharts4r({
    req(input$park_picker)
    
    park <- Node$new()
    assign(input$park_picker, park$AddChild(input$park_picker,value = 0))
    for (cat in data_species_reactive()  %>% pull(category) %>% unique()) {
      if (!is.na(cat)) {
        cat_aux <- cat
        assign(cat_aux, park[[input$park_picker]]$AddChild(cat_aux, 
                                      value = data_species_reactive()  %>% filter(category == cat_aux) %>% nrow() ))
        
        for (ord in data_species_reactive()  %>% 
                      filter(category == cat_aux) %>%  
                      group_by(order) %>% 
                      summarise(n = n()) %>% 
                      arrange(-n) %>% 
                      mutate(t = sum(n)) %>% 
                      head(4) %>% 
                      mutate(t1 = t - sum(n)) %>% 
                      bind_rows(setNames(c(order = "others", n = .[1,4],t=0,t1=0), names(.))) %>% 
                      pull(order)) {
          if (!is.na(ord) & ord != "others") {
            ord_aux <- ord
            assign(ord_aux, park[[input$park_picker]][[cat_aux]]$AddChild(ord_aux,
                                                  value = data_species_reactive()  %>% filter(category == cat_aux, order == ord_aux) %>% nrow()))
          } else{
            assign("others", park[[input$park_picker]][[cat_aux]]$AddChild("others",
                                                                          value = data_species_reactive()  %>% 
                                                                            filter(category == cat_aux) %>%  
                                                                            group_by(order) %>% 
                                                                            summarise(n = n()) %>% 
                                                                            arrange(-n) %>% 
                                                                            mutate(t = sum(n)) %>% 
                                                                            head(4) %>% 
                                                                            mutate(t1 = t - sum(n)) %>%
                                                                            head(1) %>% 
                                                                            pull(t1)))
          }
        }
      }
    }
    
    
    park %>% 
      e_charts() %>% 
      e_sunburst() %>% 
      e_title("Sunburst graph") %>% 
      e_tooltip()
  })
  
  
  
  # Order bar char -----
  output$order_pie_bar <-  renderEcharts4r({
    req(input$top_n_bar)
    data_species_reactive() %>%
      select(species_ID, order) %>%
      group_by(order) %>%
      table() %>%
      as.data.frame() %>%
      group_by(order) %>%
      summarise(count = sum(Freq)) %>%
      arrange(-count) %>%
      head(input$top_n_bar) %>%
      e_chart(order) %>%
      e_pie(count, name = "Order Category") %>%
      e_title(paste0("Top ", as.character(input$top_n_bar), " Orders")) %>% 
      e_tooltip() %>% 
      e_legend(FALSE)
  })
  
  # Category bar char -----
  output$record_status_bar <-  renderEcharts4r({
    data_species_reactive() %>%
      select(species_ID, record_status) %>%
      group_by(record_status) %>%
      table() %>%
      as.data.frame() %>%
      group_by(record_status) %>%
      summarise(count = sum(Freq)) %>%
      arrange(-count) %>%
      e_chart(record_status) %>%
      e_bar(count, name = "Record Status") %>%
      e_title(paste0("Record Status")) %>% 
      e_legend(FALSE)
  })
  
  # Map ----------------
  output$map <- renderLeaflet({
    data_parks %>%
      mutate(popup = paste(# Mutate a column for popup text
        "<center> <b> Park Name </b>"
        , "<br>"
        , park_name
        , "</center>")) %>%
      leaflet() %>%
      addProviderTiles(provider = "Esri.WorldImagery") %>% # satallite imagery provider tile
      addMarkers(
        lng = ~ lng,
        lat = ~ lat,
        label = ~ park_code,
        popup = ~ popup
      )
  })
  
  # State bar char -----
  output$park_count_bar <-  renderEcharts4r({
    data_species %>%
      inner_join(data_parks, by = "park_name") %>%
      select(species_ID, state) %>%
      group_by(state) %>%
      table() %>%
      as.data.frame() %>%
      group_by(state) %>%
      summarise(count = sum(Freq)) %>%
      arrange(-count) %>%
      e_chart(state) %>%
      e_bar(count, name = "State Species", color = "black") %>%
      e_title(paste0("State Species")) %>%
      e_legend(FALSE)
  })
  
  
}



# Run the application ----------
shinyApp(ui = ui, server = server)
