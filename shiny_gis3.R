# Before the application can be run, wd set up.
# setwd("~/Code/RShiny")

## FINAL

library(shiny)
library(rgdal)
library(rgeos)
library(tmap)
library(leaflet)
library(vioplot)

# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Singapore Electoral Boundary Data Explorer"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    sidebarPanel(
      h3("Make your choice"),
      helpText("Explore Singapore data based on selected variables"),
      selectInput(
      "selectedCol1",
      "Select 1st boundary to compare",
      choices = list("ALJUNIED",
                     "ANG MO KIO",
                     "BISHAN-TOA PAYOH",
                     "BUKIT BATOK",
                     "BUKIT PANJANG",
                     "CHUA CHU KANG",
                     "EAST COAST",
                     "HOLLAND-BUKIT TIMAH",
                     "HONG KAH NORTH",
                     "HOUGANG",
                     "JALAN BESAR",
                     "JURONG",
                     "KEBUN BARU",
                     "MACPHERSON",
                     "MARINE PARADE",
                     "MARSILING-YEW TEE",
                     "MARYMOUNT",
                     "MOUNTBATTEN",
                     "NEE SOON",
                     "PASIR RIS-PUNGGOL",
                     "PIONEER",
                     "POTONG PASIR",
                     "PUNGGOL WEST",
                     "RADIN MAS",
                     "SEMBAWANG",
                     "SENGKANG",
                     "TAMPINES",
                     "TANJONG PAGAR",
                     "WEST COAST",
                     "YIO CHU KANG",
                     "YUHUA"),
      selected = "Bound 1"
    ),
    selectInput(
      "selectedCol2",
      "Select 2st boundary to compare",
      choices = list("ALJUNIED",
                     "ANG MO KIO",
                     "BISHAN-TOA PAYOH",
                     "BUKIT BATOK",
                     "BUKIT PANJANG",
                     "CHUA CHU KANG",
                     "EAST COAST",
                     "HOLLAND-BUKIT TIMAH",
                     "HONG KAH NORTH",
                     "HOUGANG",
                     "JALAN BESAR",
                     "JURONG",
                     "KEBUN BARU",
                     "MACPHERSON",
                     "MARINE PARADE",
                     "MARSILING-YEW TEE",
                     "MARYMOUNT",
                     "MOUNTBATTEN",
                     "NEE SOON",
                     "PASIR RIS-PUNGGOL",
                     "PIONEER",
                     "POTONG PASIR",
                     "PUNGGOL WEST",
                     "RADIN MAS",
                     "SEMBAWANG",
                     "SENGKANG",
                     "TAMPINES",
                     "TANJONG PAGAR",
                     "WEST COAST",
                     "YIO CHU KANG",
                     "YUHUA"),
      selected = "Bound 2"
    ),
    selectInput("Map_Choice", 
                label = "View electoral boundary level data on an interactive map of Singapore",
                choices = list("Income Data", 
                               "Education Data"),
                selected = "Income Data"),
    
    h4("About"),
    p("This project was completed by Josea Evan, as part of coursework for GIS III at the University of Chicago. Contact: josea [at] uchicago.edu"),
    h4("Data Source"),
    p("This data is from the Singapore Census. Electoral Data from 2020 Singapore General Election. Income and Education Data crosswalked from 2015 Household Survey.")
  ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Education Data", plotOutput("distPlot")),
                  tabPanel("Income Data", plotOutput("incPlot")),
                  tabPanel("Map", leafletOutput("working_map"))

      )
      
    )
  )
)

# Define server logic for random distribution app ----
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    test_edu = 
      eld_edu %>% filter(eld_bounds %in% c(input$selectedCol1, input$selectedCol2))
    
    
    ggplot(test_edu, aes(fill=factor(edu_level, levels=rev(edu_levels)), y=pct, x= "Boundary Name")) + 
      geom_bar(position="fill", stat="identity") + facet_grid(~eld_bounds) + 
      theme(legend.box.background = element_rect(),
            legend.title = element_text(face='bold')) + 
      labs(fill = "Highest Education Level Attained (2015)") +
      ylab("Percentage of Population") 
    
  })

  #Generate a Violin Plot of Variables 
  output$incPlot <- renderPlot({
    test_inc = 
      eld_income %>% filter(eld_bounds %in% c(input$selectedCol1, input$selectedCol2))
    
    
    ggplot(test_inc, aes(fill=factor(inc_level, levels=rev(inc_levels)), y=pct, x= "Boundary Name")) + 
      geom_bar(position="fill", stat="identity") + facet_grid(~eld_bounds) + 
      theme(legend.box.background = element_rect(),
            legend.title = element_text(face='bold')) + 
      labs(fill = "Gross Monthly Income from Work (2015)") +
      ylab("Percentage of Population") 
  })
  
  output$working_map <- renderLeaflet({
    
    map <- switch(input$Map_Choice, 
                  "Income Data" = income_map,
                  "Education Data" = edu_map)
    
    working_map <- tmap_leaflet(map)
    
  })
}

# Create Shiny app ----
shinyApp(ui, server)
