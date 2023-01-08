shinyApp(ui = ui, server = server)
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
library(quantmod)
library(ggplot2)
library(dplyr)
covid_19_df <- na.omit(read.csv("/Users/eshmamdulal/Downloads/RKI_COVID19_Berlin.csv"))

# Define UI for application that draws a histogram
ui <- dashboardPage(
  # tags$script(src="https://kit.fontawesome.com/a00c56db4c.js"),
  # tags$div(
  #   tags$i(class = "fa-regular fa-face-smile")
  # ),
  skin = "blue",
  dashboardHeader(title = "SARS-CoV-2 in Berlin"),
  
  dashboardSidebar(
    sidebarMenu(menuItem("Overview", tabName = "overview", icon = icon("list")),
                menuItem("Infizierte nach Alter", tabName = "infizierteNachAlter"),
                menuItem("Infizierte nach Geschlecht", tabName = "infizierteNachGeschlecht"),
                menuItem("Infizierte nach Bezirk", tabName = "infizierteNachBezirk", icon = icon("map")))
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview", fluidRow(
        infoBox("Fälle gesamt", color = "purple", width = 3),
        infoBox("Todesfälle gesamt", color = "red", width = 3),
        infoBox("Aktive Fälle", color = "blue", width = 3),
        infoBox("Genesen gesamt", color = "green", width = 3)
        
        # TODO: Umwandeln der statischen InfoBoxen in dynamische [infoBoxOutput]
      ),
      fluidRow(
        box(plotOutput("plot1")),
        
        box(
          "Box content here", br(), "More box content",
          sliderInput("slider", "Slider input:", 1, 100, 50),
          textInput("text", "Text input:")
        )
      )
      ),
      tabItem(tabName = "infizierteNachAlter", h2("Content infizierte nach Alter")),
      tabItem(tabName = "infizierteNachGeschlecht", h2("Content Infizierte nach Geschlecht")),
      tabItem(tabName = "infizierteNachBezirk",h2("Content Infizierte nach Bezirk"))
    )
  )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$plot1 <- renderPlot({
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

