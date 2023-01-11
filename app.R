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
library(lubridate)

covid_19_df <- na.omit(read.csv("./RKI_COVID19_Berlin.csv"))

# Dataframes
dates <- covid_19_df$Meldedatum
unique_converted_dates <- unique(ymd_hms(dates))



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
        infoBoxOutput("infoBox_cases"),
        infoBoxOutput("infoBox_recovered"),
        infoBoxOutput("infoBox_deaths"),
        infoBoxOutput("infoBox_activeCases"),
        
        # TODO: Umwandeln der statischen InfoBoxen in dynamische [infoBoxOutput]
      ),
      fluidRow(
        box(plotOutput("plot1")),
        
        box(
          "Box content here", br(), "More box content",
          sliderInput("slider", "Slider input:",
                      min = min(unique_converted_dates),
                      max = max(unique_converted_dates),
                      value = max(unique_converted_dates)),
        )
      )
      ),
      tabItem(tabName = "infizierteNachAlter", h2("Content infizierte nach Alter"),
              selectInput("display", "Display:", c("Absolute numbers" = "absolute", "Relative percentages" = "relative")),
                                                  column(6, plotOutput("ageGroupPlot")),
                                                  column(6, tableOutput("ageGroupTable"))),
      tabItem(tabName = "infizierteNachGeschlecht", h2("Content Infizierte nach Geschlecht")),
      tabItem(tabName = "infizierteNachBezirk",h2("Content Infizierte nach Bezirk"))
    )
  )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$plot1 <- renderPlot({
  })
  
  # Overview
  output$infoBox_cases <- renderInfoBox({
    selected_date <- as.Date(input$slider)
    cases <- sum(covid_19_df[covid_19_df$Meldedatum <= selected_date, "AnzahlFall"])
    infoBox("Anzahl Ansteckungen:", cases, icon = icon("heartbeat"), color = "purple", width = 3)
  })
  
  output$infoBox_recovered <- renderInfoBox({
    selected_date <- as.Date(input$slider)
    recovered <- sum(covid_19_df[covid_19_df$Meldedatum <= selected_date, "AnzahlGenesen"])
    infoBox("Anzahl Genesen:", recovered, icon = icon("medkit"), color = "green", width = 3)
  })
  
  output$infoBox_deaths <- renderInfoBox({
    selected_date <- as.Date(input$slider)
    deaths <- sum(covid_19_df[covid_19_df$Meldedatum <= selected_date, "AnzahlTodesfall"])
    infoBox("Anzahl Todesfälle:", deaths, icon = icon("skull"), color = "red", width = 3)
  })
  
  # TODO: Aktive Fälle anpassen
  output$infoBox_activeCases <- renderInfoBox({
    selected_date <- as.Date(input$slider)
    recovered <- sum(covid_19_df[covid_19_df$Meldedatum <= selected_date, "AnzahlGenesen"])
    cases <- sum(covid_19_df[covid_19_df$Meldedatum <= selected_date, "AnzahlFall"])
    deaths <- sum(covid_19_df[covid_19_df$Meldedatum <= selected_date, "AnzahlTodesfall"])
    activeCases <- cases - (recovered + deaths)
    infoBox("Aktive Fälle:", activeCases, icon = icon("ambulance"), color = "blue", width = 3)
  })
  
  
  # Infizierte nach Alter
  output$ageGroupPlot <- renderPlot({
    ages <- covid_19_df$Altersgruppe
    cases <- covid_19_df$AnzahlFall
    age_cases_sum <- aggregate(cases, by = list(ages), sum)
    colnames(age_cases_sum) <- c("Altersgruppe", "Gesamtinfektionen")
    age_cases_sum$Altersgruppe <- gsub("A", "", age_cases_sum$Altersgruppe)
    
    barplot(age_cases_sum$Gesamtinfektionen, names.arg = age_cases_sum$Altersgruppe, xlab = "Altersgruppe", 
           ylab = "Gesamtinfektionen", main = "Infektionen nach Altersgruppe")
  })
  output$ageGroupTable <- renderTable({
    age_cases_sum
  }, container = div(style = "background-color: white;"))
  
    
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

