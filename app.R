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
      tabItem(tabName = "infizierteNachAlter", h2("Infizierte nach Alter"),
              selectInput("display", "Darstellung:", c("Absolute Werte" = "absolute", "Relative Werte" = "relative")),
                                                  fluidRow(
                                                  #https://stackoverflow.com/questions/69926478/figure-layout-within-shiny-app-in-r-making-the-layout-more-concise
                                                  column(width = 6,h3("Grafische Darstellung"), plotOutput("ageGroupPlot", width="100%")),
                                                  column(width = 6,h3("Tabellenwerte"), tableOutput("ageGroupTable")))),
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
    
    if(input$display == "absolute"){
      # http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization
      Agegroup_Case_Plot<-ggplot(data = age_cases_sum, aes(x=Altersgruppe, y=Gesamtinfektionen, fill=Altersgruppe)) +
        geom_bar(stat = "identity", position = position_dodge())+
        geom_text(aes(label=Gesamtinfektionen), vjust=1.6, color="white",
                  position = position_dodge(0.9), size=3.5)+
        scale_fill_brewer(palette = "Blues")+
        theme_minimal()
      Agegroup_Case_Plot
    } else {
      propTable <- round(prop.table(age_cases_sum$Gesamtinfektionen)*100,2)
      Agegroup_Case_Plot<-ggplot(data = age_cases_sum, aes(x=Altersgruppe, y=Gesamtinfektionen, fill=Altersgruppe)) +
        geom_bar(stat = "identity", position = position_dodge())+
        geom_text(aes(label=propTable), vjust=1.6, color="white",
                  position = position_dodge(0.9), size=3.5)+
        scale_fill_brewer(palette = "Blues")+
        theme_minimal()
      Agegroup_Case_Plot
    }
  })
  
  
  output$ageGroupTable <- renderTable({
    tableAge <- covid_19_df$Altersgruppe
    tableCases <- covid_19_df$AnzahlFall
    table_age_cases_sum <- aggregate(cases, by = list(ages), sum)
    colnames(table_age_cases_sum) <- c("Altersgruppe", "Gesamtinfektionen")
    table_age_cases_sum$Altersgruppe <- gsub("A", "", age_cases_sum$Altersgruppe)
    if(input$display == "absolute"){
      colnames(table_age_cases_sum) <- c("Altersgruppe in Jahren", "Anzahl der Gesamtinfektionen")
      table_age_cases_sum
    } else{
      table_age_cases_sum$Gesamtinfektionen <- round(prop.table(age_cases_sum$Gesamtinfektionen)*100,2)
      colnames(table_age_cases_sum) <- c("Altersgruppe in Jahren", "Gesamtinfektionen in Prozent")
      table_age_cases_sum
    }
  })
  
    
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

