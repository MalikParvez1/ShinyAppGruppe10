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
library(readr)
library(DT)

covid_19_df <- na.omit(read.csv("./RKI_COVID19_Berlin.csv"))

# Dataframes
dates <- covid_19_df$Meldedatum
converted_dates <- ymd_hms(dates)
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
      ),
      fluidRow(
        box(plotOutput("lineplot")),
        
        box(
          sliderInput("slider", "Datenbereich:",
                      min = min(unique_converted_dates),
                      max = max(unique_converted_dates),
                      value = max(unique_converted_dates)),
          "Hier kann der gewünschte beobachtete Zeitraum eingestellt werden",
        )
      )
      ),
      tabItem(tabName = "infizierteNachAlter", h2("Infizierte nach Alter"),
              selectInput("display", "Display:", c("Absolute Werte" = "absolute", "Relative Werte" = "relative")),
              fluidRow(
                #https://stackoverflow.com/questions/69926478/figure-layout-within-shiny-app-in-r-making-the-layout-more-concise
                column(width = 6,h3("Grafische Darstellung"), plotOutput("ageGroupPlot", width="100%")),
                column(width = 6,h3("Tabellenwerte"), tableOutput("ageGroupTable")))),
      
      tabItem(tabName = "infizierteNachGeschlecht", h2("Covid-Infizierte nach Geschlecht"),
              sidebarLayout(
                sidebarPanel(
                  selectInput(inputId = "geschlecht",
                              label = "Geschlecht auswählen:",
                              choices = c("Männlich" = "männlich", "Weiblich" = "weiblich")),
                ),
                mainPanel(
                  plotOutput("sexGroupPlot"),
                  DT::dataTableOutput("geschlechterTabelle")
                )
              )),
      
      tabItem(tabName = "infizierteNachBezirk",h2("Content Infizierte nach Bezirk"),
              fluidRow(
                box(plotOutput("districtPlot"))
              ),
              fluidRow(              
                box(
                sliderInput("districtslider", "Datenbereich:",
                            min = min(unique_converted_dates),
                            max = max(unique_converted_dates),
                            value = max(unique_converted_dates)),
                "Hier kann der gewünschte beobachtete Zeitraum eingestellt werden",
                ),
                box(
                  selectInput("district_displaymode", "Darstellungsart:", c("Absolute Werte" = "absolute", "Relative Werte" = "relative")),
                ),
                box(
                  selectInput("district_displaymode2", "Werteauswahl:", c("Infektionen" = "districtCases", "Tode" = "districtDeaths")),
                )))
    )
  )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$lineplot <- renderPlot({
    selected_date <- as.Date(input$slider)
    Infektionen <- covid_19_df[covid_19_df$Meldedatum <= selected_date, "AnzahlFall"]
    Tode <- covid_19_df[covid_19_df$Meldedatum <= selected_date, "AnzahlTodesfall"]
    rawZeitraum <- covid_19_df[covid_19_df$Meldedatum <= selected_date, "Meldedatum"]
    Zeitraum <- as.Date(rawZeitraum)
    range <- covid_19_df$Meldedatum
    crd_sum <- data.frame(Infektionen, Tode, Zeitraum)
    start_date <- as.Date("2020-02-12")
    lineplot <- ggplot(data = crd_sum, aes(x=Zeitraum))+
      geom_line(aes(y=Infektionen, color = "Infektionen"))+
      geom_line(aes(y=Tode, color = "Tode"))+
      guides(color = guide_legend(title = "Legende"))+
      labs(y = "Anzahl")+
      scale_x_date(limits= c(start_date, selected_date), breaks = c(start_date, selected_date), labels = c(start_date, selected_date))+
      scale_color_manual(values = c("Infektionen" = "#DD4B39", "Tode" = "#6D6A6A"))+
      theme_minimal()
    lineplot
  })
  
  
  
  # Overview
  output$infoBox_cases <- renderInfoBox({
    selected_date <- as.Date(input$slider)
    cases <- sum(covid_19_df[covid_19_df$Meldedatum <= selected_date, "AnzahlFall"])
    infoBox("Anzahl Ansteckungen:", cases, icon = icon("heartbeat"), color = "red", width = 3)
  })
  
  output$infoBox_recovered <- renderInfoBox({
    selected_date <- as.Date(input$slider)
    recovered <- sum(covid_19_df[covid_19_df$Meldedatum <= selected_date, "AnzahlGenesen"])
    infoBox("Anzahl Genesen:", recovered, icon = icon("medkit"), color = "green", width = 3)
  })
  
  output$infoBox_deaths <- renderInfoBox({
    selected_date <- as.Date(input$slider)
    deaths <- sum(covid_19_df[covid_19_df$Meldedatum <= selected_date, "AnzahlTodesfall"])
    infoBox("Anzahl Todesfälle:", deaths, icon = icon("skull"), color = "black", width = 3)
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
    table_age_cases_sum <- aggregate(tableCases, by = list(tableAge), sum)
    colnames(table_age_cases_sum) <- c("Altersgruppe", "Gesamtinfektionen")
    table_age_cases_sum$Altersgruppe <- gsub("A", "", table_age_cases_sum$Altersgruppe)
    if(input$display == "absolute"){
      colnames(table_age_cases_sum) <- c("Altersgruppe in Jahren", "Anzahl der Gesamtinfektionen")
      table_age_cases_sum
    } else{
      table_age_cases_sum$Gesamtinfektionen <- round(prop.table(table_age_cases_sum$Gesamtinfektionen)*100,2)
      colnames(table_age_cases_sum) <- c("Altersgruppe in Jahren", "Gesamtinfektionen in Prozent")
      table_age_cases_sum
    }
  })
  
  output$sexGroupPlot <- renderPlot({
    # altersgruppe <- covid_19_df$Altersgruppe
    # infizierte_weiblich <- nrow(subset(covid_19_df, covid_19_df$Geschlecht == "W"))
    # weiblich_altersgruppe_sum <- aggregate(infizierte_weiblich, by = list(altersgruppe), sum)
    # colnames(weiblich_altersgruppe_sum) <- c("Altersgruppe", "Gesamtinfektionen")
    # weiblich_altersgruppe_sum$Altersgruppe <- gsub("A", "", age_cases_sum$Altersgruppe)
    
    #weiblich_aggregiert_nach_Altersgruppe <- weiblich_aggregiert
    
    # age_sex_plot <- ggplot(data = women_filter_agg, aes(x=, y=, fill=Geschlecht)) +
    #   geom_bar(stat = "identity", position = position_dodge())+
    #   ggtitle("Anzahl der Infizierten nach Geschlecht und Altersgruppe")
    # age_sex_plot
    
    altersgruppe <- covid_19_df$Altersgruppe
    women_filter <- subset(covid_19_df, covid_19_df$Geschlecht=="W")
    men <- subset(covid_19_df, covid_19_df$Geschlecht=="M")
    women_order <- women_filter[order(women_filter$Altersgruppe),]
    men_order <- men[order(men$Altersgruppe),]
    women_filter_agg <- aggregate(women_order, by=list(altersgruppe), sum)
    colnames(women_filter_agg) <- c("Altersgruppe", "Geschlecht", "Gesamtinfektionen")
    men_agg <- aggregate(men_order, by=list(altersgruppe), sum)
    
    ggplot(data = women_filter_agg, aes(x=Altersgruppe, y=Gesamtinfektionen, fill=Geschlecht)) +
      geom_col(position = "dodge") +
      ggtitle("Anzahl der Infizierten nach Geschlecht und Altersgruppe") +
      xlab("Altersgruppe") + 
      ylab("Anzahl Infizierte")
  })
  
  
  output$geschlechterTabelle = 
    DT::renderDataTable({
      DT::datatable(women_filter_agg, 
                    colnames = c("Altersgruppe", "Weiblich infizierte"),
                    rownames = FALSE)
    })
  
  
  # Bezirke
  output$districtPlot <- renderPlot({
    selected_date <- as.Date(input$districtslider)
    cases <- covid_19_df[covid_19_df$Meldedatum <= selected_date, "AnzahlFall"]
    deaths <- covid_19_df[covid_19_df$Meldedatum <= selected_date, "AnzahlTodesfall"]
    districtNames <- covid_19_df[covid_19_df$Meldedatum <= selected_date, "Landkreis"]
    districtNames <- sub("SK Berlin ", "", districtNames)
    districtCases <- aggregate(cases, by = list(districtNames), sum)
    districtDeaths <- aggregate(deaths, by = list(districtNames), sum)
    colnames(districtCases) <- c("Bezirk", "Gesamtinfektionen")
    colnames(districtDeaths) <- c("Bezirk", "Tode")
    # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
    if(input$district_displaymode == "absolute" && input$district_displaymode2 == "districtCases") {
      District_Case_Plot <-ggplot(data = districtCases, aes(x=Bezirk, y=Gesamtinfektionen, fill=Bezirk)) +
        geom_bar(stat = "identity", position = position_dodge())+
        geom_text(aes(label=Gesamtinfektionen), vjust=0.4, hjust=1.2, color="white",
                  position = position_dodge(0.8), size=4.0)+
        scale_fill_brewer(palette = "Paired")+
        coord_flip()+
        theme_minimal()
      District_Case_Plot
    } else if (input$district_displaymode == "relative" && input$district_displaymode2 == "districtCases") {
      districtCases$Gesamtinfektionen <- round(prop.table(districtCases$Gesamtinfektionen)*100,2)
      District_Case_Plot <-ggplot(data = districtCases, aes(x=Bezirk, y=Gesamtinfektionen, fill=Bezirk)) +
        geom_bar(stat = "identity", position = position_dodge())+
        geom_text(aes(label=Gesamtinfektionen), vjust=0.4, hjust=1.2, color="white",
                  position = position_dodge(0.5), size=4.0)+
        scale_fill_brewer(palette = "Paired")+
        coord_flip()+
        theme_minimal()
      District_Case_Plot
    } else if (input$district_displaymode == "absolute" && input$district_displaymode2 == "districtDeaths") {
      District_Case_Plot <-ggplot(data = districtDeaths, aes(x=Bezirk, y=Tode, fill=Bezirk)) +
        geom_bar(stat = "identity", position = position_dodge())+
        geom_text(aes(label=Tode), vjust=0.4, hjust=1.2, color="white",
                  position = position_dodge(0.8), size=4.0)+
        scale_fill_brewer(palette = "Paired")+
        coord_flip()+
        theme_minimal()
      District_Case_Plot
    } else {
      districtDeaths$Tode <- round(prop.table(districtDeaths$Tode)*100,2)
      District_Case_Plot <-ggplot(data = districtDeaths, aes(x=Bezirk, y=Tode, fill=Bezirk)) +
        geom_bar(stat = "identity", position = position_dodge())+
        geom_text(aes(label=Tode), vjust=0.4, hjust=1.2, color="white",
                  position = position_dodge(0.5), size=4.0)+
        scale_fill_brewer(palette = "Paired")+
        coord_flip()+
        theme_minimal()
      District_Case_Plot
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)