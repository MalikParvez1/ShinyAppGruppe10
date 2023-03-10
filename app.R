#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Online version: https://bremaldul.shinyapps.io/shinyappgruppe10/
# @author Eshmam Dulal, Parvez Malik, Niklas Sebastian Brecht
#

library(shiny)
library(shinydashboard)
library(quantmod)
library(ggplot2)
library(dplyr)
library(lubridate)
library(readr)
library(DT)

# Read CSV
covid_19_df <- na.omit(read.csv("./RKI_COVID19_Berlin.csv"))

# Date-Dataframes
dates <- covid_19_df$Meldedatum
converted_dates <- ymd_hms(dates)
unique_converted_dates <- unique(ymd_hms(dates))



# Define UI for application 
ui <- dashboardPage(

  skin = "blue",
  dashboardHeader(title = "SARS-CoV-2 in Berlin"),
  
  dashboardSidebar(
    sidebarMenu(menuItem("Überblick", tabName = "overview"),
                menuItem("Infizierte nach Alter", tabName = "infizierteNachAlter"),
                menuItem("Infizierte nach Geschlecht", tabName = "infizierteNachGeschlecht"),
                menuItem("Infizierte nach Bezirk", tabName = "infizierteNachBezirk"))
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
      tabItem(tabName = "infizierteNachAlter", h2("Infektionen nach Alter"),
              box(
                sliderInput("ageslider", "Datenbereich:",
                            min = min(unique_converted_dates),
                            max = max(unique_converted_dates),
                            value = max(unique_converted_dates)),
                "Hier kann der gewünschte beobachtete Zeitraum eingestellt werden",
              ),
              box(
                selectInput("display2", "Werteauswahl:", c("Infektionen" = "infection", "Tode" = "deathCase")),
                selectInput("display", "Darstellungsart:", c("Absolute Werte" = "absWerte", "Relative Werte" = "relWerte")),
              ),
              fluidRow(
                # https://stackoverflow.com/questions/69926478/figure-layout-within-shiny-app-in-r-making-the-layout-more-concise
                box(column(width = 12,h3("Grafische Darstellung"), plotOutput("ageGroupPlot", width="100%"))),
                box(column(width = 6,h3("Tabellenwerte"), tableOutput("ageGroupTable"))))),
                
      
      tabItem(tabName = "infizierteNachGeschlecht", h2("Infektionen nach Geschlecht"),
              sidebarLayout(
                sidebarPanel(
                  selectInput(inputId = "geschlecht",
                              label = "Geschlecht auswählen:",
                              choices = c("Männlich" = "m", "Weiblich" = "w")),
                  radioButtons(inputId = "relabs",
                               label = "Wählen Sie eine Häufigkeit aus",
                               choices = c("Absolute Häufigkeit" = "abs", "Relative Häufigkeit" = "rel"))
                ),
                mainPanel(
                  plotOutput("sexGroupPlot"),
                  tabsetPanel(
                    id="dataset",
                    tabPanel("Männlich", DT::dataTableOutput("m_table")),
                    tabPanel("Weiblich", DT::dataTableOutput("w_table")))
                )
              )),
      
      tabItem(tabName = "infizierteNachBezirk",h2("Infektionen nach Bezirk"),
              fluidRow(
                box(plotOutput("districtPlot")),
                box(plotOutput("districtPlot2"))
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
                )),
              fluidRow(
                box(plotOutput("disctrictPlot3")),
                box(
                  checkboxGroupInput("districtselection", "Waehle beliebige Bezirke um sie miteinander zu vergleichen:", 
                                     c("SK Berlin Lichtenberg", "SK Berlin Mitte", "SK Berlin Pankow", "SK Berlin Spandau",
                                       "SK Berlin Steglitz-Zehlendorf", "SK Berlin Neukölln", "SK Berlin Tempelhof-Schöneberg",
                                       "SK Berlin Treptow-Köpenick", "SK Berlin Marzahn-Hellersdorf", "SK Berlin Reinickendorf",
                                       "SK Berlin Charlottenburg-Wilmersdorf", "SK Berlin Friedrichshain-Kreuzberg"))
                )
              ))
    )
  )
)



# Define server logic 
server <- function(input, output) {
  
  # Overview
  # https://plotly.com/ggplot2/geom_line/
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
  
  
  
  # Inspiration https://github.com/gadenbuie/tweet-conf-dash
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
  
  
  # Infektionen nach Alter
  output$ageGroupPlot <- renderPlot({
    selected_date <- as.Date(input$ageslider)
    ages <- covid_19_df[covid_19_df$Meldedatum <= selected_date, "Altersgruppe"]
    cases <- covid_19_df[covid_19_df$Meldedatum <= selected_date, "AnzahlFall"]
    deathCases <- covid_19_df[covid_19_df$Meldedatum <= selected_date, "AnzahlTodesfall"]
    age_cases_sum <- aggregate(cases, by = list(ages), sum)
    age_death <- aggregate(deathCases, by = list(ages), sum)
    colnames(age_cases_sum) <- c("Altersgruppe", "Gesamtinfektionen")
    colnames(age_death) <- c("Altersgruppe", "Gesamttodesfälle")
    
    # Gilt für alle ggplot2 Barplots
    # http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization

    if(input$display == "absWerte" && input$display2 == "infection"){
      Agegroup_Case_Plot<-ggplot(data = age_cases_sum, aes(x=Altersgruppe, y=Gesamtinfektionen, fill=Altersgruppe)) +
        geom_bar(stat = "identity", position = position_dodge())+
        geom_text(aes(label=Gesamtinfektionen), vjust=1.6, color="white",
                  position = position_dodge(0.9), size=3.5)+
        scale_fill_brewer(palette = "Blues")+
        theme_minimal()
      Agegroup_Case_Plot
    } else if(input$display == "relWerte" && input$display2 == "infection"){
      age_cases_sum$Gesamtinfektionen <- round(prop.table(age_cases_sum$Gesamtinfektionen)*100,2)
      Agegroup_Case_Plot<-ggplot(data = age_cases_sum, aes(x=Altersgruppe, y=Gesamtinfektionen, fill=Altersgruppe)) +
        geom_bar(stat = "identity", position = position_dodge())+
        geom_text(aes(label=Gesamtinfektionen), vjust=1.6, color="white",
                  position = position_dodge(0.9), size=3.5)+
        scale_fill_brewer(palette = "Blues")+
        theme_minimal()
      Agegroup_Case_Plot
    } else if(input$display == "absWerte" && input$display2 == "deathCase"){
      Agegroup_Case_Plot<-ggplot(data = age_death, aes(x=Altersgruppe, y=Gesamttodesfälle, fill=Altersgruppe)) +
        geom_bar(stat = "identity", position = position_dodge())+
        geom_text(aes(label=Gesamttodesfälle), vjust=1.6, color="white",
                  position = position_dodge(0.9), size=3.5)+
        scale_fill_brewer(palette = "Blues")+
        theme_minimal()
      Agegroup_Case_Plot
    }else{
      age_death$Gesamttodesfälle <- round(prop.table(age_death$Gesamttodesfälle)*100,2)
      Agegroup_Case_Plot<-ggplot(data = age_death, aes(x=Altersgruppe, y=Gesamttodesfälle, fill=Altersgruppe)) +
        geom_bar(stat = "identity", position = position_dodge())+
        geom_text(aes(label=Gesamttodesfälle), vjust=1.6, color="white",
                  position = position_dodge(0.9), size=3.5)+
        scale_fill_brewer(palette = "Blues")+
        theme_minimal()
      Agegroup_Case_Plot
    }
  })
  
  output$ageGroupTable <- renderTable({
    selected_date <- as.Date(input$ageslider)
    tableAge <- covid_19_df[covid_19_df$Meldedatum <= selected_date, "Altersgruppe"]
    tableCases <- covid_19_df[covid_19_df$Meldedatum <= selected_date, "AnzahlFall"]
    tableDeaths <- covid_19_df[covid_19_df$Meldedatum <= selected_date, "AnzahlTodesfall"]
    table_age_cases_sum <- aggregate(tableCases, by = list(tableAge), sum)
    table_death_cases_sum <- aggregate(tableDeaths, by = list(tableAge), sum)
    colnames(table_age_cases_sum) <- c("Altersgruppe", "Gesamtinfektionen")
    colnames(table_death_cases_sum) <- c("Altersgruppe", "Gesamttodesfälle")
    table_age_cases_sum$Altersgruppe <- gsub("A", "", table_age_cases_sum$Altersgruppe)
    if(input$display == "absWerte" && input$display2 == "infection"){
      colnames(table_age_cases_sum) <- c("Altersgruppe in Jahren", "Anzahl der Gesamtinfektionen")
      table_age_cases_sum
    } else if (input$display == "relWerte" && input$display2 == "infection") {
      table_age_cases_sum$Gesamtinfektionen <- round(prop.table(table_age_cases_sum$Gesamtinfektionen)*100,2)
      colnames(table_age_cases_sum) <- c("Altersgruppe in Jahren", "Gesamtinfektionen in Prozent")
      table_age_cases_sum
    } else if (input$display == "absWerte" && input$display2 == "deathCase") {
      colnames(table_death_cases_sum) <- c("Altersgruppe in Jahren", "Anzahl der Gesamttodesfälle")
      table_death_cases_sum
    } else {
      table_death_cases_sum$Gesamttodesfälle <- round(prop.table(table_death_cases_sum$Gesamttodesfälle)*100,2)
      colnames(table_death_cases_sum) <- c("Altersgruppe in Jahren", "Gesamttodesfälle in Prozent")
      table_death_cases_sum
    }
  })

  # Infektionen nach Geschlecht
  
  #https://towardsdatascience.com/end-to-end-shiny-app-tutorial-using-nyc-mortality-data-d29ad99506b9
  output$sexGroupPlot <- renderPlot({
    data_infiziert_w <- subset(covid_19_df, Geschlecht=="W" ,select = c(Altersgruppe, AnzahlFall))
    data_infiziert_m <- subset(covid_19_df, Geschlecht=="M" ,select = c(Altersgruppe, AnzahlFall))
    data_w_agg <- aggregate(AnzahlFall ~ Altersgruppe, data_infiziert_w, FUN=sum)
    colnames(data_w_agg) <- c("Altersgruppe", "Gesamtinfektionen")
    data_m_agg <- aggregate(AnzahlFall ~ Altersgruppe, data_infiziert_m, FUN=sum)
    colnames(data_m_agg) <- c("Altersgruppe", "Gesamtinfektionen")
    
    if(input$geschlecht == "w" && input$relabs == "abs"){
      ggplot(data = data_w_agg, aes(x=Altersgruppe, y=Gesamtinfektionen, fill=Altersgruppe)) +
        geom_bar(stat = "identity",position = "dodge", color = "red", fill = "red") +
        geom_text(aes(label=Gesamtinfektionen), vjust=2, colour="white")+
        labs(
          title = "Infizierte nach Geschlecht",
          x = "Altersgruppen",
          y = "Anzahl der Infizierten"
        )
    } else if(input$geschlecht == "w" && input$relabs == "rel"){
      data_w_agg$Gesamtinfektionen <- round(prop.table(data_w_agg$Gesamtinfektionen)*100,2)
      ggplot(data = data_w_agg, aes(x=Altersgruppe, y=Gesamtinfektionen, fill=Altersgruppe)) +
        geom_bar(stat = "identity", color = "red", fill = "red") +
        geom_text(aes(label=Gesamtinfektionen), vjust=2, colour="white")+
        labs(
          title = "Infizierte nach Geschlecht",
          x = "Altersgruppen",
          y = "Anzahl der Infizierten"
        )
    } else if(input$geschlecht == "m" && input$relabs == "rel"){
      data_m_agg$Gesamtinfektionen <- round(prop.table(data_m_agg$Gesamtinfektionen)*100,2)
      ggplot(data = data_m_agg, aes(x=Altersgruppe, y=Gesamtinfektionen, fill=Altersgruppe)) +
        geom_bar(stat = "identity", position = "dodge" ,color = "steelblue", fill = "steelblue") +
        geom_text(aes(label=Gesamtinfektionen), vjust=2, colour="white")+
        labs(
          title = "Infizierte nach Geschlecht",
          x = "Altersgruppen",
          y = "Anzahl der Infizierten"
        )
    }
    else{
      ggplot(data = data_m_agg, aes(x=Altersgruppe, y=Gesamtinfektionen, fill=Altersgruppe)) +
        geom_bar(stat = "identity", position = "dodge" ,color = "steelblue", fill = "steelblue") +
        geom_text(aes(label=Gesamtinfektionen), vjust=2, colour="white")+
        labs(
          title = "Infizierte nach Geschlecht",
          x = "Altersgruppen",
          y = "Anzahl der Infizierten"
        )
    }
  })
  
  age <- covid_19_df$Altersgruppe
  cases <- covid_19_df$AnzahlFall
  data_infiziert_w <- subset(covid_19_df, Geschlecht=="W" ,select = c(Altersgruppe, AnzahlFall))
  data_infiziert_m <- subset(covid_19_df, Geschlecht=="M" ,select = c(Altersgruppe, AnzahlFall))
  data_w_agg <- aggregate(AnzahlFall ~ Altersgruppe, data_infiziert_w, FUN=sum)
  colnames(data_w_agg) <- c("Altersgruppe", "Gesamtinfektionen")
  data_m_agg <- aggregate(AnzahlFall ~ Altersgruppe, data_infiziert_m, FUN=sum)
  colnames(data_m_agg) <- c("Altersgruppe", "Gesamtinfektionen")
  
  output$m_table <- DT::renderDataTable(
    DT::datatable(data_m_agg)
  )
  output$w_table <- DT::renderDataTable(
    DT::datatable(data_w_agg)
  )
  
  # Infektionen nach Bezirken

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
      District_Case_Plot <- ggplot(data = districtDeaths, aes(x=Bezirk, y=Tode, fill=Bezirk)) +
        geom_bar(stat = "identity", position = position_dodge())+
        geom_text(aes(label=Tode), vjust=0.4, hjust=1.2, color="white",
                  position = position_dodge(0.5), size=4.0)+
        scale_fill_brewer(palette = "Paired")+
        coord_flip()+
        theme_minimal()
      District_Case_Plot
    }
  })
  
  output$districtPlot2 <- renderPlot({
    Datum <- as.Date(input$districtslider)
    cases <- covid_19_df[covid_19_df$Meldedatum <= Datum, "AnzahlFall"]
    deaths <- covid_19_df[covid_19_df$Meldedatum <= Datum, "AnzahlTodesfall"]
    districtNames <- covid_19_df[covid_19_df$Meldedatum <= Datum, "Landkreis"]
    districtNames <- sub("SK Berlin ", "", districtNames)
    districtCases <- aggregate(cases, by = list(districtNames), sum)
    districtDeaths <- aggregate(deaths, by = list(districtNames), sum)
    colnames(districtCases) <- c("Bezirk", "Inzidenz")
    colnames(districtDeaths) <- c("Bezirk", "Tode")
    # Einwohnerdaten aus gegebener SB_A01-05-00_2022h01_BE.xlsx
    einwohner <- c(339405, 291851, 304485, 281566, 391831, 329037, 418249, 267398, 251588, 310454, 351567, 284450)
    
    if(input$district_displaymode2 == "districtCases") {
      districtCases$Inzidenz <- round(districtCases$Inzidenz / einwohner * 1000, 1)
      # Hat die Idee gebracht, um aufsteigend zu sortieren. https://stackoverflow.com/questions/3744178/ggplot2-sorting-a-plot
      districtCases <- districtCases[order(districtCases$Inzidenz),]
      districtCases$Bezirk <- factor(districtCases$Bezirk, levels = districtCases$Bezirk)
      
      
      districtperT <- ggplot(districtCases, aes(x = Bezirk, y = Inzidenz, fill = Inzidenz)) +
        geom_bar(color = "black", linewidth = 0.5, linetype = 1, stat = "identity") +
        geom_text(aes(label=Bezirk), color="white", angle = 90, vjust = 0.5, hjust = 1.10,
                  position = position_dodge(0.5), size=3.5)+
        geom_text(aes(label=Inzidenz), vjust=-0.5, color="black",
                  position = position_dodge(0.9), size=2.5)+
        scale_fill_gradient(low = "#ff9a91", high = "#fc3d3d") +
        theme(axis.text.x = element_text(color = "transparent")) +
        ggtitle("Gesamtinfektionen pro 1000 Einwohner") +
        guides(fill = guide_colourbar(title = "Inzidenz"))
      districtperT
  } else {
      districtDeaths$Tode <- round(districtDeaths$Tode / einwohner * 1000, 1)
      districtDeaths <- districtDeaths[order(districtDeaths$Tode),]
      districtDeaths$Bezirk <- factor(districtDeaths$Bezirk, levels = districtDeaths$Bezirk)
      
      districtperT <- ggplot(districtDeaths, aes(x = Bezirk, y = Tode, fill = Tode)) +
        geom_bar(color = "black", linewidth = 0.5, linetype = 1, stat = "identity") +
        geom_text(aes(label=Bezirk), color="white", angle = 90, vjust = 0.5, hjust = 1.10,
                  position = position_dodge(0.5), size=3.5)+
        geom_text(aes(label=Tode), vjust=-0.5, color="black",
                  position = position_dodge(0.9), size=2.5)+
        scale_fill_gradient(low = "#ff9a91", high = "#fc3d3d") +
        theme(axis.text.x = element_text(color = "transparent")) +
        ggtitle("Tode pro 1000 Einwohner") +
        guides(fill = guide_colourbar(title = "Inzidenz"))
        districtperT
  }
  })
  
  output$disctrictPlot3 <- renderPlot({
    district_date_case <- covid_19_df %>%
      group_by(Landkreis, Meldedatum) %>%
      summarise(cases = sum(AnzahlFall, na.rm = TRUE))
    
    
    # Entschuldigen Sie das Chaos. Wir haben stundenlang an for-Schleifen verschwendet. Leider haben wir es nun so gelöst.
    
    districts <- c("SK Berlin Lichtenberg", "SK Berlin Mitte", "SK Berlin Pankow", "SK Berlin Spandau",
                 "SK Berlin Steglitz-Zehlendorf", "SK Berlin Neukölln", "SK Berlin Tempelhof-Schöneberg",
                 "SK Berlin Treptow-Köpenick", "SK Berlin Marzahn-Hellersdorf", "SK Berlin Reinickendorf",
                 "SK Berlin Charlottenburg-Wilmersdorf", "SK Berlin Friedrichshain-Kreuzberg")
    
    cw <- subset(district_date_case, Landkreis == "SK Berlin Charlottenburg-Wilmersdorf")
    fk <- subset(district_date_case, Landkreis == "SK Berlin Friedrichshain-Kreuzberg")
    lb <- subset(district_date_case, Landkreis == "SK Berlin Lichtenberg")
    mt <- subset(district_date_case, Landkreis == "SK Berlin Mitte")
    pk <- subset(district_date_case, Landkreis == "SK Berlin Pankow")
    sp <- subset(district_date_case, Landkreis == "SK Berlin Spandau")
    sz <- subset(district_date_case, Landkreis == "SK Berlin Steglitz-Zehlendorf")
    nk <- subset(district_date_case, Landkreis == "SK Berlin Neukölln")
    th <- subset(district_date_case, Landkreis == "SK Berlin Tempelhof-Schöneberg")
    tk <- subset(district_date_case, Landkreis == "SK Berlin Treptow-Köpenick")
    mz <- subset(district_date_case, Landkreis == "SK Berlin Marzahn-Hellersdorf")
    rd <- subset(district_date_case, Landkreis == "SK Berlin Reinickendorf")
    cw <- subset(district_date_case, Landkreis == "SK Berlin Charlottenburg-Wilmersdorf")
    
    id <- covid_19_df %>%
      aggregate(AnzahlFall ~ Meldedatum, sum, na.rm=TRUE)
    
    # https://stackoverflow.com/questions/23518605/add-an-index-numeric-id-column-to-large-data-frame
    id$ID <- seq.int(nrow(id))
    cw$ID <- seq.int(nrow(cw))
    
    id$ID <- seq.int(nrow(id))
    fk$ID <- seq.int(nrow(fk))
    
    id$ID <- seq.int(nrow(id))
    lb$ID <- seq.int(nrow(lb))
    
    id$ID <- seq.int(nrow(id))
    mt$ID <- seq.int(nrow(mt))
    
    id$ID <- seq.int(nrow(id))
    pk$ID <- seq.int(nrow(pk))
    
    id$ID <- seq.int(nrow(id))
    sp$ID <- seq.int(nrow(sp))
    
    id$ID <- seq.int(nrow(id))
    sz$ID <- seq.int(nrow(sz))
    
    id$ID <- seq.int(nrow(id))
    nk$ID <- seq.int(nrow(nk))
    
    id$ID <- seq.int(nrow(id))
    th$ID <- seq.int(nrow(th))
    
    id$ID <- seq.int(nrow(id))
    tk$ID <- seq.int(nrow(tk))
    
    id$ID <- seq.int(nrow(id))
    mz$ID <- seq.int(nrow(mz))
    
    id$ID <- seq.int(nrow(id))
    rd$ID <- seq.int(nrow(rd))
    
    id$ID <- seq.int(nrow(id))
    cw$ID <- seq.int(nrow(cw))
   
    
      districtPlot3 <- ggplot()
      
      # https://stackoverflow.com/questions/22915337/if-else-condition-in-ggplot-to-add-an-extra-layer
      # Hat nur die Idee gebracht, das man if Statements verwenden kann, um so nach und nach einen Plot zu konfigurieren
      if("SK Berlin Charlottenburg-Wilmersdorf" %in% input$districtselection) {
        districtPlot3 <- districtPlot3 + geom_line(data = cw, aes(x = ID, y = cases), col = "red")
      }
      if("SK Berlin Friedrichshain-Kreuzberg" %in% input$districtselection) {
        districtPlot3 <- districtPlot3 + geom_line(data = fk, aes(x = ID, y = cases), col = "blue")
      }
      if("SK Berlin Lichtenberg" %in% input$districtselection) {
        districtPlot3 <- districtPlot3 + geom_line(data = lb, aes(x = ID, y = cases), col = "green")
      }
      if("SK Berlin Mitte" %in% input$districtselection) {
        districtPlot3 <- districtPlot3 + geom_line(data = mt, aes(x = ID, y = cases), col = "purple")
      }
      if("SK Berlin Pankow" %in% input$districtselection) {
        districtPlot3 <- districtPlot3 + geom_line(data = pk, aes(x = ID, y =cases), col = "yellow")
      }
      if("SK Berlin Spandau" %in% input$districtselection) {
        districtPlot3 <- districtPlot3 + geom_line(data = sp, aes(x = ID, y = cases), col = "orange")
      }
      if("SK Berlin Steglitz-Zehlendorf" %in% input$districtselection) {
        districtPlot3 <- districtPlot3 + geom_line(data = sz, aes(x = ID, y = cases), col = "darkgreen")
      }
      if("SK Berlin Neukölln" %in% input$districtselection) {
        districtPlot3 <- districtPlot3 + geom_line(data = nk, aes(x = ID, y = cases), col = "darkblue")
      }
      if("SK Berlin Tempelhof-Schöneberg" %in% input$districtselection) {
        districtPlot3 <- districtPlot3 + geom_line(data = th, aes(x = ID, y = cases), col = "darkred")
      }
      if("SK Berlin Treptow-Köpenick" %in% input$districtselection) {
        districtPlot3 <- districtPlot3 + geom_line(data = tk, aes(x = ID, y = cases), col = "darkgray")
      }
      if("SK Berlin Marzahn-Hellersdorf" %in% input$districtselection) {
        districtPlot3 <- districtPlot3 + geom_line(data = mz, aes(x = ID, y = cases), col = "lightblue")
      }
      if("SK Berlin Reinickendorf" %in% input$districtselection) {
        districtPlot3 <- districtPlot3 + geom_line(data = rd, aes(x = ID, y = cases), col = "lightgreen")
      }
      if("SK Berlin Charlottenburg-Wilmersdorf" %in% input$districtselection) {
        districtPlot3 <- districtPlot3 + geom_line(data = cw, aes(x = ID, y = cases), col = "black")
      }
      districtPlot3 <- districtPlot3+
           labs(x="Zeitraum", y="Infektionen", title = "Infektionen im zeitlichen Verlauf")

      districtPlot3
  })
    
}

# Run the application 
shinyApp(ui = ui, server = server)