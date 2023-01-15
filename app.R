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
    sidebarMenu(menuItem("Overview", tabName = "overview"),
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
                selectInput("display", "Darstellungsart:", c("Absolute Werte" = "absWerte", "Relative Werte" = "relWerte")),
              ),
              box(
                selectInput("display2", "Werteauswahl:", c("Infektionen" = "infection", "Tode" = "deathCase")),
              ),
              fluidRow(
                #https://stackoverflow.com/questions/69926478/figure-layout-within-shiny-app-in-r-making-the-layout-more-concise
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
                    tabPanel("weiblich", DT::dataTableOutput("w_table")))
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
    deathCases <- covid_19_df$AnzahlTodesfall
    age_cases_sum <- aggregate(cases, by = list(ages), sum)
    age_death <- aggregate(deathCases, by = list(ages), sum)
    colnames(age_cases_sum) <- c("Altersgruppe", "Gesamtinfektionen")
    colnames(age_death) <- c("Altersgruppe", "Gesamttodesfälle")
    
    
    if(input$display == "absWerte" && input$display2 == "infection"){
      # http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization
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
      # http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization
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
    tableAge <- covid_19_df$Altersgruppe
    tableCases <- covid_19_df$AnzahlFall
    tableDeaths <- covid_19_df$AnzahlTodesfall
    table_age_cases_sum <- aggregate(tableCases, by = list(tableAge), sum)
    table_death_cases_sum <- aggregate(tableDeaths, by = list(tableAge), sum)
    colnames(table_age_cases_sum) <- c("Altersgruppe", "Gesamtinfektionen")
    colnames(table_death_cases_sum) <- c("Altersgruppe", "Gesamttodesfälle")
    # table_age_cases_sum$Altersgruppe <- gsub("A", "", table_age_cases_sum$Altersgruppe)
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

  # Geschlecht
  
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
        ggtitle("Infektionen pro 1000 Einwohner") +
        guides(fill = guide_colourbar(title = "Inzidenz"))
      districtperT
  } else {
      districtDeaths$Tode <- round(districtDeaths$Tode / einwohner * 1000, 1)
      # Hat die Idee gebracht, um aufsteigend zu sortieren. https://stackoverflow.com/questions/3744178/ggplot2-sorting-a-plot
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
}

# Run the application 
shinyApp(ui = ui, server = server)