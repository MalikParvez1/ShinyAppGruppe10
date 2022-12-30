library(shiny)
library(shinydashboard)

ui <- fluidPage(
  
  navbarPage("Covid",
             tabPanel("Startseite",
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons("plotType", "Plot type",
                                       c("Scatter"="p", "Line"="l")
                          )
                        ),
                        mainPanel(
                          plotOutput("plot")
                        )
                      )
             ),
             tabPanel("Bezirke",
                      sidebarPanel(
                        helpText("Hier können Sie den verlauf der Covidinfektion in den einzelnen Bezirken verfolgen."),
                        
                        selectInput("var", 
                                    label = "Wählen Sie einen Bezirk aus",
                                    choices = list("Wedding", 
                                                   "Neukölln"),
                                    selected = "Percent White"),
                        
                        sliderInput("range", 
                                    label = "Range of interest:",
                                    min = 0, max = 100, value = c(0, 100))
                      )
             )
  )

)

server <- function(input, output) {

}

shinyApp(ui = ui, server = server)
