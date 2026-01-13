#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(conflicted)
library(tidyverse)
library(readxl)
library(lubridate)
library(xml2)
library(shiny)
conflicts_prefer(dplyr::filter, dplyr::lag)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("SciENcv Other Support XML Conversion"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          fileInput("upload", "Upload an Excel file with the right format:"),
          "Then download the XML file:",
          downloadButton("download"),
          '<p><a href="https://docs.google.com/spreadsheets/d/1h3TJNpgWc5O1S7QEUeEePHlypPz27d9sCYfHFNuKNJ4/edit?usp=sharing">Sample spreadsheet format</a></p>'
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tableOutput("files"),
          "Computed Person-Month Effort Per Year, Per Project:",
          tableOutput("preview")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  data <- reactive({
    req(input$upload)
    readxl::read_excel(input$upload$datapath)
  })
  
    output$files <- renderTable(input$upload)
    
    output$preview <- renderTable({
      datx <- data() |> prepare_projects()
      datx |> select(shorttitle, commitment) |> unnest(commitment)
    })
    output$download <- downloadHandler(
      filename = function() {
        paste0(str_remove(input$upload$name, "\\.xlsx$"), ".xml")
      },
      content = function(file) {
        data() |> prepare_projects() |> dat_to_xml() |> write_xml(file)
      }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
