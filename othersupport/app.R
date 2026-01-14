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

sampleurl <- "https://docs.google.com/spreadsheets/d/1h3TJNpgWc5O1S7QEUeEePHlypPz27d9sCYfHFNuKNJ4/edit?usp=sharing"
samplelink <- tags$a(href=sampleurl, "View Sample Excel format", target="_blank")

ui <- fluidPage(
  titlePanel("SciENcv Other Support XML Conversion"),
  tags$style(HTML("img {display: block; max-width: 100%; height: auto;}")),
  fluidRow(
    column(6,
           samplelink,
           fileInput("upload", "Upload an Excel file with the right format:"),
    ),
    column(6,
           tag("strong", "Then download the XML file:"),
           downloadButton("download")
    )),
  fluidRow(
    uiOutput("error_count"),
    uiOutput("plot_all"),
    uiOutput("plots")
  )
)

server <- function(input, output, session) {
  data <- reactive({
    req(input$upload)
    file <- input$upload$datapath
    readxl::read_excel(file) |> prepare_projects()
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste0(str_remove(input$upload$name, "\\.xlsx$"), ".xml")
    },
    content = function(file) {
      data() |> dat_to_xml() |> write_xml(file)
    }
  )
  
  output$preview <- renderTable({
    datx <- data() 
    datx |> select(shorttitle, commitment) |> unnest(commitment)
  })
  
  r <- 1/4
  w <- 8
  res <- 300
  
  output$plots <- renderUI({
    p <- data()
    n <- nrow(p)
    tagList(lapply(seq_len(n), \(i) {
      message(sprintf("project %d/%d...", i, n))
      budgeti <- p$.budget[[i]] |> 
        mutate("Budget Year" = 1:n(), .before=1) |>
        mutate(months=monthdiff(b1, b2), .after="b2") |>
        rename(start=b1, end=b2, "person-months"=effort) |>
        mutate(across(c(start, end), format))
      tagList(
        h2(p$shorttitle[i]),
        tag("p", sprintf("%s to %s", format(p$startdate[i]), format(p$enddate[i]))),
        if(!is.na(p$.error[i])) tags$h5(paste("WARNING:", p$.error[i])) else NULL,
        renderTable(budgeti),
        renderPlot(p$.plot[[i]], width=w*res, height=w*r*res, res=res)
      )
    }))
  })
  output$error_count <- renderUI({
    p <- data()
    ne <- sum(!is.na(p$.error))
    tag("p", sprintf("%d errors found.", ne))
  })
  output$plot_all <- renderUI({
    p <- data()
    tagList(
      h2("All Combined"),
      renderPlot(all_effort_plot(p), 
                 width=w*res, height=w*r*res, res=res)
    )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
