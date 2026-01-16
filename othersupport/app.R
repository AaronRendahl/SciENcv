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
current_year <- year(Sys.time())

ui <- fluidPage(
  titlePanel("SciENcv Other Support XML Conversion"),
  samplelink,
  tags$style(HTML("img {display: block; max-width: 100%; height: auto;}")),
  fluidRow(
    column(
      6,
      fileInput("upload", "Upload an Excel file with the right format:"),
      selectInput("current_year", "Select Current Year", 
                  choices = current_year + c(-10:5),
                  selected = current_year)
    ),
    column(
      6,
      tag("strong", "Then download the XML file:"),
      downloadButton("download"),
      tags$p("The XML file will only include effort from the current year forward.")
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
      data() |> dat_to_xml(current_year=as.integer(input$current_year)) |> write_xml(file)
    }
  )
  
  output$preview <- renderTable({
    datx <- data() 
    datx |> select(shorttitle, commitment) |> unnest(commitment)
  })
  
  res <- 200
  
  output$plots <- renderUI({
    p <- data()
    n <- nrow(p)
    tagList(lapply(seq_len(n), \(i) {
      message(sprintf("rendering project %d/%d...", i, n))
      wh <- getsize(p$.plot[[i]])
      budgeti <- p$.budget[[i]] |> 
        mutate("Budget Year" = 1:n(), .before=1) |>
        mutate(months=monthdiff(b1, b2), .after="b2") |>
        mutate(b2=b2-1) |>
        rename(start=b1, end=b2, "person-months"=effort) |>
        mutate(across(c(start, end), format))
      tagList(
        h2(p$shorttitle[i]),
        tag("p", sprintf("%s to %s", format(p$startdate[i]), format(p$enddate[i]))),
        if(!is.na(p$.error[i])) tags$h5(paste("WARNING:", p$.error[i])) else NULL,
        renderTable(budgeti),
        renderPlot(p$.plot[[i]], width=wh[1]*res, height=wh[2]*res, res=res)
      )
    }))
  })
  output$error_count <- renderUI({
    p <- data()
    ne <- sum(!is.na(p$.error))
    tagList(
      h2("Error checking:"),
      p(em(sprintf("%d errors found.", ne))),
      p("This app has very limited error checking, though! Be thoughtful when using it.")
    )
  })
  output$plot_all <- renderUI({
    p <- data()
    plot_all <- all_effort_plot(p) +
      theme(legend.position = "bottom")
    wh <- getsize(plot_all)
    tagList(
      h2("All Combined"),
      renderPlot(plot_all, width=wh[1]*res, height=wh[2]*res, res=res)
    )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
