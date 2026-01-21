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
    uiOutput("error_read"),
    uiOutput("warning_read"),
    uiOutput("error_count"),
    uiOutput("plot_all"),
    uiOutput("plots")
  )
)

server <- function(input, output, session) {
  
  raw_data <- reactive({
    req(input$upload)
    file <- input$upload$datapath
    read_effort(file) 
  })
  
  ok <- reactive({
    !isFALSE(raw_data()$data)
  })
  
  data <- reactive({
    if(ok()) raw_data()$data |> prepare_projects()
  })
  
  daterange <- reactive({
    if(ok()) {
      dat <- raw_data()$data
      get_data_range(dat)
    }
  })
  output$error_read <- renderUI({
    e <- raw_data()$error
    if(length(e)>0) {
      errorlist <- do.call(tags$ul, lapply(e, tags$li))
      tagList(h2("Data error:"), errorlist)
    }
  })
  output$warning_read <- renderUI({
    w <- raw_data()$warning
    if(length(w)>0) {
      warninglist <- do.call(tags$ul, lapply(w, tags$li))
      tagList(h2("Data notes:"), warninglist)
    }
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
    if(ok()) {
      datx <- data() 
      datx |> select(shorttitle, commitment) |> unnest(commitment)
    }
  })
  
  res <- 200

  output$plots <- renderUI({
    if(ok()) {
    p <- data()
    n <- nrow(p)
    tagList(lapply(seq_len(n), \(i) {
      message(sprintf("rendering project %d/%d...", i, n))
      ploti <- plot_effort(p$startdate[i], p$enddate[i], p$budget[i], p$effort[[i]], daterange())
      wh <- getsize(ploti)
      budgeti <- p$.budget[[i]] |> 
        mutate("Budget Year" = 1:n(), .before=1) |>
        mutate(months=monthdiff(b1, b2+1), .after="b2") |>
        rename(start=b1, end=b2, "person-months effort"=effort) |>
        mutate(across(c(start, end), format))
      tagList(
        h2(p$shorttitle[i]),
        tag("p", sprintf("%s to %s", format(p$startdate[i]), format(p$enddate[i]))),
        if(!is.na(p$.error[i])) tags$h5(paste("WARNING:", p$.error[i])) else NULL,
        renderTable(budgeti),
        renderPlot(ploti, width=wh[1]*res, height=wh[2]*res, res=res)
      )
    }))
    }
  })
  
  output$error_count <- renderUI({
    if(ok()) {
    p <- data()
    ne <- sum(!is.na(p$.error))
    tagList(
      h2("Error checking:"),
      p(em(sprintf("%d errors found.", ne))),
      p("This app has very limited error checking, though! Be thoughtful when using it.")
    )
    }
  })
  
  output$plot_all <- renderUI({
    if(ok()) {
    p <- data()
    plot_all <- all_effort_plot(p) +
      theme(legend.position = "bottom")
    wh <- getsize(plot_all)
    tagList(
      h2("All Combined"),
      renderPlot(plot_all, width=wh[1]*res, height=wh[2]*res, res=res)
    )
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
