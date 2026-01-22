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
samplelink <- tags$a(href=sampleurl, "Link to Sample Excel file", target="_blank")
current_year <- year(Sys.time())

ui <- fluidPage(
  titlePanel("SciENcv Other Support XML Conversion"),
  tags$style(HTML("img {display: block; max-width: 100%; height: auto;}")),
  fluidRow(
    column(
      6,
      p(strong("View Sample:")),
      samplelink,
      tag("p", ""),
      fileInput("upload", "Upload an Excel file:"),
    ),
    column(
      6,
      selectInput("current_year", "Select Current Year:", 
                  choices = current_year + c(-10:5),
                  selected = current_year),
      tags$p("(The XML file will only include effort from the current year forward.)"),
      tag("strong", "Download the XML file:"),
      downloadButton("download")

    )),
  fluidRow(column(12,
    uiOutput("error_read"),
    uiOutput("warning_read"),
    uiOutput("preview"),
    uiOutput("plot_all"),
    uiOutput("plots")
  ))
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
      get_data_range(data())
    }
  })
  output$error_read <- renderUI({
    e <- raw_data()$errors
    if(length(e)>0) {
      errorlist <- do.call(tags$ul, lapply(e, tags$li))
      tagList(h2("Data error:"), errorlist)
    }
  })
  output$warning_read <- renderUI({
    w <- raw_data()$warnings
    if(ok()) {
      p <- data()
      ne <- length(unlist(p$errors))
      if(ne > 0) w <- c(w, sprintf("%d per-project warnings; see below.", ne))
    }
    nw <- length(w)
    tagList(
      if(nw > 0) {
        tagList(
          h2("Potential data issues:"), 
          do.call(tags$ul, lapply(w, tags$li)),
          p(em("This app has very limited error checking, though! Be thoughtful when using it."))
        )
      }
    )
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste0(str_remove(input$upload$name, "\\.xlsx$"), ".xml")
    },
    content = function(file) {
      data() |> dat_to_xml(current_year=as.integer(input$current_year)) |> write_xml(file)
    }
  )
  
  
  res <- 200

  output$plots <- renderUI({
    if(ok()) {
    p <- data()
    n <- nrow(p)
    tagList(lapply(seq_len(n), \(i) {
      message(sprintf("rendering project %d/%d...", i, n))
      method <- p$method[i]
      budgeti <- p$budget[[i]] |> 
        mutate(months=monthdiff(startdate, enddate+1), .after="enddate") |>
        mutate(across(c(startdate, enddate), format))
      efforti <- p$effort[[i]] |>
        mutate(across(c(startdate, enddate), format))
      tagList(
        h2(p$shorttitle[i]),
        tag("p", sprintf("budget period: %s to %s", format(p$startdate[i]), format(p$enddate[i]))),
        tag("p", sprintf("using '%s' method", method)),
        if(length(p$errors[[i]])>0) {
          errorlist <- do.call(tags$ul, lapply(p$errors[[i]], tags$li))
          tagList(tags$h5("WARNING:"), errorlist)
        },
        if(method=="prorate") renderTable(budgeti) else renderTable(efforti),
        if(method=="prorate") {
          ploti <- plot_effort(p$effort[[i]], daterange())
          wh <- getsize(ploti)
          renderPlot(ploti, width=wh[1]*res, height=wh[2]*res, res=res)
        }
      )
    }))
    }
  })

  
  output$plot_all <- renderUI({
    if(ok()) {
    p <- data()
    #plot_all <- all_effort_plot(p) +
    #  theme(legend.position = "bottom")
    #wh <- getsize(plot_all)
    tagList(
      h2("All Projects"),
      renderTable({
        p |> 
          select(shorttitle, calendar) |> unnest(calendar) |>
          summarize(effort=sum(effort), .by=c(shorttitle, year)) |>
          arrange(year) |>
          pivot_wider(names_from=year, values_from=effort) |>
          arrange(shorttitle)
      }, na="")#,
      #renderPlot(plot_all, width=wh[1]*res, height=wh[2]*res, res=res)
    )
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
