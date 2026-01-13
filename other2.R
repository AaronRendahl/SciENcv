library(conflicted)
library(tidyverse)
library(readxl)
library(lubridate)
library(xml2)
conflicts_prefer(dplyr::filter, dplyr::lag)
source("othersupport/R/other2_functions.R")


googledrive::drive_download(
  googledrive::as_id("1h3TJNpgWc5O1S7QEUeEePHlypPz27d9sCYfHFNuKNJ4"),
  path="sample_v3.xlsx",
  overwrite=TRUE
)

file_xlsx <- "sample_v3.xlsx"
d <- readxl::read_excel(file_xlsx)
p <- d |> prepare_projects()

p$.plot[[1]]
all_effort_plot(p)


p |> dat_to_xml() |> write_xml("sample2b.xml", options=c("format", "no_declaration"))

