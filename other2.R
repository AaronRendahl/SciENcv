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

#file_xlsx <- "sample_v3.xlsx"
file_xlsx <- "Sample Other Support v3.xlsx"

for(f in list.files(path="othersupport/R", full.names = TRUE)) source(f)
d <- read_effort(file_xlsx)
dat <- d$data
p <- dat |> prepare_projects()

rr <- get_data_range(p)
plot_effort(p$effort[[1]], rr)
plot_effort(p$effort[[2]], rr)
plot_effort(p$effort[[3]], rr)
plot_effort(p$effort[[4]], rr)
plot_effort(p$effort[[5]], rr)

all_effort_plot(p)



p |> dat_to_xml() |> write_xml("sample2b.xml", options=c("format", "no_declaration"))

