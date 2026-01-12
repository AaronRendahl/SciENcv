library(conflicted)
library(tidyverse)
library(xml2)
conflicts_prefer(dplyr::filter, dplyr::lag)

source("other_functions.R")

sample <- "1GsbLoMVysEDMR5CqYRJdnz1k6O1Y2BWvTCRHZ4Upi5E"
support_xlsx <- "sample.xlsx"
googledrive::drive_download(
  googledrive::as_id(sample),
  path=support_xlsx,
  overwrite=TRUE
)

#d1 <- read_identification(support_xlsx)
d2 <- read_projects(support_xlsx)

list(profile=list(
  identification=list(),
  employment=list(),
  funding=projects_to_xml(d2)
)) |> 
  as_xml_document() |> write_xml("temp.xml")

