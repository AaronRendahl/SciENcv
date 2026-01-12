library(conflicted)
library(tidyverse)
library(xml2)
conflicts_prefer(dplyr::filter, dplyr::lag)
source("other_functions.R")

googledrive::drive_download(
  googledrive::as_id("1GsbLoMVysEDMR5CqYRJdnz1k6O1Y2BWvTCRHZ4Upi5E"),
  path="sample.xlsx",
  overwrite=TRUE
)

support_to_xml("sample.xlsx", "sample.xml")

