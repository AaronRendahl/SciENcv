library(conflicted)
library(tidyverse)
library(readxl)
library(lubridate)
library(xml2)
conflicts_prefer(dplyr::filter, dplyr::lag)
source("other2_functions.R")

datx <- readxl::read_excel("sample.xlsx", "projects2") |>
  prepare_projects()

datx |> select(shorttitle, commitment) |> unnest(commitment)

datx |> dat_to_xml() |> write_xml("sample2b.xml")
