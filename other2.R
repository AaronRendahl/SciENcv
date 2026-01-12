library(conflicted)
library(tidyverse)
library(readxl)
library(lubridate)
library(xml2)
conflicts_prefer(dplyr::filter, dplyr::lag)
source("other2_functions.R")

dat <- readxl::read_excel("sample.xlsx", "projects2") |>
  mutate(across(c(startdate, enddate), as.Date)) |>
  rename(prorate="prorate?") |>
  mutate(prorate=(prorate |> str_sub(1, 1) |> replace_na("N"))=="Y")

dat1 <- dat[2,]

d1 <- dat1 |> select(shorttitle, !starts_with("year_"), -"prorate")
d2 <- dat1 |> select(shorttitle, starts_with("year_"))

prorate <- dat1$prorate
date1 <- dat1$startdate
date2 <- dat1$enddate
effort <- d2 |> pivot_longer(starts_with("year_"), names_to=c("X", "year"), names_sep="_", values_to="effort") |>
  select(year, effort) |> filter(!is.na(effort)) |> pull(effort)


