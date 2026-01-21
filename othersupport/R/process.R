process_effort <- function(date1, date2, effort, budget) {
  w <- c()
  effort <- effort$effort
  date1 <- as.Date(date1)
  date2 <- as.Date(date2)
  budget <- as.Date(budget)
  ## I assume date2 is the last day of the budget period, however...
  ## if end is actually on the same day of the month as the full year starts,
  ## assume last date is actually the day before what's given, and
  ## and subtract a day.
  if(day(budget) == day(date2)) {
    date2 <- date2 - 1
    w <- c(w, "Moving last day back a day...")
  }
  
  ## get calendar years (eg. 2025:2027)
  years <- year(date1):year(date2)
  
  ## get budget year start dates (this is a bit of a hack...)
  ## including the start of the year after it ends
  budgets <- as.Date(sprintf("%d-%02d-%02d", years, month(budget), day(budget)))
  budgets <- c(date1, budgets[budgets > date1 & budgets < date2], date2+1)

  ne <- length(effort)
  nb <- length(budgets) - 1L
  ## ERROR: need to have effort equal to the number of budget years  
  if(ne != nb) {
    error <- sprintf("Years with effort (%d) not equal to the number of budget years (%d): %s", ne, nb,
                     if(ne < nb) { "Setting missing effort years to zero." } 
                     else if (ne > nb) {"Discarding extra effort years." })
    effort <- effort[seq_len(nb)] |> replace_na(0)
    warning(error)
    w <- c(w, error)
  } 

  ## table of budget periods and effort
  ef <- tibble(b1=budgets[-length(budgets)], b2=budgets[-1]-1, effort=effort)
        
  ## compute how much of each budget period is in each calendar year
  ## there's got to be an easier way...
  cal1 <- ef |> mutate(cal=as.Date(sprintf("%d-01-01", year(b2)))) |>
    select(b1, cal, b2, everything()) |>
    mutate(cal=cal |> pmax(b1),
           r1=monthdiff(b1, cal), 
           r2=monthdiff(cal, b2+1),
           effort_1=effort*r1/(r1+r2), 
           effort_2=effort*r2/(r1+r2),
           start_1=b1, end_1=cal-1,
           start_2=cal, end_2=b2) |>
    mutate(budget=1:n(), year_1=year(b1), year_2=year(b2))
  cal2 <- cal1 |>
    select(budget, contains("_")) |>
    pivot_longer(contains("_"), names_to=c(".value", "X"), names_sep="_") |>
    filter(effort > 0) |> select(-X) |> relocate(effort, .after="year")

  cal <- cal2 |>
    summarize(effort=sum(effort), .by=year)
  
  tibble(calendar=list(cal), .budget=list(ef), effort=list(cal2), errors=list(w))
}


prepare_projects <- function(dat) { 
  dat |> select(shorttitle, startdate, enddate, budget, effort) |> rowwise() |>
    mutate(process_effort(startdate, enddate, effort, budget)) |> ungroup()
}
