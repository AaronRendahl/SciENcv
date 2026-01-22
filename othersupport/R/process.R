

effort_prorate <- function(budget) {
  date1 <- min(budget$startdate, na.rm=TRUE)
  date2 <- max(budget$enddate, na.rm=TRUE)
  
  ## get calendar years (eg. 2025:2027)
  years <- year(date1):year(date2)

  ne <- sum(!is.na(budget$effort))
  nb <- sum(!is.na(budget$startdate))
  e <- character()
  ## ERROR: need to have effort equal to the number of budget years  
  if(ne != nb) {
    error <- sprintf("Years with effort (%d) not equal to the number of budget years (%d): %s", ne, nb,
                     if(ne < nb) { "Setting missing effort years to zero." } 
                     else if (ne > nb) {"Discarding extra effort years." })
    budget <- budget |> filter(!is.na(startdate)) |> mutate(effort=replace_na(effort, 0))
    warning(error)
    e <- c(e, error)
  } 
        
  ## compute how much of each budget period is in each calendar year
  ## there's got to be an easier way...
  effort <- budget |> mutate(cal=as.Date(sprintf("%d-01-01", year(enddate)))) |>
    select(startdate, cal, enddate, everything()) |>
    mutate(cal=cal |> pmax(startdate),
           r1=monthdiff(startdate, cal), 
           r2=monthdiff(cal, enddate+1),
           effort_1=effort*r1/(r1+r2), 
           effort_2=effort*r2/(r1+r2),
           startdate_1=startdate, enddate_1=cal-1,
           startdate_2=cal, enddate_2=enddate) |>
    mutate(year=1:n(), calendar_1=year(startdate), calendar_2=year(enddate)) |>
    select(year, contains("_")) |>
    pivot_longer(contains("_"), names_to=c(".value", "X"), names_sep="_") |>
    filter(effort > 0) |> select(-X) |> relocate(effort, .after="calendar") |>
    mutate(calendar=as.integer(calendar))

  calendar <- effort |>
    summarize(effort=sum(effort), .by=calendar) |> rename(year=calendar)
  
  tibble(calendar=list(calendar), effort=list(effort), errors=list(e))
}

effort_start <- function(budget) {
  effort <- budget |> mutate(calendar=year(startdate) |> as.integer()) |> 
    relocate("calendar", .before="effort")
  calendar <- effort |> select(year=calendar, effort)
  e <- character()
  if(any(duplicated(calendar$year))) {
    e <- c(e, "Calendar year duplicated.")
  }
  tibble(calendar=list(calendar), effort=list(effort), errors=list(e))
}

effort_end <- function(budget) {
  effort <- budget |> mutate(calendar=year(enddate) |> as.integer()) |> 
    relocate("calendar", .before="effort")
  calendar <- effort |> select(year=calendar, effort)
  e <- character()
  if(any(duplicated(calendar$year))) {
    e <- c(e, "Calendar year duplicated.")
  }
  tibble(calendar=list(calendar), effort=list(effort), errors=list(e))
}

create_effort <- function(budget, method=c("prorate", "start", "end")) {
  method <- match.arg(method)
  case_when(method=="prorate" ~ effort_prorate(budget),
            method=="start" ~ effort_start(budget),
            method=="end" ~ effort_end(budget))
}


prepare_projects <- function(dat) { 
  dat |> rowwise() |> mutate(create_effort(budget, method)) |> ungroup()
}
