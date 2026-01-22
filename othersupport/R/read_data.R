required_vars <- c("projecttitle", "awardnumber", "supportsource", 
                   "location", "contributiontype", "awardamount", "inkinddescription", 
                   "overallobjectives", "potentialoverlap", "startdate", "enddate", 
                   "supporttype")
start_var <- "full year start date"

read_effort <- function(file) {
  d <- readxl::read_excel(file)
  nexp <- c("shorttitle", required_vars)
  oops <- names(d)[1:13] != nexp
  e <- character()
  w <- character()
  if(any(oops)) {
    e <- c(e,
           sprintf("Name mismatch: column %s should be named '%s'.",
                   which(oops), nexp[oops]))
  }
  year_vars <- str_subset(names(d), "^year [0-9]+$")
  if(length(year_vars)==0) {
    e <- c(e, "No years of effort found.")
  }
  # other things to check??
  
  # "full year start date"
  if(!start_var %in% names(d)) {
    d[[start_var]] <- NA
  }
  
  # "method"
  if(!"method" %in% names(d)) {
    d$method <- "PRORATE"
  }
  # any(duplicated(dat$shorttitle)) 
  # awardamount is an integer
  
  # unneeded columns
  extra_vars <- setdiff(names(d), c(nexp, year_vars, start_var, "method"))
  if(length(extra_vars) > 0) {
    w <- c(w, sprintf("Ignored variables: %s", paste(paste0("'", extra_vars, "'"), sep=", ")))    
  }
  
  if(length(e)>0) {
    d <- FALSE
  } else {

    d <- d |> 
      select(-all_of(extra_vars)) |>
      mutate(awardamount=as.integer(awardamount)) |>
      rename(c(budgetdate=any_of(start_var))) |>
      mutate(budgetdate=if_else(is.na(budgetdate), startdate, budgetdate)) |>
      mutate(across(ends_with("date"), as.Date)) |>
      mutate(shorttitle=shorttitle |> replace_na("_blank_")) |>
      mutate(shorttitle=paste0(shorttitle, if(n()>1) paste0("-", 1:n()) else ""), .by=shorttitle) |>
      mutate(shorttitle=as_factor(shorttitle)) |>
      create_budget()
  }
  list(errors=e, warnings=w, data=d)
}

add_budget_dates <- function(date1, date2, date0, budget) {  
  w <- character()
  if(day(date0) == day(date2)) {
    date2 <- date2 - 1
    w <- c(w, "Moving last day back a day...")
  }
  ## get budget year start dates (this is a bit of a hack...)
  ## including the start of the year after it ends
  years <- year(date1):year(date2+1)
  budgets <- as.Date(sprintf("%d-%02d-%02d", years, month(date0), day(date0)))
  budgets <- c(date1, budgets[budgets > date1 & budgets < date2], date2+1)
  budgets <- tibble(startdate=budgets[-length(budgets)], enddate=budgets[-1]-1) |> 
    mutate(year=1:n(), .before=1)
  budget <- full_join(budgets, budget, by="year") |> arrange(year)
  tibble(budget=list(budget), warnings=list(w))
}

create_budget <- function(dat) {
  year_vars <- str_subset(names(dat), "^year [0-9]+$")
  dat |> nest(budget=all_of(year_vars)) |> 
    mutate(budget=map(budget, \(x) {
      x |> pivot_longer(all_of(year_vars), 
                        names_to=c("X", "year"), names_sep=" ",
                        values_to="effort", values_drop_na = TRUE) |>
        mutate(year=as.integer(year)) |> select(-X) |> arrange(year)
    })) |> rowwise() |>
    mutate(add_budget_dates(startdate, enddate, budgetdate, budget)) |> ungroup() |>
    ## this error checking really belongs in add_budget_dates
    ## but I can't easily combine all the results together right now
    #mutate(warnings=if_else(day(budgetdate) == day(enddate), list("Moving last day back a day..."), list(character()))) |>
    select(-budgetdate)
}


