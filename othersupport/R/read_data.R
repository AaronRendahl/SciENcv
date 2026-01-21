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
  years <- str_subset(names(d), "^year [0-9]+$")
  if(length(years)==0) {
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
  hmm <- setdiff(names(d), c(nexp, years, start_var, "method"))
  if(length(hmm) > 0) {
    w <- c(w, sprintf("Unneeded variables: %s", paste(hmm, sep=", ")))    
  }
  
  
  if(length(e)>0) {
    d <- FALSE
  } else {
    d <- d |> mutate(awardamount=as.integer(awardamount)) |>
      rename(c(budget=any_of(start_var))) |>
      mutate(budget=if_else(is.na(budget), startdate, budget)) |>
      mutate(shorttitle=shorttitle |> replace_na("_blank_")) |>
      mutate(shorttitle=paste0(shorttitle, if(n()>1) paste0("-", 1:n()) else ""), .by=shorttitle) |>
      mutate(shorttitle=as_factor(shorttitle))
  }
  
  list(error=e, warning=w, data=d)
}