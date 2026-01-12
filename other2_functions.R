get_effort <- function(date1, date2, effort, prorate) {
  ## if ends on the first day of the year, the last day must have been last of the previous
  years <- year(date1):year(date2-(yday(date2)==1L)*1L)
  ## don't prorate
  if(!prorate) {
    if(length(effort)!=length(years)) {
      stop("Need as many effort as years.")
    }
    effort1 <- tibble(year=years, effort=effort)
  } else {
    ## do prorate
    ## only smart enough so far to prorate if
    ## starts and ends on same day of the year
    ## AND starts on the first of the month
    if(!(day(date1) == 1 & day(date2) == 1)) {
      stop("Can only prorate if start and end on first of month.")
    }
    if(month(date1) != month(date2)) {
      stop("Can only prorate if start and end on the same day of the year.")
    }
    m <- month(date1)
    ## don't need to prorate if started on first of the year...
    if(m==1) {
      if(length(effort)!=length(years)) {
        stop("Need as many effort as years.")
      }
      effort1 <- tibble(year=years, effort=effort)
    } else {
      if(length(effort)!=length(years)-1) {
        stop("Need as many effort as years.")
      }
      p <- (m-1)/12
      effort1 <- tibble(year=years, effort1=c(effort,0), effort2=c(0, effort)) |>
        mutate(effort=effort1*(1-p) + effort2*p)
    }
  }
  effort1 |> select("year", "effort") |> 
    mutate(effort=round(effort, 3))
}

add_commitment <- function(dat) {
  row2list <- function(dat1) {
    d1 <- dat1 |> select(shorttitle, !starts_with("year_"), -"prorate")
    d2 <- dat1 |> select(shorttitle, starts_with("year_"))
    ## grab the effort columns
    effort <- d2 |> pivot_longer(starts_with("year_"), names_to=c("X", "year"), 
                                 names_sep="_", values_to="effort") |>
      select(year, effort) |> arrange(year) |> filter(!is.na(effort)) |> pull(effort)
    ## if there is any effort, process it and add it to the data set
    if(length(effort) > 0) {
      ef <- get_effort(dat1$startdate, dat1$enddate, effort, dat1$prorate)
      d1$commitment <- list(ef)
    }
    d1
  }
  bind_rows(lapply(seq_len(nrow(dat)), \(idx) row2list(dat[idx,])))
}

row_to_xml <- function(dat1) {
  d1 <- dat1 |> select(-c("shorttitle", "commitment"))
  ## make a list of the elements,
  ## where each element has to be a list,
  ## and missing values should be empty lists
  d1.list <- lapply(d1, \(x) if(is.na(x)) list() else list(x))
  if("commitment" %in% names(dat1)) {
    d2 <- dat1$commitment[[1]]
    d2.list <- map2(d2$year, d2$effort, \(y, e) {
      list(personmonth = structure(list(e), year = y))
    })
    d1.list$commitment <- d2.list
  }
  list(support=d1.list)
}

dat_to_xml <- function(d) {
  row_xml <- lapply(seq_len(nrow(d)), \(idx) row_to_xml(d[idx,]))
  list(profile=list(
    identification=list(),
    employment=list(),
    funding=row_xml
  )) |> 
    as_xml_document() 
}

prepare_projects <- function(d) {
  d |> mutate(across(c(startdate, enddate), as.Date)) |>
    rename(prorate="prorate?") |>
    mutate(prorate=(prorate |> str_sub(1, 1) |> replace_na("N"))=="Y") |>
    mutate(awardamount=as.integer(awardamount)) |>
    add_commitment()
}
