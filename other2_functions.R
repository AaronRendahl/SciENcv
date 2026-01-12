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
      if(nrow(effort)!=length(years)) {
        stop("Need as many effort as years.")
      }
      effort1 <- tibble(year=years, effort=effort)
    } else {
      if(nrow(effort)!=length(years)-1) {
        stop("Need as many effort as years.")
      }
      p <- (m-1)/12
      effort1 <- tibble(year=years, effort1=c(effort,0), effort2=c(0, effort)) |>
        mutate(effort=effort1*(1-p) + effort2*p)
    }
  }
  effort1 |> select(years, effort)
}
