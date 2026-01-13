process_effort <- function(date1, date2, effort, budget) {
  date1 <- as.Date(date1)
  date2 <- as.Date(date2)
  budget <- as.Date(budget)
  
  ## I assume date2 is the first day after it ends, however...
  ## if the next day after the end is the same day of the month as it starts,
  ## assume last date is actually the day before what's given
  ## and add a day.
  if(day(date1) == day(date2+1)) {
    date2 <- date2 + 1
  }
  
  ## get calendar years (eg. 2025:2027)
  years <- year(date1):year(date2-1)
  
  ## get budget year start/end dates (this is a bit of a hack...)
  budgets <- as.Date(sprintf("%d-%02d-%02d", years, month(budget), day(budget)))
  budgets <- c(date1, budgets[budgets > date1 & budgets < date2], date2)

  ## need to have effort equal to the number of budget years  
  stopifnot(length(effort) == length(budgets) - 1)

  ## table of budget periods and effort
  ef <- tibble(b1=budgets[-length(budgets)], b2=budgets[-1], effort=effort)

  ## for simplicity, assume 30 days in every month...
  monthdiff <- function(x1, x2) {
    (year(x2) - year(x1))*12 + month(x2) - month(x1) + 
      (pmin(day(x2),30) - pmin(day(x1),30))/30
  }
  monthmid <- function(x1, x2) {
    dif <- monthdiff(x1, x2)/2
    x1 + months(dif %/% 1) + days(round((dif %% 1)*30))
  }
        
  ## compute how much of each budget period is in each calendar year
  ## there's got to be an easier way...
  cal1 <- ef |> mutate(cal=as.Date(sprintf("%d-01-01", year(b2)))) |>
    select(b1, cal, b2, everything()) |>
    mutate(cal=cal |> pmax(b1),
           r1=monthdiff(b1, cal), 
           r2=monthdiff(cal, b2),
           effort_1=effort*r1/(r1+r2), 
           effort_2=effort*r2/(r1+r2),
           mid_1=monthmid(b1, cal),
           mid_2=monthmid(cal, b2)) |>
    mutate(budget=1:n(), year_1=year(b1), year_2=year(b2))
  cal2 <- cal1 |>
    select(budget, contains("_")) |>
    pivot_longer(contains("_"), names_to=c(".value", "X"), names_sep="_") |>
    filter(effort > 0) |> select(-X)
  cal <- cal2 |>
    summarize(effort=sum(effort), .by=year)
  
  ## get start/end of full budget years
  ## needed to give the plot enough breathing room on the sides
  ## this is hacky...
  budget_range <- local({
    bx <- as.Date(sprintf("%d-%02d-%02d", (year(date1)-1):(year(date2)+1), month(budget), day(budget)))
    bb1 <- max(bx[bx <= date1])
    bb2 <- min(bx[bx >= date2])
    bx[bx >= bb1 & bx <= bb2]
  })  
  
  ef_txt <- ef |> mutate(months=monthdiff(b1, b2), efper=effort/months,
                         by=seq_len(n())) |>
    mutate(txt=sprintf("Budget Year %d:\n%d months effort\nover %d months\n(%s per month)", 
                       by, effort, months, sapply(round(efper, 2), format))) |>
    mutate(x=budget_range[-length(budget_range)] + months(6))
  
  cal2_txt <- cal2 |> mutate(txt=sapply(round(effort, 2), format))
  cal_txt <- cal |> mutate(x=as.Date(sprintf("%d-07-01", year))) |>
    mutate(eff=sapply(round(effort, 2), format),
           txt=sprintf("Year %d\n%s month%s", year, eff, if_else(eff=="1", "", "s")))
  
  year_color <- "blue"
  effort_plot <- ggplot(ef_txt) + aes(xmin=b1, xmax=b2, ymin=0, ymax=efper) + 
    geom_rect(fill="gray90", color="gray50") +
    theme_minimal() +
    theme(axis.text.x = element_text(color=year_color)) +
    scale_x_date(breaks=as.Date(sprintf("%d-07-01", years)), labels=years,
                 minor_breaks=as.Date(as.vector(outer(years, c(1,4,7,10), \(y,m) sprintf("%d-%02d-01", y, m)))),
                 expand=expansion(mult=0.001)) +
    scale_y_continuous(expand=expansion(mult=c(0.001,0.1)), labels=scales::percent) +
    geom_vline(xintercept=as.Date(sprintf("%d-01-01", c(years, max(years)+1))),
               color=year_color, alpha=0.8, linetype="dashed") +
    geom_vline(xintercept=budget_range,
               color="black", alpha=0.8, linetype="dotted") +
    geom_text(aes(x=x, y=Inf, label=txt), size=6/.pt, vjust=1, hjust=0.5) +
    coord_cartesian(clip="off") +
    expand_limits(x=budget_range) +
    geom_text(aes(x=mid, y=0, label=txt), data=cal2_txt, vjust=-0.5, inherit.aes=FALSE, size=8/.pt, color=year_color) +
    geom_text(aes(x=x, y=max(ef_txt$efper)/2, label=txt), data=cal_txt, inherit.aes=FALSE, size=10/.pt, color=year_color) +
    labs(x=NULL, y="Percent Effort")
  list(calendar=cal, budget=ef, plot=effort_plot)
}

required_vars <- c("shorttitle", "projecttitle", "awardnumber", "supportsource", 
                   "location", "contributiontype", "awardamount", "inkinddescription", 
                   "overallobjectives", "potentialoverlap", "startdate", "enddate", 
                   "supporttype")

prepare_projects <- function(dat) {
  dat <- dat |> mutate(awardamount=as.integer(awardamount))
  row2list <- function(dat1) {
    d1 <- dat1 |> select(!starts_with("year "))
    d2 <- dat1 |> select(starts_with("year "))
    budget_start <- dat1[["budget year start date"]]
    if(is.na(budget_start)) {
      budget_start <- dat1$startdate
    }
    ## grab the effort columns
    effort <- d2 |> pivot_longer(starts_with("year "), names_to=c("X", "year"), 
                                 names_sep=" ", values_to="effort") |>
      select(year, effort) |> arrange(year) |> filter(!is.na(effort)) |> pull(effort)
    ## if there is any effort, process it and add it to the data set
    if(length(effort) > 0) {
      ef <- process_effort(dat1$startdate, dat1$enddate, effort, budget_start)
      d1$commitment <- list(ef$calendar)
      d1$.budget <- list(ef$budget)
      d1$.plot <- list(ef$plot)
    }
    d1
  }
  bind_rows(lapply(seq_len(nrow(dat)), \(idx) row2list(dat[idx,])))
}

row_to_xml <- function(dat1) {
  d1 <- dat1 |> select(all_of(required_vars))
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

all_effort_plot <- function(p) {
  b <- p |> select(shorttitle, budget=.budget) |>
    unnest(budget) |> mutate(row=seq_len(n()), .before=1)
  dx <- sort(unique(c(b$b1, b$b2)))
  ef_all <- b |> cross_join(tibble(d1=dx)) |> filter(d1>=b1 & d1<=b2) |> arrange(row) |> 
    mutate(d2=lead(d1), dif=monthdiff(d1, d2), 
           p=dif/sum(dif, na.rm=TRUE), 
           ef=effort*p,
           .by=row) |>
    filter(!is.na(d2))
  
  years <- sort(unique(year(dx)))
  ef_all_plot <- ef_all |> mutate(d2=d2-minutes(1)) |>
    select(row, shorttitle, ddd_1=d1, ddd_2=d2, ef) |>
    pivot_longer(starts_with("ddd_"), values_to="date")
  pp <- ggplot(ef_all_plot) + geom_area(aes(x=date, y=ef, fill=shorttitle)) +
    theme_minimal() +
    coord_cartesian(clip="off") +
    theme(legend.title = element_blank()) +
    scale_y_continuous(expand=expansion(c(0,0.05)), labels=scales::percent) +
    labs(x=NULL, y="Percent Effort") +
    scale_x_date(breaks=as.Date(sprintf("%d-07-01", years)), labels=years,
                 minor_breaks=as.Date(as.vector(outer(years, c(1,4,7,10), \(y,m) sprintf("%d-%02d-01", y, m)))),
                 expand=expansion(mult=0.001)) +
    scale_y_continuous(expand=expansion(mult=c(0.001,0.1)), labels=scales::percent) +
    geom_vline(xintercept=as.Date(sprintf("%d-01-01", c(years, max(years)+1))),
               color=year_color, alpha=0.8, linetype="dashed")
  
  pp
}