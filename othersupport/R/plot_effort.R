plot_effort <- function(date1, date2, budget, cal2, daterange) {
  ef <- cal2 |> summarize(b1=min(start), b2=max(end), effort=sum(effort), .by=budget)
  
  ## get start/end of full budget years
  ## needed to give the plot enough breathing room on the sides
  ## this is hacky...
  budget_range <- get_range(date1, date2, budget)
  if(missing(daterange)) {daterange <- budget_range}
  
  ef_txt <- ef |> mutate(months=monthdiff(b1, b2+1), efper=effort/months,
                         by=seq_len(n())) |>
    mutate(txt=sprintf("Budget Year %d:\n%s months effort\nover %s months\n(%s per month)", 
                       by, round2(effort), round2(months), 
                       round2(efper))) |>
    mutate(x=budget_range[-length(budget_range)] + months(6))
  
  cal2_txt <- cal2 |> mutate(txt=sapply(round(effort, 2), format)) |>
    mutate(mid=monthmid(start, end))
  cal <- cal2 |>
    summarize(effort=sum(effort), .by=year)
  cal_txt <- cal |> mutate(x=as.Date(sprintf("%d-07-01", year))) |>
    mutate(eff=sapply(round(effort, 2), format),
           txt=sprintf("Year %d\n%s month%s", year, eff, if_else(eff=="1", "", "s")))
  years <- cal$year
  
  yr <- monthdiff(min(daterange), max(daterange))/12
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
               color="black", alpha=0.8, linetype="dashed", linewidth=1) +
    geom_text(aes(x=x, y=Inf, label=txt), size=6/.pt, vjust=1, hjust=0.5) +
    coord_cartesian(clip="off") +
    expand_limits(x=daterange, y=1) +
    geom_text(aes(x=mid, y=0, label=txt), data=cal2_txt, vjust=-0.5, inherit.aes=FALSE, size=8/.pt, color=year_color) +
    geom_text(aes(x=x, y=max(c(ef_txt$efper, 1))/2, label=txt), data=cal_txt, inherit.aes=FALSE, size=10/.pt, color=year_color) +
    labs(x=NULL, y="Percent Effort") +
    fitto(2, yr)
  effort_plot
}

all_effort_plot <- function(p) {
  cal <- p |> select(shorttitle, calendar) |> unnest(calendar) |>
    summarize(effort=sum(effort), .by=year) |>
    mutate(x=as.Date(sprintf("%d-07-01", year))) |>
    mutate(eff=sapply(round(effort, 2), format),
           txt=sprintf("Year %d\n%s month%s", year, eff, if_else(eff=="1", "", "s")))
  
  b <- p |> select(shorttitle, budget=.budget) |>
    unnest(budget) |> mutate(row=seq_len(n()), .before=1)
  dx <- sort(unique(c(b$b1, b$b2)))
  ef_all <- b |> cross_join(tibble(d1=dx)) |> filter(d1>=b1 & d1<=b2) |> arrange(row) |> 
    mutate(d2=lead(d1), dif=monthdiff(d1, d2), 
           p=dif/sum(dif, na.rm=TRUE), 
           ef=effort*p,
           efper=ef/dif,
           .by=row) |>
    filter(!is.na(d2))
  
  ef_all_sum <- ef_all |> summarize(efper=sum(efper), ef=sum(ef), .by=c(d1, d2, dif)) |> arrange(d1)
  my <- max(ef_all_sum$efper)
  
  years <- sort(unique(year(dx)))
  ef_all_plot <- ef_all |> mutate(d2=d2-minutes(1)) |>
    select(row, shorttitle, ddd_1=d1, ddd_2=d2, ef, efper) |>
    pivot_longer(starts_with("ddd_"), values_to="date")
  
  yr <- length(years)
  pp <- ggplot(ef_all_plot) + geom_area(aes(x=date, y=efper, fill=shorttitle)) +
    theme_minimal() +
    coord_cartesian(clip="off") +
    theme(legend.title = element_blank()) +
    labs(x=NULL, y="Percent Effort") +
    scale_x_date(breaks=as.Date(sprintf("%d-07-01", years)), labels=years,
                 minor_breaks=as.Date(as.vector(outer(years, c(1,4,7,10), \(y,m) sprintf("%d-%02d-01", y, m)))),
                 expand=expansion(mult=0.001)) +
    scale_y_continuous(expand=expansion(mult=c(0.001,0.15)), labels=scales::percent) +
    geom_vline(xintercept=as.Date(sprintf("%d-01-01", c(years, max(years)+1))),
               color=year_color, alpha=0.8, linetype="dashed") +
    geom_text(aes(x=x, y=Inf, label=txt), data=cal, vjust=1,
              inherit.aes=FALSE, size=9/.pt, color=year_color, fontface="bold") +
    theme(axis.text.x = element_text(color=year_color)) +
    fitto(2, yr)
  
  pp
}