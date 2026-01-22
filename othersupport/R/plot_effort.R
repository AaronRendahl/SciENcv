plot_effort <- function(effort, daterange) {
  date1 <- min(effort$startdate)
  date2 <- max(effort$enddate)
  date0 <- effort$startdate[nrow(effort)]

  budget <- effort |> summarize(b1=min(startdate), b2=max(enddate), effort=sum(effort), .by=year)
  calendar <- effort |> summarize(effort=sum(effort), .by=calendar) |> rename(year=calendar)
  
  budget_txt <- budget |> mutate(months=monthdiff(b1, b2+1), efper=effort/months) |>
    mutate(txt=sprintf("Budget Year %d:\n%s months effort\nover %s months\n(%s per month)", 
                       year, round2(effort), round2(months), 
                       round2(efper))) |>
      mutate(x1=pmin(b1, lead(b1, default=as.Date("2999-01-01")) - years(1)), 
             x=x1 + months(6))
  calendar_txt <- calendar |> 
    mutate(x=as.Date(sprintf("%d-07-01", year))) |>
    mutate(eff=sapply(round(effort, 2), format),
           txt=sprintf("Year %d\n%s month%s", year, eff, if_else(eff=="1", "", "s")))
  effort_txt <- effort |> mutate(txt=sapply(round(effort, 2), format)) |>
    mutate(mid=monthmid(startdate, enddate))
  
  years <- calendar$year

  plot_range <- range(
    as.Date(sprintf("%d-01-01", years[1])),
    budget_txt$x1[1],
    budget_txt$b1[nrow(ef_txt)] + years(1),
    as.Date(sprintf("%d-01-01", years[nrow(calendar)]+1)))
  if(missing(daterange)) {daterange <- plot_range}
  
  budget_range <- c(budget_txt$x1, budget_txt$x1[nrow(budget_txt)] + years(1))
    
  yr <- monthdiff(min(daterange), max(daterange))/12
  effort_plot <- ggplot(budget_txt) + aes(xmin=b1, xmax=b2, ymin=0, ymax=efper) + 
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
    geom_text(aes(x=mid, y=0, label=txt), data=effort_txt, vjust=-0.5, inherit.aes=FALSE, size=8/.pt, color=year_color) +
    geom_text(aes(x=x, y=max(c(ef_txt$efper, 1))/2, label=txt), data=calendar_txt, inherit.aes=FALSE, size=10/.pt, color=year_color) +
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
  
  p |> select(shorttitle, effort) |> unnest(effort)
  
  b <- p |> select(shorttitle, budget) |>
    unnest(budget) |> mutate(row=seq_len(n()), .before=1)
  dx <- sort(unique(c(b$startdate, b$enddate)))
  ef_all <- b |> cross_join(tibble(d1=dx)) |> filter(d1>=startdate & d1<=enddate) |> arrange(row) |> 
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