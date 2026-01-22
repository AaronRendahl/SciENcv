year_color <- "blue"

round2 <- function(x, digits=2) {
  sapply(round(x, digits), format)
}

## for simplicity, assume 30 days in every month...
monthdiff <- function(x1, x2) {
  (year(x2) - year(x1))*12 + month(x2) - month(x1) + 
    (pmin(day(x2),30) - pmin(day(x1),30))/30
}

monthmid <- function(x1, x2) {
  dif <- monthdiff(x1, x2)/2
  x1 + months(dif %/% 1) + days(round((dif %% 1)*30))
}

get_range <- function(date1, date2, budget) {
  date1 <- as.Date(date1)
  date2 <- as.Date(date2)
  budget <- as.Date(budget)
  if(day(date1) == day(date2+1)) { date2 <- date2 + 1 }
  bx <- as.Date(sprintf("%d-%02d-%02d", (year(date1)-1):(year(date2)+1), month(budget), day(budget)))
  bb1 <- max(bx[bx <= date1])
  bb2 <- min(bx[bx >= date2])
  bx[bx >= bb1 & bx <= bb2]
}

get_data_range <- function(p) {
  px <- p |> select(shorttitle, effort) |> unnest(effort) |>
    mutate(startdate=pmin(startdate, lead(startdate, default=as.Date("2999-01-01")) - years(1)), .by=shorttitle) 
  range(min(px$startdate), max(px$startdate + years(1)),
        as.Date(sprintf("%d-01-01", c(min(px$calendar), max(px$calendar) + 1))))
}

getsize <- function(ggobj) {
  g <- ggplotGrob(ggobj)
  known_ht <- sum(grid::convertHeight(g$heights, "in", valueOnly = TRUE))
  known_wd <- sum(grid::convertWidth(g$widths, "in", valueOnly = TRUE))
  c(width=known_wd, height=known_ht)
}

fitto <- function(h, w) ggh4x::force_panelsizes(rows=unit(h, "in"), cols = unit(w, "in"))