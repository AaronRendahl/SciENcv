row_to_xml <- function(dat1, current_year) {
  d1 <- dat1 |> select(all_of(required_vars))
  ## make a list of the elements,
  ## where each element has to be a list,
  ## and missing values should be empty lists
  d1.list <- lapply(d1, \(x) if(is.na(x)) list() else list(x))
  if("commitment" %in% names(dat1)) {
    d2 <- dat1$commitment[[1]] |> mutate(effort=round(effort, 2)) |>
      filter(year >= current_year, effort > 0)
    d2.list <- map2(d2$year, d2$effort, \(y, e) {
      list(personmonth = structure(list(e), year = y))
    })
    d1.list$commitment <- d2.list
  }
  list(support=d1.list)
}

dat_to_xml <- function(d, current_year=0) {
  row_xml <- lapply(seq_len(nrow(d)), \(idx) row_to_xml(d[idx,], current_year))
  list(profile=list(
    identification=list(),
    employment=list(),
    funding=row_xml
  )) |> 
    as_xml_document() 
}