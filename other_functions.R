read_projects <- function(file, sheet="projects") {
  d2a <- readxl::read_excel(file, sheet) |> rename(var="internalcode")
  kb <- which(str_detect(d2a$var, "date$"))
  d2b <- readxl::read_excel(support_xlsx, "projects", skip=kb[1], col_names=names(d2a), n_max = diff(range(kb))+1)
  kc <- which(str_detect(d2a$var, "^personmonths_[0-9]+$"))
  d2c <- readxl::read_excel(support_xlsx, "projects", skip=kc[1], col_names=names(d2a), n_max = diff(range(kc))+1)
  xa <- d2a |> column_to_rownames("var") |> t() |> as_tibble()
  xb <- d2b |> column_to_rownames("var") |> t() |> as_tibble()
  xc <- d2c |> column_to_rownames("var") |> t() |> as_tibble()
  xa[names(xb)] <- xb
  xa[names(xc)] <- xc
  xa$awardamount <- as.integer(xa$awardamount)
  xa
}
read_identification <- function(file, sheet="identification") {
  name <- readxl::read_excel(file, sheet, "A1:B3", col_names=c("name", "value")) |>
    column_to_rownames("name") |> t() |> as_tibble()
  fix1 <- function(x) {c("name", paste0("Position_", 1:(length(x)-1)))}
  positions <- readxl::read_excel(file, sheet, skip=5, col_names=FALSE, .name_repair = fix1) |>
    column_to_rownames("name") |> t() |> as_tibble()
  list(name=name, positions=positions)
}

projects_to_xml <- function(dat) {
  rowtolist <- function(d) {
    as.list1 <- function(x) {
      if(length(x)==1 & is.na(x)) {
        list()
      } else {
        list(x)
      }
    }
    com <- d |> mutate(row=1:n()) |> select(row, starts_with("personmonths_")) |>
      pivot_longer(-row, names_to=c("X", "year"), names_sep="_") |>
      filter(!is.na(value))
    foo <- lapply(seq_len(nrow(com)), \(idx) {
      list(personmonth = structure(list(com$value[idx]), year = com$year[idx]))
    })
    out <- d |> select(!starts_with("personmonths_")) |> lapply(as.list1)
    out$commitment <- foo
    out
  }
  foo <- lapply(seq_len(nrow(dat)), \(idx) {
    list(support=rowtolist(dat[idx,]))
  })
  foo
}

support_to_xml <- function(file_xlsx, file_xml) {
  #d1 <- read_identification(support_xlsx)
  d2 <- read_projects(file_xlsx)
  
  list(profile=list(
    identification=list(),
    employment=list(),
    funding=projects_to_xml(d2)
  )) |> 
    as_xml_document() |> write_xml(file_xml)
}