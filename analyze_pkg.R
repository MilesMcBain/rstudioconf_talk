temp_dir <-  ".tmp" 
dir.create(temp_dir)

library(curl)
library(glue)
library(readr)
library(stringr)

repo <- "datapasta"
curl_download(url = glue("https://github.com/cran/{repo}/archive/master.zip"),
              destfile = file.path(temp_dir, glue("{repo}.zip")))

unzip(file.path(temp_dir, glue("{repo}.zip")),
      exdir = file.path(temp_dir))

file.rename(file.path(temp_dir, glue("{repo}-master")),
            file.path(temp_dir, glue("{repo}")))


extract_namespace <- function(x, thing) {
  if (is.null(x)) {
    return(NA_character_)
  }
  str <- stringr::str_extract_all(x, glue::glue("{thing}\\((?:([^\\)]*)(?:[^\\)]|$))\\)"))
  if (length(str[[1]]) == 0) {
    return(NA_character_)
  }
  str <- stringr::str_remove_all(unlist(str),
                                 glue::glue("\\n|\\\"| |{thing}\\(|\\)"))
  if (thing == "S3method") {
    return(gsub(",", ".", str))
  }
  unlist(stringr::str_split(str, ","))
}

get_namespace_tbl <- function(namespace_path, verbose = TRUE) {
  ns_data <- read_file(namespace_path)

  list(
    export_methods = extract_namespace(ns_data, "exportMethods"),
    export_classes = extract_namespace(ns_data, "exportClasses"),
    export = extract_namespace(ns_data, "export"),
    s3_methods = extract_namespace(ns_data, "S3method")
  ) %>%
    enframe("namespace_directive", "func") %>%
    unnest()
}

pkg_namespace <- get_namespace_tbl(file.path(temp_dir,repo,"NAMESPACE"))

## use snake case?
## tabs or spaces?
## number of functions?
## median function name length?
## n code lines?
## n documentation lines?
## n non R src lines?
## use rcpp?
## use shiny?
## use rstudioapi?
## use htmlwidgets?
## provides addins.dcf?
## n vignettes?
## has readme?
## on github?
## image in readme?
## gif in readme?
## description?


## papers:
## https://arxiv.org/abs/1411.4911
## http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/115-famd-factor-analysis-of-mixed-data-in-r-essentials/
