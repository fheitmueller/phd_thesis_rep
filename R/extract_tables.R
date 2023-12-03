#' extract tables with python camelot
#'
#' @param file is the path to the pdf file
#' @param pages is the pages from which the tables should be extracted
#' @param flavor specifies the extraction method, can be either stream or lattice
#' @import reticulate
#' @import purrr
#' @import tibble
#' @export
#' @return a list of the extracted tables

extract_tables <- function(file, pages, flavor, check_installation = TRUE) {
  if (check_installation == TRUE) {
  python_pdf_set_up()

  }
  cm <- reticulate::import("camelot")
  pages <- paste0(pages, collapse = ",")
  tables <- cm$read_pdf(file, pages = pages, flavor = flavor)
  tablesnumbers <- c(0:(tables$n - 1))
  take_out_table <- function(x){
    df <- as.data.frame(tables[x]$df)
    attributes(df)$page <- tables[x]$page
    return(df)
  }
  tableslist <- lapply(tablesnumbers, take_out_table)
  return(tableslist)
}


#' extract tables with python camelot (a simplified version without precising page numbers)
#'
#' @param file is the path to the file to be extracted
#' @import reticulate
#' @import purrr
#' @import tibble
#' @export
#' @return a list of the extracted tables

extract_tables_simple <- function(file) {
  python_pdf_set_up()
  cm <- reticulate::import("camelot")
  tables <- cm$read_pdf(file, flavor = "lattice")
  tablesnumbers <- c(0:(tables$n - 1))
  tableslist <- lapply(tablesnumbers, function(x) as.data.frame(tables[x]$df))
  return(tableslist)
}



#' extract tables with python camelot using a table region
#'
#' @param file is the path to the pdf file
#' @param pages is the pages from which the tables should be extracted
#' @param flavor specifies the extraction method, can be either stream or lattice
#' @param region is the region of the page in which the table can be found
#' @import reticulate
#' @import purrr
#' @import tibble
#' @export
#' @return a list of the extracted tables

extract_tables_region <- function(file, pages, flavor, region) {
  python_pdf_set_up()
  cm <- reticulate::import("camelot")
  pages <- paste0(pages, collapse = ",")
  tables <- cm$read_pdf(file, pages = pages, flavor = flavor, table_areas = as.list(region))
  tablesnumbers <- c(0:(tables$n - 1))

  take_out_table <- function(x){
    df <- as.data.frame(tables[x]$df)
    attributes(df)$page <- tables[x]$page
    return(df)
  }
  tableslist <- lapply(tablesnumbers, take_out_table)

  return(tableslist)
}


#' extract tables with python camelot specifying where column should be split
#'
#' @param file is the path to the pdf file
#' @param pages is the pages from which the tables should be extracted
#' @param flavor specifies the extraction method, can be either stream or lattice
#' @param region is the region of the page in which the table can be found
#' @import reticulate
#' @import purrr
#' @import tibble
#' @export
#' @return a list of the extracted tables

extract_tables_split <- function(file, pages, columns) {
  python_pdf_set_up()
  cm <- reticulate::import("camelot")
  pages <- paste0(pages, collapse = ",")
  columns <-  paste0(columns, collapse = ",")
  tables <- cm$read_pdf(file, pages = pages, flavor = "stream", columns = columns)
  tablesnumbers <- c(0:(tables$n - 1))

  take_out_table <- function(x){
    df <- as.data.frame(tables[x]$df)
    attributes(df)$page <- tables[x]$page
    return(df)
  }
  tableslist <- lapply(tablesnumbers, take_out_table)

  return(tableslist)
}
