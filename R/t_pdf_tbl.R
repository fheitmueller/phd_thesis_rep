#' transform table output
#'
#' When, for example, exporting a table from a pdf, then content belonging to one line is often scattered across several lines.
#' This function copies lines that belong together in one and deletes the rest.
#'
#' @param df is the dataframe to be transformed
#' @param pl is a vector containing the numbers of the "principle lines". Can for example be obtained by a formula like the following: which(is.na(df[,1]), arr.ind = TRUE)
#' @param column is a vector containing the names of the columns to be copied. Defaults to all columns.
#' @param separator is the separator between the copied columns. Defaults to 1 whitespace.
#' @return returns the transformed dataframe
#' @export


t_pdf_tbl <- function(df, pl, column = NULL, separator = " ") {
  if (is.null(column)) {
    column <- colnames(df)
  }
  end_lines <- pl[-1]
  end_lines <- end_lines - 1
  end_lines <- append(end_lines, nrow(df))
  column_to_paste <- map(column, function(x_1, y_1){dplyr::pull(y_1, x_1)}, df)
  df <- df[pl, ]
  for (i in 1:length(column_to_paste)) {
    paste_together <- function(x_1, y_1, column_to_paste){
      together <- paste(column_to_paste[x_1:y_1], collapse = separator)
      return(together)
    }
    together <- purrr::map2_chr(pl, end_lines, paste_together, column_to_paste[[i]])
    df[, column[i]] <- together
  }
  return(df)
}
