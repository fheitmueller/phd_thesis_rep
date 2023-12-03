#' treaties in year
#'
#' showing which treaties are in force in a giving year
#'
#' @param year is the year for which the network should be reconstructed
#' @param data is the tax treaty data, needs columns "ended" and "EFFECTIVE"
#' @import dplyr
#' @return returns the dataset including new column with the datayear
#' @export

treaties_in_year <- function(year, data) {
  data <- data %>%
    filter(STATUS != "Not In Force" & !is.na(EFFECTIVE)) %>%
    filter(!(STATUS == "Terminated" & is.na(ended))) %>%
    filter(EFFECTIVE <= year & (ended >= year | is.na(ended))) %>%
    mutate(datayear = year)
  return(data)
}
