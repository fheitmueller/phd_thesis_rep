#' calculate weights
#'
#' @param data is the standard dataframe
#' @import dplyr
#' @return the dataframe containing the different weights
#'
#'

calculate_weights <- function(data) {

  # subtracting distances from maximum distance
  data %<>% mutate(dist_diff_max = 20000 - dist)

  # year-to-year weights
  data %<>%
    group_by(host, year, category) %>%
    mutate(weight = (NY.GDP.MKTP.CD / sum(NY.GDP.MKTP.CD, na.rm = TRUE)) * 0.5 +
      (NY.GDP.PCAP.CD / sum(NY.GDP.PCAP.CD, na.rm = TRUE)) * 0.3 +
      (dist_diff_max / sum(dist_diff_max, na.rm = TRUE)) * 0.2)

  # constant weights taking average GDP and average GDP per capita over the whole period
  data %<>%
    group_by(home, category) %>%
    mutate(average_GDP = mean(NY.GDP.MKTP.CD, na.rm = TRUE), average_GDP_cap = mean(NY.GDP.PCAP.CD, na.rm = TRUE))

  data %<>%
    group_by(host, year, category) %>%
    mutate(weight_const = (average_GDP / sum(average_GDP, na.rm = TRUE)) * 0.5 +
      (average_GDP_cap / sum(average_GDP_cap, na.rm = TRUE)) * 0.3 +
      (dist_diff_max / sum(dist_diff_max, na.rm = TRUE)) * 0.2)
}


#' calculate means
#'
#' @param data is the standard dataframe
#' @param weight indicates whether time-constant or time-dynamic weights should be used. Depends on what kind of analysis is carried out. Defaults to the time dynamic weight.
#' @import dplyr
#' @return the dataframe containing the different weights
#'
#'

all_countries_means <- function(data, weight = "weight") {
  data %<>%
    group_by(host, year, category) %>%
    summarise(
      rate_weighted_mean = weighted.mean(.data[["appl_max"]], .data[[weight]], na.rm = TRUE),
      rate_shopping_mean = weighted.mean(.data[["rate_shopping"]], .data[[weight]], na.rm = TRUE),
      rate_shopping_mean_no_aa = weighted.mean(.data[["rate_shopping_no_aa"]], .data[[weight]], na.rm = TRUE),
      min_available_mean = weighted.mean(.data[["min_available"]], .data[[weight]], na.rm = TRUE),
      min_available_mean_no_aa = weighted.mean(.data[["min_available_no_aa"]], .data[[weight]], na.rm = TRUE)
      )
  return(data)
}
