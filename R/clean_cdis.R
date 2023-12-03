#' clean cdis
#'
#' function for basic cleaning of cdis data frame: removing of values for aggregate geographical zones, adding countrycodes, pivot longer, and express values in Millions
#'
#' @param cdis is the cdis dataset
#' @import dplyr
#' @import countrycode
#' @return the cleaned cdis dataframe
#'

clean_cdis <- function(cdis) {
  cdis %<>% rename("2021" = "...20")
  # filter out aggregates
  aggregates <- c(
    "Sub-Saharan Africa",
    "South America",
    "Oceania and Polar Regions",
    "East Asia",
    "North Atlantic and Caribbean",
    "Europe",
    "Central and South Asia",
    "North Africa",
    "World",
    "North and Central America",
    "Economies of Persian Gulf"
  )

  cdis %<>% filter(!`Country Name` %in% aggregates)
  cdis %<>% filter(!`Counterpart Country Name` %in% aggregates)

  custom_codes <- c(
    "Aruba, Kingdom of the Netherlands" = "ABW",
    "Curaçao, Kingdom of the Netherlands" = "CUW",
    "Guiana, French" = "GUF",
    "Sint Maarten, Kingdom of the Netherlands" = "SXM",
    "US Pacific Islands" = "PUS"
  )
  cdis %<>% mutate(country_iso3c = countrycode(coalesce(countryname(`Country Name`), `Country Name`), "country.name", "iso3c", custom_match = custom_codes))
  cdis %<>% mutate(counterpart_iso3c = countrycode(coalesce(countryname(`Counterpart Country Name`), `Counterpart Country Name`), "country.name", "iso3c", custom_match = custom_codes))
  cdis %<>% pivot_longer(cols = c(8:20), names_to = "year", values_to = "value")
  cdis$value %<>% as.numeric
  # bringing value to millions
  cdis %<>% mutate(value = value / 1000000)
  return(cdis)
}
