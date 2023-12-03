# extract table of contents coporate tax guides
if (!require('here')) install.packages('here'); library('here')
if (!require('readr')) install.packages('readr'); library('readr')
if (!require('tidyr')) install.packages('tidyr'); library('tidyr')
if (!require('dplyr')) install.packages('dplyr'); library('dplyr')
if (!require('magrittr')) install.packages('magrittr'); library('magrittr')
if (!require('countrycode')) install.packages('countrycode'); library('countrycode')
if (!require('purrr')) install.packages('purrr'); library('purrr')
if (!require('pdftools')) install.packages('pdftools'); library('pdftools')
if (!require('qpdf')) install.packages('qpdf'); library('qpdf')
if (!require('stringr')) install.packages('stringr'); library('stringr')
if (!require('forcats')) install.packages('forcats'); library('forcats')

devtools::load_all()

# defining for which data years the script should run
years <- c(2004:2021)


filenames <- list.files(path = here("data-raw/corporate_tax_guides"))
filenames <- map_chr(filenames, function(x){paste0(here("data-raw/corporate_tax_guides"), "/", x)})


# Countries which are listed in the table of contents, but which do not have an "At A Glance" table for the year in question. This is only the case in earlier reports.

countries_not_in <- list("2004" = c("COMMONWEALTH OF INDEPENDENT STATES",
                                    "RÉUNION"),
                         "2005" = c("COMMONWEALTH OF INDEPENDENT STATES",
                                    "RÉUNION",
                                    "TURKMENISTAN"),
                         "2006" = c("COMMONWEALTH OF INDEPENDENT STATES",
                                    "LIECHTENSTEIN",
                                    "TURKMENISTAN"),
                         "2007" = c("COMMONWEALTH OF INDEPENDENT STATES",
                                    "LAOS",
                                    "TURKMENISTAN",
                                    "IRAQ"),
                         "2008" = c("COMMONWEALTH OF INDEPENDENT STATES",
                                    "LAOS"),
                         "2009" = NA,
                         "2010" = NA,
                         "2011" = NA,
                         "2012" = NA,
                         "2013" = NA,
                         "2014" = NA,
                         "2015" = NA,
                         "2016" = NA,
                         "2017" = NA,
                         "2018" = NA,
                         "2019" = NA,
                         "2020" = NA,
                         "2021" = NA)

instructions <- list(filenames, years, countries_not_in)


extract_tocs <- function(num, instructions, custom_matches){

  if (num != 9) {
    toc <- pdf_toc(instructions[[1]][[num]])
    toc %<>% flatten
    toc %<>% flatten
    toc %<>% compact
    if (num == 18) {toc %<>% keep(is.character)}

    toc %<>% flatten_chr
    toc <- data.frame(country = toc)
  } else {
    toc <- pdf_text(instructions[[1]][[num]])
    toc <- toc[4:7] # the pages with the table of content
    toc <- as.data.frame(read_lines(toc))
    names(toc) <- "country"
    # delete the Hong Kong line jump
    toc %<>% slice(-65)
    toc %<>% mutate(country2 = str_sub(country, start = 1, end = str_locate(country, "\\.\\.")[ ,1]))
    toc %<>% mutate(country2 = str_sub(country2, start = 1, end = -2))
    toc %<>% mutate(country = coalesce(country2, country))
    toc %<>% mutate(country = trimws(country))
    toc %<>% select(-country2)
  }
  # the function specific to the 2012 version where the table of content cannot be extracted automatically

  # filter out countries that do not have the At a Glance table
  toc %<>% filter(!country %in% instructions[[3]][[num]])

  # Serbia and Montenegro have a separate table but only one entry in the table of content for the years in which they were still one country

  if ("SERBIA AND MONTENEGRO, UNION OF" %in% toc$country) {
    toc$country %<>% recode("SERBIA AND MONTENEGRO, UNION OF" = "SERBIA") # separate data for Serbia and Montenegro
    toc %<>% add_row(country = "MONTENEGRO", .after = which(toc$country == "SERBIA"))}

  # add countrycodes
  ##Custom matches are added wherever the procedure does not give a good result. For example, countryname does not distinguish well between Saint-Martin (French Part) and Sint Maarten (Dutch part)

  toc %<>% mutate(try = countrycode(country,
                                    "country.name",
                                    "country.name"))


  toc %<>% mutate(iso3c = countrycode(coalesce(try, country),
                                      "country.name",
                                      "iso3c",
                                      custom_match = custom_matches))
  toc %<>% select(-try)
  # filter out entries in the table of content, which are not countries

  # extract the section after the last country
  ctry_sections <- which(!is.na(toc$iso3c))
  other_sec <- toc$country[ctry_sections[length(ctry_sections)] + 1]
  toc %<>% filter(!is.na(iso3c))
  return(list(toc, other_sec))
}


custom_matches <- c("Netherlands Antilles" = "ANT",
                    "GUERNSEY, CHANNEL ISLANDS" = "GGY",
                    "JERSEY, CHANNEL ISLANDS" = "JEY",
                    "Kosovo" = "KSV",
                    "Saint-Martin" = "MAF",
                    "Bonaire, Sint Eustatius and Saba(BES-Islands; extraordinary overseasmunicipalities of the Netherlands)" = "BES",
                    "Bonaire, Sint Eustatius and Saba (BES-Islands; extraordinary overseas municipalities of the Netherlands)" = "BES")


all_tocs <- map(seq_along(1:length(years)), extract_tocs, instructions, custom_matches)
tocs <- lapply(all_tocs, "[[", 1)
tocs <- map2(tocs, years, function(x, y){add_column(x, year = y)})
tocs <- reduce(tocs, rbind)

saveRDS(tocs, here("data-created/tocs.rds"))

other_sec <- lapply(all_tocs, "[[", 2)
other_sec %<>% flatten_chr
saveRDS(other_sec, here("data-created/other_sec.rds"))
