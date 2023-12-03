# MAP Best practices

library(stringr)
library(tidyverse)
library(here)
library(pdftools)
library(reticulate)
library(qpdf)
library(countrycode)
library(lubridate)

devtools::load_all()

files <- list.files(here("data-raw/map_best_practices"))
paths <- map_chr(files, function(x, y){paste0(y, "/", x)}, here("data-raw/map_best_practices/"))

names <- str_remove_all(files, ".pdf")
iso3c <- countrycode(countryname(files), "country.name", "iso3c")

dates <- map(paths, pdf_info)
dates <- map(dates, pluck, 6)
dates %<>% flatten_dbl()
dates %<>% as_datetime()
dates %<>% year()

bps <- data.frame(iso3c, dates)
write_csv(bps, here("data-created/map_best_practices_reviewed.csv"))
