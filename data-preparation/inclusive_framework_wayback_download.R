library(here)
library(tidyverse)
library(archiveRetriever)
library(pdftools)
library(countrycode)
library(magrittr)

file <- "http://www.oecd.org/tax/beps/inclusive-framework-on-beps-composition.pdf"

urls <- retrieve_urls(file, startDate = "20150101", endDate = stringr::str_remove_all(Sys.Date(), "-"))

retrieve_composition <- function(path){
print(path)
lines <- try(pdf_text(path))
if (inherits(lines, "try-error")) {
  return(NA)
} else {
lines <- lines[1]
lines <- str_split(lines, pattern = "  |\n")[[1]]
lines <- lines[which(lines != "")]
lines[which(str_detect(lines, "Türkiye"))] <- "Turkey"
inc_frame <- countrycode(countryname(lines), "country.name", "iso3c")
inc_frame <- inc_frame[which(!is.na(inc_frame))]
date <- pdf_info(path) %>% pluck(6) %>% lubridate::as_datetime() %>% lubridate::date()
inc_frame <- data.frame(iso3c = inc_frame, date = date)
return(inc_frame)
}
}

compositions <- map(urls, retrieve_composition)
compositions %<>% reduce(rbind)
compositions %<>% group_by(iso3c) %>% summarize(joined = min(date), still_in = max(date))
compositions %<>% filter(!is.na(iso3c))
write_csv(compositions, here("data-created/inc_frame_all.csv"))


