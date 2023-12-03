# common download function


library(rvest)
library(tidyverse)
library(lubridate)
library(here)
library(archiveRetriever)
library(pdftools)

# This script contains procedures for downloading pdfs from different OECD websites

get_urls_oecd <- function(page){
  file_list <- page %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    str_subset("\\.pdf") %>%
    str_c("https://www.oecd.org", .)
  return(file_list)
}

# This code is downloading PDF files from two webpages "https://www.oecd.org/tax/dispute/country-map-profiles.htm" and "https://www.oecd.org/tax/transfer-pricing/transfer-pricing-country-profiles.htm". The files are being downloaded in two steps:

#Searching the webpage for PDF files by using the read_html() function and then extracting the links using html_nodes() and html_attr().
# Downloading the files using the download.file() function.

# Before downloading the files, the code checks if the archive.org has different versions of the file and downloads the version with the latest creation date. This is done using the retrieve_urls() function which searches archive.org and returns a list of URLs. The latest creation date is determined by using the get_dates() function which extracts the creation date from the PDF file.

download_files <- function(url, directory, remove_from_url, start_date) {

  ## Procedure taken from <https://towardsdatascience.com/scraping-downloading-and-storing-pdfs-in-r-367a0a6d9199>
  page <- read_html(url)

  file_list <- get_urls_oecd(page)
  file_list <- file_list[!str_detect(file_list, "htvi")]

  get_date <- function(file) {
    date <- tryCatch(pdf_info(file) %>% pluck(6) %>% lubridate::as_datetime() %>% lubridate::date(),
                     error = function(err) NA)
    return(date)
  }

  download_from_archive <- function(file) {
    name <- str_remove_all(file, remove_from_url)
    name <- str_remove_all(name, regex(".pdf", ignore_case = TRUE))

    wayback_available <- try(archiveRetriever::retrieve_urls(file, startDate = start_date, endDate = stringr::str_remove_all(Sys.Date(), "-")))

    if (inherits(wayback_available, "try-error")) {
      date <- get_date(file)
      try(download.file(file, here(str_glue("{directory}/{name}_{date}.pdf")), mode = "wb"))
    } else {

      wayback_available <- append(wayback_available, file)
      creation_dates <- as_date(map_dbl(wayback_available, get_date))
      distinct_dates <- unique(creation_dates)

      retrieve_position <- function(x){
        pos <- which(creation_dates %in% x)
        return(pos[1])
      }
      positions <- map_int(distinct_dates, retrieve_position)

      urls <- wayback_available[positions]

      for (i in 1:length(urls)) {
        try(download.file(urls[i], here(str_glue("{directory}/{name}_{distinct_dates[i]}.pdf")), mode = "wb"))
      }
    }
  }

  walk(file_list, download_from_archive)
}

# MAP profiles

download_files("https://www.oecd.org/tax/dispute/country-map-profiles.htm", here("data-raw/map_profiles"), "https://www.oecd.org/tax/dispute/|-dispute-resolution-profile", "20150101")

# TP Profiles

download_files("https://www.oecd.org/tax/transfer-pricing/transfer-pricing-country-profiles.htm", here("data-raw/tp_profiles"), "https://www.oecd.org/tax/transfer-pricing/transfer-pricing-country-profile-|https://www.oecd.org/ctp/transfer-pricing/transfer-pricing-country-profile-", "20100101")


# MAP Peer Review and best practices

page <- read_html("https://www.oecd.org/tax/beps/beps-actions/action14/")
file_list <- page %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  str_subset("\\.htm")

file_list <- file_list[which(str_detect(file_list, "peer-review-report"))]

file_list[which(!str_detect(file_list, "http"))] <- paste0("https://www.oecd.org", file_list[which(!str_detect(file_list, "http"))])

names <- str_remove_all(file_list, "https://|http://|www.oecd.org/tax/|beps/|making-dispute-resolution-more-effective-map-peer-review-report-")
names <- str_sub(names, 1, -17)
names <- str_remove_all(names, "-9789")

for (i in 1:length(file_list)) {
  page1 <- read_html(file_list[i])
  peer_review <- page1 %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    str_subset("pdf")
  download.file(peer_review[1], here(str_glue("data-raw/map_peer_review/{names[i]}.pdf")), mode = "wb")

best_practice <- get_urls_oecd(page1)

  best_practice <- best_practice[which(str_detect(best_practice, "best"))]
  best_practice <- unique(best_practice)
  if (!is.na(best_practice[1])) {
    try(download.file(best_practice[1], here(str_glue("data-raw/map_best_practices/{names[i]}-bp.pdf")), mode = "wb"))
    }
  }




