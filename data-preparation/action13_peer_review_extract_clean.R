# CbCR peer review report

library(here)
library(pdftools)
library(qpdf)
library(tidyverse)
library(countrycode)
library(magrittr)
devtools::load_all()


extract_cbcrs <- function(path, toc_page){

toc <- extract_tables(path, toc_page, "stream")
toc %<>% reduce(rbind)
toc %<>% mutate(iso3c = countrycode(countryname(`0`), "country.name", "iso3c"))
toc %<>% filter(!is.na(iso3c)) %>% pull(iso3c)

#extracting the part of the summary on appropriate use
cbcrtext <- pdf_text(path)

pagenumbers <- which(grepl("Summary of recommendations", cbcrtext))

#extracting the part of the summary on appropriate use

read_table <- function(x, cbcrtext, toc){
  print(paste(str_sub(path, -8, -5), x, collapse = " "))
  page <- read_lines(cbcrtext[pagenumbers[x]]) #setting the text of the relevant page
  if (length(str_which(page, "References|Notes|The domestic legal and administrative framework")) > 0) {
  page_part <- page[(str_which(page, "Summary of recommendations") + 2):(str_which(page, "References|Notes|The domestic legal and administrative framework") - 1)]
  } else {
    page_part <- page[(str_which(page, "Summary of recommendations") + 2):length(page)]
  }
  page_part <- as.data.frame(page_part)
  page_part %<>% filter(page_part != "")
  page_part %<>% filter(!str_detect(page_part, "COUNTRY‑BY‑COUNTRY REPORTING"))

  if (str_detect(path, "2018|2019|2020")) {
  page_part$page_part[str_which(page_part$page_part, "Part")] %<>% trimws()
  page_part %<>% separate(page_part, into = c("part", "name", "text"), sep = " {2,}")
  page_part %<>% mutate(text = ifelse(is.na(text) & str_detect(part, "Part"), "-",  text))
  page_part %<>% mutate(text  = ifelse(is.na(text), name, text))
  page_part %<>% t_pdf_tbl(pl = str_which(page_part$part, "Part"), column = "text")
  } else {
    page_part %<>% mutate(page_part = str_remove_all(page_part, ""))
    page_part$page_part %<>% trimws()
    page_part %<>% separate(page_part, into = c("name", "text"), sep = " {2,}")
    other_rows <- which(is.na(page_part$text))
    page_part %<>% mutate(text = ifelse(is.na(text), name, text))
    if (length(other_rows > 0)) {
      page_part$name[other_rows] <- NA
      pl <- str_which(str_sub(page_part$name, 1, 1), "[:upper:]")
      page_part %<>% mutate(name = replace_na(name, ""))
      page_part %<>% t_pdf_tbl(pl = pl, column = c("name", "text"))
      page_part %<>% mutate(across(everything(), ~ str_squish(.x)))
    }
  page_part <- page_part[-1, ]
  page_part %<>% mutate(part = case_when(name == "Domestic legal and administrative framework" ~ "Part A",
                                         name == "Exchange of information framework" ~ "Part B",
                                         name == "Appropriate use" ~ "Part C"))
  }

  page_part %<>% mutate(iso3c = toc[x])
  return(page_part)
}

cbcr_table <- map(seq_along(toc), read_table, cbcrtext, toc)
cbcr_table %<>% reduce(rbind)
cbcr_table %<>% mutate(text = ifelse(str_sub(text, 1, 1) == "-", "-", text))
cbcr_table %<>% mutate(phase = str_sub(path, -8, -5))
return(cbcr_table)
}


paths <- list(here("data-raw/cbcr_peer_review/2018.pdf"),
              here("data-raw/cbcr_peer_review/2019.pdf"),
              here("data-raw/cbcr_peer_review/2020.pdf"),
              here("data-raw/cbcr_peer_review/2021.pdf"),
              here("data-raw/cbcr_peer_review/2022.pdf"))

toc_pages <- list(c(7, 8, 9, 10),
                  c(7, 8, 9, 10),
                  c(6, 7, 8, 9),
                  c(6, 7, 8, 9),
                  c(6, 7, 8, 9))

cbcr_tables <- map2(paths, toc_pages, extract_cbcrs)
cbcr_tables %<>% reduce(rbind)
cbcr_tables %<>% mutate(part = ifelse(str_sub(part, 1, 6) == "Part C", "Part C", part))
write_csv(cbcr_tables, here("data-created/cbcr_peer_reviews_summaries.csv"))
beepr::beep()

