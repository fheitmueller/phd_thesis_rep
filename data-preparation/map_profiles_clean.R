# Extract and clean MAP profiles

## load libraries
library(stringr)
library(tidyverse)
library(here)
library(qpdf)
library(countrycode)
devtools::load_all()


# read info from pdfs

files <- list.files(here("data-raw/map_profiles"))
paths <- map_chr(files, function(x, y){paste0(y, "/", x)}, here("data-raw/map_profiles/"))
page_lengths <- map_chr(paths, pdf_length)
page_lengths <- map(page_lengths, function(x) {c(1:x)})
countries <- str_sub(files, 1, -16)
iso3c <- countrycode(countryname(countries), "country.name", "iso3c")
dates <- str_sub(files, -14, -5)
tables <- pmap(list(paths, page_lengths, "stream"), extract_tables)


take_out_columns <- function(df){
  df %<>% mutate(across(where(is.character), ~ na_if(.,"")))
  nacounts <- as.integer(colSums(is.na(df)))
  df %<>% select(-which(nacounts == nrow(df)))
  if (ncol(df) > 5) {
    nacounts <- as.integer(colSums(is.na(df)))
    if ((nrow(df) - 1) %in% nacounts) {

      titel_only <- which(nacounts == nrow(df) - 1)
      col_nos <- length(titel_only)

      for (i in 1:col_nos) {
        nacounts <- as.integer(colSums(is.na(df)))
        titel_only <- which(nacounts == nrow(df) - 1)
        col_names <- c(colnames(df)[titel_only[[1]] - 1], colnames(df)[titel_only[[1]]])
        df %<>% unite("new", all_of(col_names), sep = " ", na.rm = TRUE)
        new_name <- "new"
        names(new_name) <-  paste0("new", i)
        df %<>% rename(all_of(new_name))
      }
    }
  }

  if (ncol(df) > 5) {
    df %<>% unite("3", c(3:4), sep = "", na.rm = TRUE)
  }

  return(df)
}


extract_clean <- function(df){
  df %<>% modify_if(~ ncol(.) > 5 , take_out_columns)

  cols <- map_int(df, ncol)
  df <- df[which(cols == 5)]
  df %<>% map(rename, c("rownumber" = 1, "question" = 2, "response" = 3, "detailed_explanation" = 4, "links" = 5))
  df %<>% reduce(rbind)
  df %<>% mutate(across(.cols = everything(), trimws))
  df %<>% mutate(rownumber = str_remove_all(rownumber, "•  Are|\\n|Where"))

  df %<>% filter(!(rownumber %in% c("s/n", "s/\nn", "s/\nn ", "s\n/n", "Point", "P\noint", "point", "and access to MAP?") |
                     str_sub(rownumber, 1, 1) %in% c("s", "W", "P", "n") |
                     str_detect(rownumber, "APA") |
                     str_detect(rownumber, "s/n")))
  df %<>% filter(!(response %in% c("Réponse", "Response")))
  df <- df[-(which(str_sub(df$rownumber, 1, 1) %in% c("A", "B", "C", "D"))), ]

  treaties <- which(df$rownumber == "Your jurisdiction")
  if (!is_empty(treaties)) {
    df %<>% slice(1:(treaties[1] - 1))
  }

  df %<>% t_pdf_tbl(which(df$rownumber != "" |
                            str_sub(df$question, 1, 1) %in% c(0:9) |
                            str_sub(df$question, 1, 1) == "-"))


  df %<>% mutate(question = str_replace_all(question, "  | \n|●|•", ""))
  df %<>% mutate(new_row = as.character(c(1:39)))
}


tables %<>% map(extract_clean)
tables %<>% map2(iso3c, ~ mutate(.x, iso3c = .y))
tables %<>% map2(dates, ~ mutate(.x, dates = .y))
tables %<>% reduce(rbind)


write_csv(tables, here("data-created/map_profiles.csv"))

