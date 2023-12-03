# clean MAP statistics

library(tidyverse)
library(readxl)
library(here)
library(stringr)
library(magrittr)
library(countrycode)

files <- list.files(here("data-raw/map_statistics/"))
paths <- map_chr(files, function(x, y){paste0(y, "/", x)}, here("data-raw/map_statistics/"))
map_stats <- map(paths, read_excel)

names <- map(files, str_sub, 1, -6)

map_stats %<>% map2(names, function(x, y){mutate(x, table = y)})

set_names <- function(df, names){
  colnames(df) <- names
  return(df)
}


clean_map_stats <- function(df){

  map_stats1 <- df[c(1:2)]
  map_stats1 %<>% map(set_names, c("country", 2006:2015, "table"))
  map_stats1 %<>% map(filter, !is.na(country))
  map_stats1 %<>% map(mutate, across(2:11, ~ str_replace_all(.x, "--", "0")))
  map_stats1 %<>% map(mutate, across(2:11, ~ as.integer(.x)))
  map_stats1 %<>% map(pivot_longer, cols = c(2:11), names_to = "year", values_to = "value")
  map_stats1[[1]] %<>% rename("end_inventory" = "value")
  map_stats1[[2]] %<>% rename("cases_started" = "value")
  map_stats1 %<>% map(select, -table)
  map_stats1 %<>% map(mutate, iso3c = countrycode(countryname(country), "country.name", "iso3c"))
  map_stats1 <- left_join(map_stats1[[1]], map_stats1[[2]], by = c("iso3c", "year"))
  map_stats1 %<>% rename("country" = "country.x")
  map_stats1 %<>% select(-country.y)
  map_stats1 %<>% mutate(type = "all")
  map_stats1 %<>% mutate(across(c(end_inventory), as.character))
  #taking out those countries which only submitted MAP statistics after a specific date
  map_stats1 %<>% filter(!(iso3c %in% c("LVA", "CHN") & year < 2013 | iso3c == "CRI" & year < 2014 | iso3c == "LTU" & year < 2015))

  map_stats2 <- df[c(3:8)]
  map_stats2 %<>% map(set_names, c("country", "start_inventory", "cases_started", "cases_closed", "end_inventory", "table"))
  map_stats2 %<>% reduce(rbind)
  map_stats2 %<>% mutate(across(c(start_inventory, end_inventory), as.character))

  map_stats3 <- df[c(9:11)]
  map_stats3 %<>% map(set_names, c("country", "start_inventory", "cases_started", "cases_closed", "end_inventory", "months", "closing_ratio", "table"))
  map_stats3 %<>% reduce(rbind)

  map_stats4 <- df[c(12:17)]
  map_stats4 %<>% map(set_names, c("country", "start_inventory", "cases_started", "cases_closed", "end_inventory", "months", "closing_ratio", "portion_if_cases", "table"))
  map_stats4 %<>% reduce(rbind)

  df <- bind_rows(map_stats2, map_stats3, map_stats4)
  df %<>% separate(table, into = c("year", "type"), sep = "_")
  df %<>% mutate(iso3c = countrycode(countryname(country), "country.name", "iso3c"))
  df <- bind_rows(df, map_stats1)
  df %<>% filter(!is.na(iso3c))
  df %<>% mutate(across(c(start_inventory, cases_started, cases_closed, end_inventory), str_remove_all, "[:space:]"))
  df %<>% mutate(across(c(start_inventory, cases_started, cases_closed, end_inventory, year), as.integer))


  return(df)
}



map_stats %<>% clean_map_stats()
write_csv(map_stats, here("data-created/map_stats_clean.csv"))
