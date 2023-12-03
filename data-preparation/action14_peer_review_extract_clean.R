# Extract and clean MAP Peer Review reports

## load libraries
library(stringr)
library(tidyverse)
library(here)
library(pdftools)
library(reticulate)
library(qpdf)
library(countrycode)
library(lubridate)

devtools::load_all()

files <- list.files(here("data-raw/map_peer_review"))
paths <- map_chr(files, function(x, y){paste0(y, "/", x)}, here("data-raw/map_peer_review/"))


# identify sections in pdf

identify_sections <- function(path){
  extract <- pdftools::pdf_text(path)
  first_lines <- str_sub(extract, start = 1, end = str_locate(extract, "\\n")[,1])
  heading1 <- str_which(first_lines, "Summary|SUMMARY")
  not_in <- str_which(first_lines, "Executive|EXECUTIVE|Annex A")
  heading1 <- setdiff(heading1, not_in)
  return(heading1)
}

pages <- map(paths, identify_sections)

# read info from pdfs

countries <- str_remove_all(files, "-stage-1.pdf|-stage-2.pdf")
iso3c <- countrycode(countryname(countries), "country.name", "iso3c")
stages <- str_extract_all(files, "stage-1|stage-2") %>% flatten_chr

stream_countries_numbers <- which(iso3c %in% c("EST", "GRC", "HUN", "ISL", "ROU", "SVK", "SVN", "TUR") & stages == "stage-1")

iso3c_std <- iso3c[-stream_countries_numbers]
stages_std <- stages[-stream_countries_numbers]
paths_std <- paths[-stream_countries_numbers]
pages_std <- pages[-stream_countries_numbers]

iso3c_ext <- iso3c[stream_countries_numbers]
stages_ext <- stages[stream_countries_numbers]
paths_ext <- paths[stream_countries_numbers]
pages_ext <- pages[stream_countries_numbers]



clean_table <- function(table){
  align_names <- function(subtable) {
    if (ncol(subtable) == 4) {
      subtable %<>% mutate(`1` = paste0(`1`, `2`))
      subtable %<>% select(-`2`)
      names(subtable) <- c("Part", "Area_for_improvement", "Recommendation")
    }
    if (ncol(subtable) == 7) {
      subtable %<>% mutate(`1` = paste0(`1`, `2`, `3`, `4`, `6`))
      subtable %<>% select(-c(`2`, `3`, `4`, `5`, `6`))
      names(subtable) <- c("Area_for_improvement", "Recommendation")
      subtable %<>% mutate(Part = "")
    }
    if (ncol(subtable) == 3) {
      names(subtable) <- c("Part", "Area_for_improvement", "Recommendation")
    }
    if (ncol(subtable) == 2) {
      names(subtable) <- c("Part", "Area_for_improvement")
      subtable %<>% mutate(Recommendation = "")
      }
    if (ncol(subtable) == 1) {
      names(subtable) <- c("Part")
      subtable %<>% mutate(Area_for_improvement = "",
      Recommendation = "")
    }


    return(subtable)
  }
  table %<>% map(align_names)
  table %<>% map(slice, -1)
  table %<>% reduce(rbind)

  table %<>% filter(str_detect(Part, "\\[") | Part == "")
  heading_names <- c("Part A: Preventing disputes",
                     "Part B: Availability and access to MAP",
                     "Part C: Resolution of MAP cases",
                     "Part D: Implementation of MAP agreements")
  table %<>% filter(!Part %in% heading_names &
                      !Area_for_improvement %in% heading_names &
                      !Recommendation %in% heading_names)
  #table %<>% filter(!str_detect(Part, "Area|Part"))
  pl <- which(str_detect(table$Part, "\\["))
  table %<>% t_pdf_tbl(pl)
}


all_tables <- pmap(list(paths_std, pages_std, "lattice"), extract_tables)

all_tables %<>% map(clean_table)
all_tables %<>% map2(iso3c_std, ~ mutate(.x, iso3c = .y))
all_tables %<>% map2(stages_std, ~ mutate(.x, iso3c = .y))
beepr::beep()
all_tables %<>% reduce(rbind)
write_csv(all_tables, here("data-created/map_reviews_int.csv"))



extract_new <- function(parameter){
  print(paste(parameter[[1]], parameter[[5]]))
  test <- pdftools::pdf_data(parameter[[2]])
  test <- test[parameter[[3]]]

basic_filters <- function(df){
  df %<>% filter(height <= 11 & y > 50 & y < 720)
  df %<>% filter(!str_detect(text, "SUMMARY"))
  df %<>% filter(!str_detect(text, "│"))
  df %<>% filter(!str_detect(text, "ò|↓"))
  df %<>% mutate(diff_to_prev = c(1, diff(y)))
  df %<>% mutate(len = x + width)
  df %<>% mutate(diff_to_x = c(1, diff(x)))
}

test %<>% map(basic_filters)

get_positions <- function(df){
  df <- df %>% reduce(rbind)
  end_first_col <- df$len[which(df$text == "[B.10]")]
  second_col <-  end_first_col - 4

  third_col <- df %>%
    filter(!text %in% c("-", "•")) %>%
    group_by(x) %>%
    summarise(n = n()) %>%
    filter(x > end_first_col + 100) %>%
    arrange(desc(n)) %>%
    mutate(differ = c(101, diff(x))) %>%
    filter(abs(differ) > 100) %>%
    slice(1) %>%
    pull(x)

  if (length(third_col) != 1) {
boundaries <- df %>%
    filter(text == "-" & x > end_first_col + 50) %>%
    group_by(x) %>%
    summarise(n = n()) %>%
    arrange(desc(n)) %>%
    mutate(difference = c(81, diff(x))) %>%
    filter(difference > 80) %>%
    slice(1:2) %>%
    pull(x)
third_col <- boundaries[1] + (boundaries[1] - second_col)
  }

  positions <- c(second_col, third_col)

  return(positions)
}

positions <- get_positions(test)

clean_new <- function(test1, positions, parameter){

test1 %<>% mutate(column_tab = case_when(x < positions[1] ~ 1,
                                           x >= positions[1] & x < positions[2] ~ 2,
                                           x >= positions[2] ~ 3))

test1 %<>% mutate(column_tab_f = case_when(x < positions[1] ~ 1,
                                             x >= positions[1] ~ 2))
test1 %<>% group_by(column_tab_f) %>% mutate(new_part = c(20, diff(y)))

part_change <- 15

if (parameter[[4]] == "stream") {
  part_change <- test1 %>%
    filter(column_tab != 1) %>%
    group_by(new_part) %>%
    summarise(n = n()) %>%
    filter(new_part > 11) %>%
    arrange(desc(n)) %>%
    slice(1) %>%
    pull(new_part)
}

test1 %<>% group_by(column_tab_f) %>% arrange(y) %>% mutate(jumps = c(20, diff(y)))

part_changes <- which(test1$jumps >= part_change)
test1 %<>% mutate(part_no = NA)
test1$part_no[part_changes] <- c(1:length(part_changes))
test1 %<>% fill(part_no, .direction = "down")

test1 %<>% mutate(overlap = ifelse(x < positions[2] & len > positions[2], 1, NA))
overlaps <- test1 %>% group_by(part_no) %>% summarise(n = sum(overlap, na.rm = TRUE))
overlaps %<>% mutate(n = ifelse(n > 0, "yes", "no"))
test1 %<>% left_join(overlaps, by = "part_no")
test1 %<>% mutate(column_tab = ifelse(column_tab == 3 & n == "yes", 2, column_tab))

test1 %<>% arrange(column_tab, y, x) %>% mutate(new_line = c(1, diff(y)))

test1 <- t_pdf_tbl(test1, pl = which(test1$new_line != 0), column = c("text"))
test1 %<>% filter(!text %in% c("Areas for Improvement", "Areas for improvement", "Recommendations"))


test1 %<>% filter(!text %in% c("Part A: Preventing", "disputes",
                               "Part A: Preventing disputes",
                               "Part B: Availability", "and access to MAP",
                               "Part B: Availability and",
                               "access to MAP",
                               "Part B: Availability and access to MAP",
                               "Part C: Resolution", "of MAP cases",
                               "Part C: Resolution of", "MAP cases",
                               "Part C: Resolution of MAP cases",
                               "Part D: Implementation", "of MAP agreements",
                               "Part D: Implementation of", "MAP agreements",
                               "Part D: Implementation of MAP agreements"))
test1 %<>% group_by(column_tab) %>% mutate(new_part = c(20, diff(y)))

test1 <- t_pdf_tbl(test1, pl = which(test1$new_part >= part_change), column = "text")


overlapping_rows <- test1 %>% filter(column_tab == 2 & n == "yes") %>% mutate(text = "overlap", column_tab = 3)
test1 %<>% bind_rows(overlapping_rows)

most_rows <- test1 %>% group_by(column_tab) %>% summarise(n = n()) %>% arrange(desc(n)) %>% filter(column_tab != 1) %>% slice(1) %>% pull(column_tab)

columns <- test1 %>% split(f = test1$column_tab)

search_index <- function(pointer, df){
  differences <- df$y - pointer
  differences
  index <- which.min(abs(differences))
  return(index)
}

if (length(columns) == 2) {
  if (most_rows == 2) {
    indices_3 <- map_int(columns[[1]]$y, search_index, columns[[2]])
    columns[[1]]$recommendation <- columns[[2]]$text[indices_3]
    df <- columns[[1]]
    df %<>% rename("improvement" = "text")
  }

  if (most_rows == 3) {
    indices_3 <- map_int(columns[[2]]$y, search_index, columns[[1]])
    columns[[2]]$improvement <- columns[[1]]$text[indices_3]
    df <- columns[[2]]
    df %<>% rename("recommendation" = "text")
  }

  df %<>% ungroup() %>%  select(improvement, recommendation)
  df %<>% mutate(category = NA)
} else {

if (most_rows == 1) {
  indices_2 <- map_int(columns[[1]]$y, search_index, columns[[2]])
  columns[[1]]$improvement <- columns[[2]]$text[indices_2]
  indices_3 <- map_int(columns[[1]]$y, search_index, columns[[3]])
  columns[[1]]$recommendation <- columns[[3]]$text[indices_3]
  df <- columns[[1]]
  df %<>% rename("category" = "text")
  }

if (most_rows == 2) {
  indices_2 <- map_int(columns[[2]]$y, search_index, columns[[1]])
  columns[[2]]$category <- columns[[1]]$text[indices_2]
  indices_3 <- map_int(columns[[2]]$y, search_index, columns[[3]])
  columns[[2]]$recommendation <- columns[[3]]$text[indices_3]
  df <- columns[[2]]
  df %<>% rename("improvement" = "text")
}

if (most_rows == 3) {
  indices_2 <- map_int(columns[[3]]$y, search_index, columns[[1]])
  columns[[3]]$category <- columns[[1]]$text[indices_2]
  indices_3 <- map_int(columns[[3]]$y, search_index, columns[[2]])
  columns[[3]]$improvement <- columns[[2]]$text[indices_3]
  df <- columns[[3]]
  df %<>% rename("recommendation" = "text")
}

df %<>% ungroup() %>%  select(category, improvement, recommendation)
}
return(df)
}


test %<>% map(clean_new, positions, parameter)
test %<>% reduce(rbind)
test %<>% mutate(category = ifelse(str_detect(category, "\\["), category, "" ))
# finish here
test %<>% mutate(differ = ifelse(category == lag(category, default = NA), 1, NA))
pl <- which(str_detect(test$category, "\\[") & is.na(test$differ))
test %<>% select(-differ)
test <- t_pdf_tbl(test, pl = pl, column = c("improvement", "recommendation"))
test %<>% mutate(iso3c = parameter[[1]],
                 stage = parameter[[5]])
return(test)
}


new_tables <- map(parameters_stream, extract_new)
beepr::beep()
new_tables %<>% reduce(rbind)


all_tables <- read_csv(here("data-created/map_reviews_int.csv"))

all_tables %<>% mutate(Part = str_remove_all(Part, "ò|↓"))

all_tables %<>% mutate(across(c("Area_for_improvement", "Recommendation"), ~ str_replace_all(.x, "\n", " ")))
all_tables %<>% mutate(across(c("Area_for_improvement", "Recommendation"), ~ trimws(.x)))



all_tables %<>% mutate(Part_new = str_sub(Part,
                                          str_locate(all_tables$Part, "\\[(?=[:alpha:])")[, 1],
                                          str_locate(all_tables$Part, "(?<=[:digit:])\\]")[, 1]))
all_tables %<>% mutate(additional_text = str_remove_all(Part, str_sub(Part_new, 2, -2)))
all_tables %<>% mutate(additional_text = str_remove_all(additional_text, "\\[|\\]"))
all_tables %<>% mutate(across(c("additional_text"), ~ str_replace_all(.x, "\n", " ")))
all_tables %<>% mutate(across(c("additional_text"), ~ trimws(.x)))
all_tables %<>% mutate(Area_for_improvement = ifelse((Area_for_improvement == "" | is.na(Area_for_improvement)) & additional_text != "", additional_text, Area_for_improvement))
all_tables %<>% mutate(Area_for_improvement = ifelse((Area_for_improvement == "" | is.na(Area_for_improvement)) & str_detect(Part, "-"), "-", Area_for_improvement))
all_tables %<>% mutate(Recommendation = ifelse((Recommendation == "" | is.na(Recommendation)) & str_count(Part, "-") == 2, "-", Recommendation))

all_tables %<>% mutate(improvement = ifelse(str_detect(Area_for_improvement, "maintain|continue|it will provide|it will give|possible at this stage to evaluate|was not yet possible to assess") & (is.na(Recommendation) | Recommendation %in% c("", "-")), "-", Area_for_improvement))
all_tables %<>% mutate(Recommendation = ifelse(str_detect(Area_for_improvement, "maintain|continue|it will provide|it will give|possible at this stage to evaluate|was not yet possible to assess") & (is.na(Recommendation) | Recommendation %in% c("", "-")), Area_for_improvement, Recommendation))

all_tables %<>% select(iso3c, stage, Part_new, improvement, Recommendation, Area_for_improvement)

all_tables %<>% mutate(Recommendation = ifelse(improvement %in% c("- -", "-  -"), "-", Recommendation))
all_tables %<>% mutate(improvement = ifelse(improvement %in% c("- -", "-  -"), "-", improvement))
all_tables %<>% mutate(improvement = ifelse(is.na(improvement), "-", improvement))



# for the moment, exclude those countries where extraction did not work well
all_tables %<>% select(-Area_for_improvement)
# dates

dates <- map(paths, pdf_info)
dates <- map(dates, pluck, 6)
dates %<>% flatten_dbl()

dates <- data.frame(iso3c, stages, dates)
dates %<>% mutate(dates = as_datetime(dates))
dates %<>% mutate(year = year(dates))

all_tables %<>% left_join(dates[c("iso3c", "stages", "year")], by = c("iso3c", "stage" = "stages"))


# add labels to part numbers

part_numbers <- data.frame(description =
                             c("[A.1]" = "treaty_interpretation_clause",
                               "[A.2]" = "apa_roll_back",
                               "[B.1]" = "domestic_remedies_three_years",
                  "[B.2]" = "request_either_ca",
                  "[B.3]" = "access_tp",
                  "[B.4]" = "access_anti_abuse",
                  "[B.5]" = "access_audit_settlement",
                  "[B.6]" = "access_information",
                  "[B.7]" = "double_taxation_other_than_treaty",
                  "[B.8]" = "guidelines_on_access",
                  "[B.9]" = "publish_MAP_profile",
                  "[B.10]" = "access_audit_settlement_in_guidelines",
                  "[C.1]" = "treaty_provision_resolution",
                  "[C.2]" = "resolution_24_months",
                  "[C.3]" = "resources_MAP_function",
                  "[C.4]" = "independence_MAP_function",
                  "[C.5]" = "performance_indicators",
                  "[C.6]" = "arbitration_position",
                  "[D.1]" = "implementing_agreements",
                  "[D.2]" = "timely_implementation",
                  "[D.3]" = "implementation_notwithstanding_time_limits"))
part_numbers <- tibble::rownames_to_column(part_numbers, "Part_new")

all_tables %<>% left_join(part_numbers)

write_csv(all_tables, here("data-created/map_reviews.csv"))
