# extract Action 6 peer review

library(tidyverse)
library(here)
library(countrycode)
library(qpdf)
library(pdftools)
devtools::load_all()


folder <- here("data-raw/action6_peer_review")
file_locations <- list.files(folder)
file_paths <- map_chr(file_locations, function(x,y){paste(y, x, sep = "/")}, folder)

page_list <- list(c(40:257), c(25:277), c(31:300), c(39:316))
methods <- rep("stream", times = 3)
regions <- rep("0,770,600,20", times = 3)

# procedure for the first three reports

tables <- pmap(list(file_paths[1:3], page_list[1:3], methods, regions), extract_tables_region)

clean_extract <- function(tablelist){

tablelist %<>% keep(~ ncol(.x) >= 4)
tablelist %<>% keep(~ length(which(as.integer(.x$`0`) %in% c(1:200))) != 0)

eliminate_before_1 <- function(df){
  row <- which(as.integer(df$`0`) %in% c(1:200))[1]
  df %<>% slice(-c(1:row - 1))
  df %<>% filter(!str_detect(1, "[:alpha:]"))
  df %<>% filter(!if_any(everything(), ~ grepl("PREVENTION OF TREATY ABUSE|JURISDICTIONAL DATA", .)))
  df %<>% select(!where(~ all(. == "")))
  if (ncol(df) == 9) {
    df$`6` <- paste(df$`6`, df$`7`, df$`8`)
    df %<>% select(-c(`7`, `8`))
  }
  return(df)
}

tablelist %<>% map(eliminate_before_1)

separate_columns <- function(df){
  cols_with <- which(str_detect(df[1, ], "\\n"))
  page_no <- attributes(df)$page
  for (i in 1:length(cols_with)) {
    cols_with_now <- which(str_detect(df[1, ], "\\n"))[1]
    df %<>% separate(col = cols_with_now, into = c(str_glue("new1_{i}"), str_glue("new2_{i}"), str_glue("new3_{i}")), sep = "\\n")
  }
  df %<>% select(!where(~ all(is.na(.))))
  df %<>% mutate(across(everything(), ~ replace_na(.x, "")))
  attributes(df)$page <- page_no
  return(df)
}
tablelist %<>% modify_if(~ ncol(.x) < 6, ~ separate_columns(.x))

tablelist %<>% modify_if(~ ncol(.x) == 6, ~ mutate(.x, comments = ""))
set_names <- function(df){
  names(df) <- c("number", "partner", "compliance", "alternative", "signature", "alternative_other_MLI", "comments")
  return(df)
}
tablelist %<>% map(set_names)
reduce_tables <- function(df){
  df %<>% filter(!str_detect(number, "JURISDICTIONAL DATA|PREVENTION OF TREATY ABUSE"))
  df %<>% filter(!str_detect(alternative, "JURISDICTIONAL DATA|PREVENTION OF TREATY ABUSE"))
  pl <- which(df$number != "")
  df %<>% t_pdf_tbl(pl = pl)
  df %<>% mutate(across(everything(), ~ str_squish(.x)))
}
tablelist %<>% map(reduce_tables)
tablelist %<>% map(function(x) {mutate(x, page_no = attributes(x)$page)})
tablelist %<>% reduce(rbind)
return(tablelist)
}

tables %<>% map(clean_extract)




## special procedure for the fourth report

text <- pdf_text(file_paths[4])
text <- text[page_list[[4]]]
lines <- map(text, read_lines)
lines %<>% map(as.data.frame)

filter_out <- function(df){
 names(df) <- "text"
 df %<>% filter(str_detect(text, "Summary of the jurisdiction response|Other agreements|Other Agreements") |
                  str_sub(text, 1, 6) == "      " |
                  str_detect(text, "[:blank:]*[:digit:]{1,2}[:blank:]{2,}") |
                  str_detect(text, "Yes MLI"))
 df %<>% filter(!str_detect(text, "PREVENTION OF TAX TREATY ABUSE|"))
}
lines %<>% map(filter_out)

split_columns <- function(df){

  #splitting the page into "Other agreement" tables and "summary of the jurisdiction response" tables, based on the heading and number of times a yes or no answer is in a line
  split_jur <- which(str_detect(df$text, "Summary of the jurisdiction response"))
  split_other <- which(str_detect(df$text, "Other agreements|Other Agreements"))

  if (!is_empty(split_other)) {
    jur_response <- df %>% slice(1:split_other )
    other_agreements <- df %>% slice(c(split_other:nrow(df)))
  } else {
    if (!is_empty(split_jur)) {
      jur_response <- df
      other_agreements <- df %>% slice(0)
    } else {
      # to deal with issue of the o from the Greek alphabet in the table on Greece
      df %<>% mutate(text = str_replace_all(text, "Νο", "No"))
     if (max(str_count(df$text, "Yes(?![:alpha:])|No(?![:alpha:])")) > 1) {
       jur_response <- df
       other_agreements <- df %>% slice(0)
     } else {
       other_agreements <- df
       jur_response <- df %>% slice(0)
     }
    }
  }


  jur_response %<>% filter(!str_detect(text, "Summary of the jurisdiction response|Other agreements|Other Agreements|1.Treaty partners|instrument"))
  other_agreements %<>% filter(!str_detect(text, "Summary of the jurisdiction response|Other agreements|Other Agreements|1.Treaty partners|instrument"))


  border1 <- min(str_locate(jur_response$text, "[:alpha:]")[, 1], na.rm = TRUE)
  border2 <- min(str_locate(jur_response$text, "Yes |No ")[, 1], na.rm = TRUE)
  border3 <- min(str_locate(jur_response$text, "PPT|LOB|N/A")[, 1], na.rm = TRUE)
  if (is.infinite(border3)) {
    border3 <- max(str_count(jur_response$text)) + 1
  }

  jur_response %<>% mutate(number = str_sub(text, 1, border1 - 1),
                           partner = str_sub(text, border1, border2 - 1),
                           compliance = str_sub(text, border2, border2 + 11),
                           signature = str_sub(text, border2 + 12, border3 - 1),
                           minimum_standard = str_sub(text, border3, -1),
                           type = "jur_response",
                           inc_frame_member = NA)
  border1 <- min(str_locate(other_agreements$text, "[:alpha:]")[, 1], na.rm = TRUE)
  border2 <- min(str_locate(other_agreements$text, "Yes(?![:alpha:])|No(?![:alpha:])")[, 1], na.rm = TRUE)

  other_agreements %<>% mutate(number = str_sub(text, 1, border1 - 1),
                               partner = str_sub(text, border1, border2 - 1),
                               inc_frame_member = str_sub(text, border2, -1),
                               compliance = "No",
                               signature = "No",
                               minimum_standard = "N/A",
                               type = "other_agreement")
  df <- rbind(jur_response, other_agreements)
  df %<>% select(-text)
  df %<>% mutate(across(everything(), ~ str_squish(.x)))
  df %<>% t_pdf_tbl(pl = which(df$number != ""), column = c(1:5))
  return(df)
}

lines_next <- map(lines, split_columns)
lines_next <- imap(lines_next, ~ mutate(.x, page_no = .y + 38))
lines_next %<>% reduce(rbind)


tables[[4]] <- lines_next




get_countries <- function(path, pages, fontname, fontsize){
  text_data <- pdf_data(path, font_info = T)
  text_data <- imap(text_data, ~ mutate(.x, page_no = .y)) # capital X is necessary, because there is already a variable called "x" in the respective data frame
  text_data %<>% reduce(rbind)
  text_data %<>% filter(page_no >= pages[[1]])
  countries <- text_data %>% filter(font_name == fontname & font_size == fontsize)
  # dealing with country names consisting in more than one word
  countries %<>% mutate(same_page = c(1, diff(page_no)))
  countries %<>% t_pdf_tbl(pl = which(countries$same_page != 0), column = "text")
  countries %<>% mutate(iso3c = countrycode(countryname(text), "country.name", "iso3c"))
  countries %<>% filter(!is.na(iso3c))
  return(countries)
}



ctry_parameters <- list(file_paths,
                        page_list,
                        c("MBLMLR+Times New Roman,Bold", rep("ABCDEE+ArialNarrow-Bold", times = 3)),
                        c(12, rep(26.04, times = 3)))

countries <- pmap(ctry_parameters, get_countries)

add_countries_dates <- function(tablelist, countries, year){

  params <- data.frame(page_no = c(countries$page_no[1]:countries$page_no[nrow(countries)]))
  params %<>% left_join(countries[c("page_no", "iso3c")], by = "page_no")
  params %<>% fill(iso3c, .direction = "down")
  tablelist %<>% left_join(params, by = "page_no")
  tablelist %<>% select(-page_no)
  tablelist %<>% mutate(year = year)
}

tables <- pmap(list(tables, countries, c(2018:2021)), add_countries_dates)
lines_next <- add_countries_dates(lines_next, countries[[4]], 2021)

coalesce_column <- function(df){
  df %<>% mutate(across(c("alternative", "alternative_other_MLI"), ~ na_if(.x, "N/A")))
  df %<>% mutate(minimum_standard = coalesce(alternative, alternative_other_MLI))
}
tables[1:3] %<>% map(coalesce_column)
tables %<>% reduce(bind_rows)
tables %<>% filter(!is.na(partner))
tables %<>% mutate(inc_frame_member = case_when(inc_frame_member == "Yes" ~ "yes",
                                                inc_frame_member == "No" ~ "no",
                                                str_detect(partner, "\\*") ~ "no",
                                                TRUE ~ "yes"))

write_csv(tables, here("data-created/action6_peer_review.csv"))

countries_reviewed <- map(countries, select, iso3c)
countries_reviewed %<>% map2(c(2018:2021), ~ mutate(.x, year = .y))



## paragraph B.

lines <- map(file_paths, pdf_text)
lines <- map2(lines, page_list, ~ .x[.y])

extract_para_b <- function(all_lines, countries_report){

  all_lines <- read_lines(all_lines)


  ## identify the lines in which a table starts and ends
  relevant_lines1 <- str_which(all_lines, "Implementation issues|B. Conclusion")

  if (min(diff(relevant_lines1)) < 10) {
    position <- which(diff(relevant_lines1) < 10) + 1
    relevant_lines1 <- relevant_lines1[-position]
  }

  relevant_lines2 <- str_which(all_lines, "Summary of the jurisdiction response|Other agreements")

  ## dealing with those jurisdictions that do not have a summary table (because they do not have tax treaties)
  new_corresp <- c()
  for (i in 1:length(relevant_lines1) - 1) {
    position <- which(relevant_lines2 > relevant_lines1[i] & relevant_lines2 < relevant_lines1[i + 1])
    if (length(position) != 0) {
          new_corresp[i] <- relevant_lines2[position]
    } else {
      new_corresp[i] <- relevant_lines1[i] + 3
    }
  }

new_corresp[length(relevant_lines1)]  <- relevant_lines2[length(relevant_lines2)]


  ## extract the content in between the start and end lines and put all of it in a list
  list_of_tables <- c()
  for (i in 1:length(relevant_lines1)) {
    vec <- data.frame(text = all_lines[(relevant_lines1[i] + 1):(new_corresp[i] - 1)])
    list_of_tables <- append(list_of_tables, list(vec))
  }

  take_out_footnotes <- function(df){
    start_foot <- which(!is.na(as.integer(df$text)))[1]
    end_foot <- str_which(df$text, "PREVENTION")[1]
    if (!is.na(start_foot) & !is.na(end_foot)) {
      df %<>% slice(-c(start_foot:end_foot))
    }
    return(df)
  }
  list_of_tables %<>% map(take_out_footnotes)
  list_of_tables %<>% map(filter, text != "")
  list_of_tables %<>% map(filter, !str_detect(text, "PEER REVIEW REPORT|PREVENTION OF TREATY ABUSE|"))
    list_of_tables <- map2(list_of_tables, countries_report$iso3c, ~ mutate(.x, iso3c = .y))
  list_of_tables %<>% reduce(rbind)
  list_of_tables %<>% mutate(text = str_squish(text))
  uniques <- unique(list_of_tables$iso3c)
  pl <- map_int(uniques, function(y, x){str_which(y, x)[[1]]}, list_of_tables$iso3c)
  list_of_tables %<>% t_pdf_tbl(pl = pl, column = "text")
  return(list_of_tables)
}

lines <- map2(lines, countries_reviewed, extract_para_b)
lines %<>% map2(c(2018:2021), ~ mutate(.x, year = .y))
lines %<>% reduce(rbind)
write_csv(lines, here("data-created/action6_recommendations.csv"))

countries_reviewed %<>% reduce(rbind)
write_csv(countries_reviewed, here("data-created/action6_countries_reviewed.csv"))
