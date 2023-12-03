# Number of treaties and treaty partners

library(tidyverse)
library(readxl)
library(here)
library(magrittr)
library(countrycode)

treaties <- read_excel(here("data-raw/treaties.xlsx"), col_names = FALSE)
names(treaties) <- "text"

treaties_bil <- treaties %>% slice(1:5077) %>% filter(!str_detect(text, "Chile - Colombia - Mexico - Peru"))

treaties_mul <- treaties %>% slice(5078:5094)
treaties_mul %<>% rbind(filter(treaties, str_detect(text, "Chile - Colombia - Mexico - Peru")))

treaties_bil$text %<>% str_remove_all("\\(")
treaties_bil$text %<>% str_remove_all("\\)")

treaties_bil %<>% mutate(countries = str_sub(text, start = 1, end = str_locate(text, "Income|Agreement|Individuals|Memorandum|Exchange")[, "start"] - 2))

treaties_bil %<>% mutate(signed = str_sub(text,
                                          start = str_locate(text, "Signed")[, "end"] + 2,
                                          end = str_locate(text, "Status")[, "start"] - 1))
treaties_bil %<>% mutate(status = str_sub(text,
                                            start = str_locate(text, "Status:")[, "end"] + 2,
                                            end = str_locate(text, "In Force:")[, "start"] - 1))
treaties_bil %<>% mutate(in_force = str_sub(text,
                                          start = str_locate(text, "In Force:")[, "end"] + 2,
                                          end = str_locate(text, "Effective:")[, "start"] - 1))


positions <- str_locate_all(treaties_bil$text, treaties_bil$countries)
positions %<>% transpose()
positions <- positions[[2]] %>% flatten_int()

treaties_bil %<>% add_column(positions = positions)

treaties_bil %<>% mutate(effective = str_sub(text,
                                            start = str_locate(text, "Effective:")[, "end"] + 2,
                                            end = coalesce(str_locate(text, "MLI Treaty Status:|Termination:")[, "start"],
                                                           positions) - 1))

treaties_bil %<>% mutate(terminated = str_sub(text,
                                             start = str_locate(text, "Termination:")[, "end"] + 2,
                                             end = coalesce(str_locate(text, "MLI Treaty Status:")[, "start"],
                                                            positions) - 1))


treaties_bil %<>% mutate(country_1 = str_sub(countries,
                                             start = 1,
                                             end = str_locate(countries, " - ")[, "start"] - 1))

treaties_bil %<>% mutate(country_2 = str_sub(countries,
                                             start = str_locate(countries, " - ")[, "end"] + 1,
                                             end = -1))

treaties_bil %<>% mutate(iso3c_1 = countrycode(countryname(country_1), "country.name", "iso3c"))
treaties_bil %<>% mutate(iso3c_2 = countrycode(countryname(country_2), "country.name", "iso3c"))


treaties_bil2 <- rename(treaties_bil, c("iso3c_1" = "iso3c_2", "iso3c_2" = "iso3c_1", "country_1" = "country_2","country_2" = "country_1"))

taxtreaties_double <- rbind(treaties_bil, treaties_bil2)

## attention now every treaty is twice in the dataset, but this facilitates country level analysis

## add an identifier for each country pair

number_treaties <- taxtreaties_double %>%
  filter(status == "In Force") %>%
  group_by(iso3c_1) %>%
  summarise(number_treaties = n())

number_treaties %<>% rename("iso3c" = "iso3c_1")

all_jur <- read_rds(here("data-created/groupings.rds"))
all_jur %<>% select(iso3c)
number_treaties <- left_join(all_jur, number_treaties)
number_treaties$number_treaties %<>% replace_na(0)

total_number_treaties_in_force <- treaties_bil %<>% filter(status == "In Force") %>% nrow()

write_csv(number_treaties, here("data-created/number_treaties_in_force.csv"))


# in force per year from ICTD dataset
taxtreaties_double <- read_csv(here("data-created/taxtreaties_double.csv"), show_col_types = FALSE)
included_countries <- read_csv(here("data-raw/included_countries_ictd.csv"), show_col_types = FALSE)
taxtreaties_double %<>% filter(C1CODE %in% included_countries$iso3c)

treaties_period <- map(1950:2021, treaties_in_year, taxtreaties_double)
treaties_period %<>% reduce(rbind)
treaties_period %<>% mutate(id = paste(paircode, datayear, sep = " - "))
number_treaties_year <- treaties_period %>% group_by(datayear, C1CODE) %>% distinct(C2LONG) %>% summarise(number = n())
number_treaties_year %<>% rename("iso3c"= "C1CODE", "year" = "datayear")
##assuming zero if not in dataset
all_countries <- included_countries %>%
  slice(rep(1:n(), each = length(c(1950:2021)))) %>%
  add_column(year = rep(c(1950:2021), times = nrow(included_countries)))
number_treaties_year <- left_join(all_countries, number_treaties_year)
number_treaties_year %<>% mutate(number = replace_na(number, 0))

write_csv(number_treaties_year, here("data-created/number_treaties_per_year.csv"))


