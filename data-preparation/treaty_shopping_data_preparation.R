#Data preparation for treaty shopping analysis

if (!require('here')) install.packages('here'); library('here')
if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')
if (!require('magrittr')) install.packages('magrittr'); library('magrittr')
if (!require('countrycode')) install.packages('countrycode'); library('countrycode')
if (!require('purrr')) install.packages('purrr'); library('purrr')
if (!require('readxl')) install.packages('readxl'); library('readxl')

devtools::load_all()




all_years <- c(2004:2021)

# loading and cleaning external datasets and table of datasets used

cols_to_select <- c("dividends_subst", "dividends_port", "interest_general", "interest_bank", "royalties_general", "royalties_copyright", "technical_services", "land_rich", "shares", "anti_avoidance_rule",  "id", "EFFECTIVE", "TYPE", "C1CODE", "C2CODE", "datayear")


prepare_treaty_data <- function(all_years, cols_to_select, modelling_mli_ratified = "no", modelling_specific_treaty_ratified = NA, modelling_year = NA){

  # add treaty information to overall dataset

  taxtreaties_double <- read_csv(here("data-created/taxtreaties_double.csv"))

  if (modelling_mli_ratified == "yes") {
    taxtreaties_double$EFFECTIVE[which(taxtreaties_double$TYPE == "Amended by MLI" & taxtreaties_double$STATUS == "Not In Force")] <- 2021
    taxtreaties_double$STATUS[which(taxtreaties_double$TYPE == "Amended by MLI" & taxtreaties_double$STATUS == "Not In Force")] <- "In Force"
  }

  if (!is.na(modelling_specific_treaty_ratified)) {
    taxtreaties_double$EFFECTIVE[which(taxtreaties_double$TREATYID %in% modelling_specific_treaty_ratified)] <- modelling_year
    taxtreaties_double$STATUS[which(taxtreaties_double$TREATYID %in% modelling_specific_treaty_ratified)] <- "In Force"
  }


  # treaties_in_year function in separate file
  treaties_period <- map(all_years, treaties_in_year, taxtreaties_double)
  treaties_period %<>% reduce(rbind)

  treaties_period %<>% mutate(id = paste(paircode, datayear, sep = " - "))
  treaties_period %<>% rename(
    "dividends_subst" = `10i`,
    "dividends_port" = `10iii`,
    "interest_general" = `11i`,
    "interest_bank" = `11ii`,
    "royalties_general" = `12i`,
    "royalties_copyright" = `12ia`,
    "technical_services" = `12iv`,
    "land_rich" = `13i`,
    "shares" = `13ii`,
    "anti_avoidance_rule" = `29i`)


  treaties_period %<>% select(all_of(cols_to_select))


  choose_more_recent_treaty <- function(data){
    data %<>%
      group_by(id) %>%
      arrange(desc(EFFECTIVE), .by_group = TRUE) %>%
      slice_head %>%
      ungroup
    return(data)
  }

  treaties_period %<>% choose_more_recent_treaty()
return(treaties_period)
}




preparing_treaty_shopping_data <- function(all_years, treaty_data, cols_to_select){

# UN
un_data <- read_csv(here("data-raw/un_pop.csv"), skip = 1)
un_data <- un_data[-c(1:899), ] #taking out data on regional as opposed to country-level
un_data %<>% mutate(iso3c = countrycode(`Region/Country/Area`, "iso3n", "iso3c")) ## problem with Channel Islands

# CEPII
geo_dist <- read_excel(here("data-raw/dist_cepii.xls"))
geo_dist %<>% mutate(paircode = paste(iso_o, iso_d, sep = " - "))

# World Bank

world_bank <- read_csv(here("data-raw/wdi.csv"))
world_bank %<>% filter(year %in% all_years)
world_bank %<>% group_by(iso3c) %>% filter(any(!is.na(`NY.GDP.MKTP.CD`)) &
                                             any(!is.na(`NY.GDP.PCAP.CD`)))



#Countries listed on treaties.tax website as included in the data collection effort (not all show up in the database, but we can then assume that they have not had any treaties)

included_countries <- read_csv(here("data-raw/included_countries_ictd.csv"))


# EY data

domestic_law <- read_csv(here("data-created/all_withholding.csv"))
domestic_law %<>% select(c("year", "rate_appl", "iso3c", "category"))
domestic_law %<>% rename("wth_rate_max" = "rate_appl")
domestic_law %<>% filter(category != "statutory_rate")

categories <- levels(as.factor(domestic_law$category))
categories <- categories[-which(categories %in% c("land_rich_indirect", "shares_indirect"))]

domestic_law %<>% pivot_wider(names_from = "category", values_from = "wth_rate_max")
domestic_law %<>% pivot_longer(all_of(categories), names_to = "category", values_to = "wth_rate_max")


# filter out those with missing years over the assessed period
domestic_law %<>% filter(year %in% all_years)
all_missing <- domestic_law %>%
  filter(is.na(wth_rate_max)) %>%
  select(iso3c)
domestic_law %<>% filter(!iso3c %in% all_missing$iso3c)

# checking overlapping data availability
host_country_data_available <- un_data %>%
  select(iso3c) %>%
  distinct(iso3c) %>%
  filter(iso3c %in% included_countries$iso3c &
           iso3c %in% domestic_law$iso3c &
           iso3c %in% geo_dist$iso_o)

home_country_data_available <- un_data %>%
  select(iso3c) %>%
  distinct(iso3c) %>%
  filter(iso3c %in% geo_dist$iso_o & iso3c %in% world_bank$iso3c)



# daydic data preparation

all_countries_bilateral <- host_country_data_available %>%
  slice(rep(1:n(), each = nrow(home_country_data_available))) %>%
  add_column(ctry2 = rep(home_country_data_available$iso3c, times = nrow(host_country_data_available)))

names(all_countries_bilateral) <- c("host", "home")

# eliminate the relations type GER - GER
all_countries_bilateral <-  all_countries_bilateral[-c(which(all_countries_bilateral$host == all_countries_bilateral$home)), ]

all_countries_bilateral %<>% mutate(paircode = paste(host, home, sep = " - "))

# add time invariant data
all_countries_bilateral %<>% left_join(geo_dist[c("comlang_off", "dist", "paircode")], by = "paircode")


# adding years for the chosen period

all_countries_bilateral_year <- all_countries_bilateral %>%
  slice(rep(1:n(), each = length(all_years))) %>%
  add_column(year = rep(all_years, times = nrow(all_countries_bilateral)))

all_countries_bilateral_year %<>%
  slice(rep(1:n(), each = length(categories))) %>%
  add_column(category = rep(categories, times = nrow(all_countries_bilateral_year)))

# add ID
all_countries_bilateral_year %<>% mutate(id = paste(host, home, year, sep = " - "))




cols_to_select <- cols_to_select[-which(cols_to_select %in% c("id", "EFFECTIVE", "anti_avoidance_rule", "TYPE", "C1CODE", "C2CODE", "datayear"))]
treaty_data %<>% select(-c(C1CODE, C2CODE, datayear))
treaty_data %<>% pivot_longer(cols = all_of(cols_to_select), names_to = "category", values_to = "treaty_rate")
# to deal with the situation where potentially two treaties are in force at the same time
## check whether this works with the as.numeric operator, maybe incorporate anti-avoidance in that, maybe put the function only at a later stage


all_countries_bilateral_year %<>% left_join(treaty_data, by = c("id", "category"))


# combine treaty data with domestic law data


all_countries_bilateral_year %<>% left_join(
  domestic_law,
  by = c("host" = "iso3c", "year", "category")
)

# choose the applicable rate for each bilateral relation for interest/dividend/royalty/technical services
# for capital gains, if the indicator says "YES", i.e. gains can be taxed by source country, the domestic rate replaces the treaty rate. If indicator says "NO", a treaty rate of 0 is assumed.

all_countries_bilateral_year$treaty_rate %<>% recode("NO" = "0")

all_countries_bilateral_year %<>% mutate(
  treaty_rate = as.double(ifelse(treaty_rate %in% c("NO LIMIT", "YES"), wth_rate_max, treaty_rate))
)


all_countries_bilateral_year %<>% mutate(appl_max = pmin(treaty_rate, wth_rate_max, na.rm = TRUE))

# add world bank data

all_countries_bilateral_year %<>% left_join(
  world_bank[c("iso3c", "year", "NY.GDP.MKTP.CD", "NY.GDP.PCAP.CD")],
  by = c("home" = "iso3c", "year" = "year")
)


conduits <- read_csv(here("data-created/conduits.csv"))

all_countries_bilateral_year %<>% left_join(conduits, by = c("home" = "iso3c", "year", "category"))
all_countries_bilateral_year$conduit %<>% replace_na(0)


# calculate minimum rate and shopping incentive

minimum_rates_fun <- function(data){
  minimum_rates <- data %>%
    filter(conduit == 1) %>%
    group_by(host, year, category) %>%
    summarise(min_rate_max = min(appl_max, na.rm = TRUE))

  data %<>% left_join(minimum_rates, by = c("host", "year", "category"))
  data %<>% mutate(rate_shopping = appl_max - min_rate_max)

  minimum_rates_no_aa <- data %>%
    filter(conduit == 1 & !anti_avoidance_rule %in% c("LOB", "LOB-PPT", "PPT")) %>%
    group_by(host, year, category) %>%
    summarise(min_rate_max_no_aa = min(appl_max, na.rm = TRUE))

  data %<>% left_join(minimum_rates_no_aa, by = c("host", "year", "category"))
  data %<>% mutate(rate_shopping_no_aa = appl_max - min_rate_max_no_aa)

  # setting 0 where there is no treaty with a conduit jurisdictions
  data$rate_shopping %<>% replace_na(0)
  data$rate_shopping_no_aa %<>% replace_na(0)

  # negative values as 0 (where treaty with conduit jurisdiction is less favourable than other)
  data %<>% mutate(rate_shopping = if_else(rate_shopping < 0, 0, rate_shopping))
  data %<>% mutate(rate_shopping_no_aa = if_else(rate_shopping_no_aa < 0, 0, rate_shopping_no_aa))

  #lowest available taking into account direct and indirect route
  data %<>% mutate(min_available = appl_max - rate_shopping)
  data %<>% mutate(min_available_no_aa = appl_max - rate_shopping_no_aa)

  return(data)
}

all_countries_bilateral_year %<>% minimum_rates_fun()

# add data on the average weighted mean for each type of flow across all countries in a given year, to calculate the incentive to engage into treaty shopping using a given jurisdiction
all_countries_bilateral_year %<>% calculate_weights()
means <- all_countries_means(all_countries_bilateral_year, "weight_const")

all_countries_bilateral_year %<>% left_join(means[c("year", "host", "category", "rate_weighted_mean")], by = c("year", "host", "category"))

all_countries_bilateral_year %<>% mutate(advantage_over_rest = rate_weighted_mean - appl_max)

# create dummy indicating whether the home country is a conduit and at the same time an equally or more advantageous rate with another conduit is available

eq_more_adv_conduit <- function(advantages, conduit){
  output <- vector()
  conduits <- which(conduit == 1, arr.ind = TRUE)
  for (i in 1:length(advantages)) {
    more_advantageous <- which(advantages >= advantages[i], arr.ind = TRUE)
    more_advantageous <- more_advantageous[-which(more_advantageous == i)]
    more_advantageous <- more_advantageous[which(more_advantageous %in% conduits)]
    more_advantageous_exist <- ifelse(length(more_advantageous) > 0, 1, 0)
    output[i] <-  more_advantageous_exist
  }
  return(output)
}

all_countries_bilateral_year %<>% group_by(host, year, category) %>%
  mutate(equally_more_advantageous_conduit_exists = conduit * eq_more_adv_conduit(advantage_over_rest, conduit)) %>% ungroup()


# add dummy indicating whether a tax treaty is in force between two jurisdictions
all_countries_bilateral_year %<>% mutate(treaty_in_place = ifelse(is.na(EFFECTIVE), 0, 1))
all_countries_bilateral_year %<>% select(-c(paircode, comlang_off, dist, EFFECTIVE, TYPE,  `NY.GDP.MKTP.CD`, `NY.GDP.PCAP.CD`, dist_diff_max, weight, weight_const, average_GDP, average_GDP_cap, wth_rate_max, treaty_rate, land_rich_indirect, shares_indirect, id))
all_countries_bilateral_year %<>% filter(!category %in% c("dividends_port", "interest_bank", "royalties_copyright"))

return(all_countries_bilateral_year)
}


treaties_yearly <- prepare_treaty_data(all_years, cols_to_select, modelling_mli_ratified = "no")

treaties_yearly_mli_mod <- prepare_treaty_data(all_years, cols_to_select, modelling_mli_ratified = "yes")

#write_csv(treaties_yearly, here("data-created/treaties_yearly.csv"))
#write_csv(treaties_yearly_mli_mod, here("data-created/treaties_yearly_mli_mod.csv"))

all_countries_bilateral_year <- preparing_treaty_shopping_data(all_years, treaties_yearly, cols_to_select)
#all_countries_bilateral_year_mli_mod <- preparing_treaty_shopping_data(all_years, treaties_yearly_mli_mod, cols_to_select)

write_csv(all_countries_bilateral_year, here("data-created/all_countries_bilateral_year.csv"))
#write_csv(all_countries_bilateral_year_mli_mod, here("data-created/all_countries_bilateral_year_mli_mod.csv"))

# Calculate as if the Nigeria Mauritius treaty was ratified in 2012

treaties_yearly_nga_mod <- prepare_treaty_data(all_years, cols_to_select, modelling_mli_ratified = "no", modelling_specific_treaty_ratified = 3858, modelling_year = 2012)
all_countries_bilateral_year_nga_mod <- preparing_treaty_shopping_data(all_years, treaties_yearly_nga_mod, cols_to_select)

all_countries_bilateral_year_nga_mod %<>% filter((host == "IND" & home == "MUS") |
                                (host == "SEN" & home == "MUS") |
                                (host == "COL" & home == "ESP") |
                                (host == "NGA" & home == "NLD") |
                                (host == "NGA" & home == "MUS"))

write_csv(all_countries_bilateral_year_nga_mod, here("data-created/all_countries_bilateral_year_nga_mod.csv"))




