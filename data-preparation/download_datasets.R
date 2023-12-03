# download datasets through APIs

library(WDI)
library(OECD)
library(eurostat)
library(bea.R)
library(tidyverse)
library(magrittr)
library(rvest)
library(XML)
library(countrycode)



download_dataset <- function(all_years){

# countries included in ICTD dataset
  included_countries <- c("Afghanistan", "Albania", "Algeria", "Angola", "Argentina", "Armenia", "Azerbaijan", "Bangladesh", "Benin", "Bhutan", "Bolivia", "Bosnia and Herzegovina", "Botswana", "Brazil", "Burkina Faso", "Burundi", "Cabo Verde", "Cambodia", "Cameroon", "Central African Republic", "Chad", "China", "Colombia", "Comoros", "Côte d'Ivoire", "Democratic People’s Republic of Korea", "Democratic Republic of Congo", "Djibouti", "Dominican Republic", "Ecuador", "Egypt", "El Salvador", "Equatorial Guinea", "Eritrea", "Eswatini", "Ethiopia", "Federal States of Micronesia", "Gabon", "Georgia", "Ghana", "Guatemala", "Guinea", "Guinea-Bissau", "Guyana", "Haiti", "Honduras", "India", "Indonesia", "Iran", "Iraq", "Jordan", "Kenya", "Kiribati", "Kosovo", "Kyrgyz Republic", "Lao PDR", "Lebanon", "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", "Maldives", "Mali", "Marshall Islands", "Mauritania", "Mauritius", "Mexico", "Moldova", "Mongolia", "Morocco", "Mozambique", "Myanmar", "Namibia", "Nepal", "Nicaragua", "Niger", "Nigeria", "North Macedonia", "Pakistan", "Papua New Guinea", "Paraguay", "Peru", "Philippines", "Republic of Congo", "Rwanda", "Samoa", "São Tomé and Principe", "Senegal", "Seychelles", "Sierra Leone", "Solomon Islands", "Somalia", "South Africa", "South Sudan", "Sri Lanka", "Sudan", "Syrian Arab Republic", "Tajikistan", "Tanzania", "Thailand", "The Gambia", "Timor-Leste", "Togo", "Tonga", "Trinidad and Tobago", "Tunisia", "Turkmenistan", "Uganda", "Ukraine", "Uzbekistan", "Vanuatu", "Venezuela", "Vietnam", "West Bank and Gaza", "Yemen", "Zambia", "Zimbabwe")
  included_countries <- tibble(included_countries, iso3c = countrycode(coalesce(countryname(included_countries), included_countries),
                                                                     "country.name",
                                                                     "iso3c",
                                                                     custom_match = c("Federal States of Micronesia" = "FSM",
                                                                                      "Kosovo" = "KSV")))
  write_csv(included_countries, here("data-raw/included_countries_ictd.csv"))


# oecd members
page <- read_html("https://www.oecd.org/about/document/ratification-oecd-convention.htm")
table <- html_table(page, fill = TRUE)
oecd_members <- table[[2]]
oecd_members <- oecd_members[-1, -c(1,4)]
colnames(oecd_members) <- c("country", "ratification_date")
oecd_members$country %<>% str_to_title
oecd_members %<>% mutate(iso3c = countrycode(countryname(country), "country.name", "iso3c"))
oecd_members %<>% mutate(ratification_year = str_sub(ratification_date, -4,-1))
write_csv(oecd_members, here("data-created/oecd_members.csv"))


# World Bank
world_bank <- WDI(
  indicator = c("NY.GDP.MKTP.CD", "NY.GDP.PCAP.CD"),
  start = all_years[1],
  end = all_years[length(all_years)],
  extra = TRUE
)
world_bank %<>% filter(region != "Aggregates")
## filter out those for which no data for no year is available
world_bank %<>%
  group_by(country) %>%
  filter(!all(is.na(NY.GDP.MKTP.CD)) & !all(is.na(NY.GDP.PCAP.CD)))
write_csv(world_bank, here("data-raw/wdi.csv"))

# OECD FDI income
oecd_members <- c("Austria", "Australia", "Belgium", "Canada", "Chile", "Colombia", "Costa Rica", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Israel", "Italy", "Japan", "Korea", "Latvia", "Lithuania", "Luxembourg", "Mexico", "The Netherlands", "New Zealand", "Norway", "Poland", "Portugal", "Slovak Republic", "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey", "United Kingdom", "United States")
oecd_members <- countrycode(countryname(oecd_members), "country.name", "iso3c")
# taking out Costa Rica, because data not yet available for all datasets and creates error otherwise
oecd_members <- oecd_members[-c(which(oecd_members == "CRI"))]

  oecd_fdi_income1 <- get_dataset(
    "FDI_INC_CTRY",
    filter = list(
      c(oecd_members[1:5]),
      c("USD"),
      c("DO"),
      c(""),
      c("ALL", "RSP"),
      c("NET"),
      c("IMC"),
      c(host_country_data_available$iso3c)
    ),
    start_time = all_years[1],
    end_time = all_years[length(all_years)],
    pre_formatted = FALSE
  )
  oecd_fdi_income2 <- get_dataset(
    "FDI_INC_CTRY",
    filter = list(
      c(oecd_members[6:10]),
      c("USD"),
      c("DO"),
      c(""),
      c("ALL", "RSP"),
      c("NET"),
      c("IMC"),
      c(host_country_data_available$iso3c)
    ),
    start_time = all_years[1],
    end_time = all_years[length(all_years)],
    pre_formatted = FALSE
  )
  oecd_fdi_income3 <- get_dataset(
    "FDI_INC_CTRY",
    filter = list(
      c(oecd_members[11:15]),
      c("USD"),
      c("DO"),
      c(""),
      c("ALL", "RSP"),
      c("NET"),
      c("IMC"),
      c(host_country_data_available$iso3c)
    ),
    start_time = all_years[1],
    end_time = all_years[length(all_years)],
    pre_formatted = FALSE
  )
  oecd_fdi_income4 <- get_dataset(
    "FDI_INC_CTRY",
    filter = list(
      c(oecd_members[16:20]),
      c("USD"),
      c("DO"),
      c(""),
      c("ALL", "RSP"),
      c("NET"),
      c("IMC"),
      c(host_country_data_available$iso3c)
    ),
    start_time = all_years[1],
    end_time = all_years[length(all_years)],
    pre_formatted = FALSE
  )
  oecd_fdi_income5 <- get_dataset(
    "FDI_INC_CTRY",
    filter = list(
      c(oecd_members[21:25]),
      c("USD"),
      c("DO"),
      c(""),
      c("ALL", "RSP"),
      c("NET"),
      c("IMC"),
      c(host_country_data_available$iso3c)
    ),
    start_time = all_years[1],
    end_time = all_years[length(all_years)],
    pre_formatted = FALSE
  )
  oecd_fdi_income6 <- get_dataset(
    "FDI_INC_CTRY",
    filter = list(
      c(oecd_members[26:30]),
      c("USD"),
      c("DO"),
      c(""),
      c("ALL", "RSP"),
      c("NET"),
      c("IMC"),
      c(host_country_data_available$iso3c)
    ),
    start_time = all_years[1],
    end_time = all_years[length(all_years)],
    pre_formatted = FALSE
  )
  oecd_fdi_income7 <- get_dataset(
    "FDI_INC_CTRY",
    filter = list(
      c(oecd_members[31:37]),
      c("USD"),
      c("DO"),
      c(""),
      c("ALL", "RSP"),
      c("NET"),
      c("IMC"),
      c(host_country_data_available$iso3c)
    ),
    start_time = all_years[1],
    end_time = all_years[length(all_years)],
    pre_formatted = FALSE
  )
  oecd_fdi_income <- rbind(oecd_fdi_income1, oecd_fdi_income2, oecd_fdi_income3, oecd_fdi_income4, oecd_fdi_income5, oecd_fdi_income6, oecd_fdi_income7)
  write_csv(oecd_fdi_income, here("data-raw/oecd_fdi_income.csv"))

# S means all services, SH is charges for intellectual property, and SJ is other business services
oecd_trade_services <- get_dataset(
  "BATIS_EBOPS2010",
  filter = list(
    c(oecd_members),
    c(host_country_data_available$iso3c),
    c("C"),
    c("S", "SH", "SJ"),
    c("RV")
  ),
  start_time = all_years[1],
  end_time = all_years[length(all_years)],
  pre_formatted = FALSE
)
write_csv(oecd_trade_services, here("data-raw/oecd_trade_services.csv"))

# OECD FDI general

oecd_fdi_general <- function(oecd_members){
  oecd_fdi = try(OECD::get_dataset(
    "FDI_POS_CTRY",
    filter = list(
      c(oecd_members),
      c("USD"),
      c(""),
      c(""),
      c("ALL"),
      c("NET"),
      c("IMC"),
      c("")
    ),
    start_time = all_years[1],
    end_time = all_years[length(all_years)-1],
    pre_formatted = FALSE
  ))
  return(oecd_fdi)
}

oecd_fdi_general <- lapply(oecd_members, oecd_fdi_general)
oecd_fdi_general <- oecd_fdi_general[-24]
oecd_fdi_general %<>% map(select, COU, FDI_TYPE,  COUNTERPART_AREA, MEASURE_PRINCIPLE, Time, ObsValue, MEASURE)
oecd_fdi_general %<>% reduce(rbind)
colnames(oecd_fdi_general) <- c("reporter", "type_fdi", "counterpart", "direction", "year", "value", "indicator")
#indicator is actually in Millions
oecd_fdi_general$indicator %<>% recode("USD" = "million_USD")
write_csv(oecd_fdi_general, here("data-raw/fdi_data/oecd_fdi_general.csv"))

# OECD FDI SPE shares
oecd_spe_reporters <- c("NLD", "BEL", "LUX", "EST", "HUN", "PRT", "ESP")
oecd_fdi <- function(oecd_spe_reporter){
oecd_fdi = OECD::get_dataset(
  "FDI_POS_CTRY",
  filter = list(
    c(oecd_spe_reporter),
    c("USD"),
    c("DO"),
    c(""),
    c("RSP"),
    c("NET"),
    c("IMC"),
    c(host_country_data_available$iso3c)
  ),
  start_time = all_years[1],
  end_time = all_years[length(all_years)-1],
  pre_formatted = FALSE
)
return(oecd_fdi)
}
oecd_spe_fdi <- lapply(oecd_spe_reporters, oecd_fdi)
oecd_spe_fdi %<>% map(select, COU, FDI_TYPE,  COUNTERPART_AREA, Time, ObsValue, MEASURE)
oecd_spe_fdi %<>% reduce(rbind)
colnames(oecd_spe_fdi) <- c("home", "type_fdi", "host", "year", "value", "indicator")
#indicator is actually in Millions
oecd_spe_fdi$indicator %<>% recode("USD" = "million_USD")
write_csv(oecd_spe_fdi, here("data-raw/fdi_data/oecd_spe_fdi.csv"))

#eurostat

eurostat_spe_fdi <- get_eurostat("bop_fdi6_pos")

eurostat_spe_fdi %<>% filter(
  entity == "SPE" &
    partner %in% countrycode(host_country_data_available$iso3c, "iso3c", "eurostat") &
    stk_flow == "NO" &
    nace_r2 == "FDI")
eurostat_spe_fdi %<>% select(fdi_item, partner, geo, time, values, currency)
colnames(eurostat_spe_fdi) <- c("type_fdi", "host", "home", "year", "value", "indicator")
eurostat_spe_fdi %<>% mutate(host = countrycode(host, "eurostat", "iso3c"),
                             home = countrycode(home, "eurostat", "iso3c"))
eurostat_spe_fdi %<>% filter(!is.na(home))
eurostat_spe_fdi %<>% mutate(year = lubridate::year(year))
write_csv(eurostat_spe_fdi, here("data-raw/fdi_data/eurostat_spe_fdi.csv"))


# BEA

## the BEA Api key is in the global environment in a variable called "bea_key"


sets <- beaSets(Sys.getenv("bea_key"))
params <- beaParams(Sys.getenv("bea_key"), "IntlServTrade")
ctries <- beaParamVals(Sys.getenv("bea_key"), "IntlServTrade", "AreaOrCountry") %>% reduce(rbind)
ctries %<>% mutate(iso3c = countrycode(countryname(Desc), "country.name", "iso3c"))
ctries %<>% mutate(iso3n = countrycode(iso3c, "iso3c", "iso3n"))
ctries_request <- ctries %>% filter(iso3c %in% host_country_data_available$iso3c) %>% pull(Key)
ctries_request_n <- ctries %>% filter(iso3c %in% host_country_data_available$iso3c) %>% pull(iso3n)
services <- beaParamVals(Sys.getenv("bea_key"), "IntlServTrade", "TypeOfService")
aff <- beaParamVals(Sys.getenv("bea_key"), "IntlServTrade", "Affiliation")
years <- beaParamVals(Sys.getenv("bea_key"), "IntlServTrade", "Year")


us_trade_services <- data.frame()
for (i in 1:length(ctries_request)){
  for (j in 1:(length(all_years)-1)){
bea_spec <- list('UserID' = Sys.getenv("bea_key"),
                 'Method' = 'GetData',
                 'datasetname' = "IntlServTrade",
                 "TradeDirection" = "Exports",
                 "AreaOrCountry" = ctries_request[i],
                 "Affiliation" = "Affiliated",
                 "Frequency" = "A",
                 "TypeOfService" = "ChargesForTheUseOfIpNie",
                 "Year" = all_years[j])
us_trade_services1 <- beaGet(bea_spec, asString = FALSE, asList = FALSE, asTable = TRUE,
                            asWide = TRUE, isMeta = FALSE, iTableStyle = TRUE)
us_trade_services1 %<>% rename("value" = 9)
us_trade_services1 %<>% mutate(year = all_years[j])
us_trade_services <- rbind(us_trade_services, us_trade_services1)
  }
}

us_trade_services %<>% left_join(ctries[c("Key", "iso3c")], by = c("AreaOrCountry" = "Key"))
write_csv(us_trade_services, here("data-raw/us_trade_services.csv"))





#34 = royalties received from affiliates; 27 is direct investment income

params <- beaParams(Sys.getenv("bea_key"), "MNE")
indict <- beaParamVals(Sys.getenv("bea_key"), "MNE", "Investment") %>% reduce(rbind)
ctries <- beaParamVals(Sys.getenv("bea_key"), "MNE", "Country") %>% reduce(rbind)
ctries %<>% mutate(iso3c = countrycode(countryname(desc), "country.name", "iso3c"))
# excluding "UK islands" because cannot be correctly attributed
ctries_request <- ctries %>% filter(iso3c %in% host_country_data_available$iso3c & !key %in% c("455", "268", "266", "605", "604")) %>% pull(key)
ctries_request %<>% unique()

bea_spec <- list('UserID' = Sys.getenv("bea_key"),
                 'Method' = 'GetData',
                 'datasetname' = "MNE",
                 "DirectionOfInvestment" = "Outward",
                 "Country" = ctries_request[7],
                 "Classification" = "Country",
                 "SeriesID" = "34",
                 "Year" = all_years[1])
us_investment_income <- beaGet(bea_spec, asString = FALSE, asList = FALSE, asTable = TRUE,
                             asWide = TRUE, isMeta = FALSE, iTableStyle = TRUE)
us_trade_services1 %<>% rename("value" = 9)
us_trade_services1 %<>% mutate(year = all_years[j])
us_trade_services <- rbind(us_trade_services, us_trade_services1)


# OECD Statutory Income Tax Rates

oecd_rates <- OECD::get_dataset(
  "CTS_CIT",
    pre_formatted = FALSE
)
oecd_rates %<>% rename("iso3c" = "COU", "indicator" = "CORP_TAX") %>% select(-TIME_FORMAT)
write_csv(oecd_rates, here("data-raw/statutory_rates/oecd_rates.csv"))
}
