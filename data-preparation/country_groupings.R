# Groupings

library(tidyverse)
library(magrittr)
library(countrycode)
library(here)

inc_frame <- readRDS(here("data-created/inc_frame.rds"))

oecd_members <- read_csv(here("data-created/oecd_members.csv"))

wdi <- read_csv(here("data-raw/wdi.csv"))

low_tax <- readRDS(here("data-created/low_tax_jurisdictions.rds"))


wdi %<>% filter(year == 2020) %>% select(iso3c, income, region)
wdi$iso3c %<>% recode("XKX" = "KSV")
# For a few jurisdictions that are relevant for the data analyzed, there is no income group defined in the World Bank definition. Based on information about these countries GDP levels, these are added to the group of High income countries.
additional_rows <- tribble(
  ~iso3c, ~income, ~region,
  "AIA", "High income", "Latin America & Caribbean",
  "ANT", "High income", "Latin America & Caribbean",
  "BES", "High income", "Latin America & Caribbean",
  "COK", "High income", "East Asia & Pacific",
  "FLK", "High income", "Latin America & Caribbean",
  "GGY", "High income", "Europe & Central Asia",
  "GIB", "High income", "Europe & Central Asia",
  "JEY", "High income", "Europe & Central Asia",
  "MAF", "High income", "Latin America & Caribbean",
  "MSR", "High income", "Latin America & Caribbean",
  "MYT", "Upper middle income", "Sub-Saharan Africa",
  "NIU", "High income", "East Asia & Pacific",
  "SHN", "Upper middle income", "Sub-Saharan Africa",
  "SPM", "High income", "North America",
  "TWN", "High income", "East Asia & Pacific",
  "VGB", "High income", "East Asia & Pacific",
  "WLF", "Upper middle income", "East Asia & Pacific"
)

wdi <- rbind(wdi, additional_rows)
groupings <- wdi

#recode Venezuela to Lower Middle Income based on IMF estimate: https://www.imf.org/en/Publications/WEO/weo-database/2021/October/weo-report?c=299,&s=NGDPD,PPPGDP,NGDPDPC,PPPPC,&sy=2021&ey=2022&ssm=0&scsm=1&scc=0&ssd=1&ssc=0&sic=0&sort=country&ds=.&br=1
groupings$income[which(groupings$iso3c == "VEN")] <- "Lower middle income"

groupings %<>% mutate(income_new = ifelse(income %in% c("Lower middle income", "Low income"), "Lower income", income))

groupings %<>% mutate(inclusive_framework = ifelse(iso3c %in% inc_frame, "yes", "no"))
groupings %<>% mutate(oecd_member = ifelse(iso3c %in% oecd_members$iso3c, "yes", "no"))
groupings %<>% mutate(low_tax = ifelse(iso3c %in% low_tax, "yes", "no"))
groupings %<>% mutate(composite_group = ifelse(low_tax == "yes", "Low tax", income_new))
groupings %<>% mutate(composite_group_orig = ifelse(low_tax == "yes", "Low tax", income))

groupings %<>% mutate(composite_group = factor(composite_group, levels = c("High income", "Upper middle income", "Lower income", "Low tax")))
groupings %<>% mutate(composite_group_orig = factor(composite_group_orig, levels = c("High income", "Upper middle income", "Lower middle income", "Low income", "Low tax")))


groupings %<>% mutate(beps_44 = ifelse(iso3c != "CRI" & oecd_member == "yes" | iso3c %in% c("ARG", "BRA", "CHN", "COL", "IND", "IDN", "LVA", "RUS", "SAU", "ZFA"), "yes", "no"))

groupings %>% write_rds(here("data-created/groupings.rds"))


eu_members <- c("Austria",
                "Belgium",
                "Bulgaria",
                "Croatia",
                "Cyprus",
                "Czech Republic",
                "Denmark",
                "Estonia",
                "Finland",
                "France",
                "Germany",
                "Greece",
                "Hungary",
                "Ireland",
                "Italy",
                "Latvia",
                "Lithuania",
                "Luxembourg",
                "Malta",
                "Netherlands",
                "Poland",
                "Portugal",
                "Romania",
                "Slovakia",
                "Slovenia",
                "Spain",
                "Sweden")
eu_members %<>% as.data.frame()
colnames(eu_members) <- "country"
eu_members %<>% mutate(iso3c = countrycode(countryname(country), "country.name", "iso3c"))
write_csv(eu_members, here("data-created/eu_members.csv"))



eu_1999 <- c("Austria",
                "Belgium",
                "Denmark",
                "Finland",
                "France",
                "Germany",
                "Greece",
                "Ireland",
                "Italy",
                "Luxembourg",
                "Netherlands",
                "Portugal",
                "Spain",
                "Sweden",
             "United Kingdom")

eu_1999 %<>% as.data.frame()
colnames(eu_1999) <- "country"
eu_1999 %<>% mutate(iso3c = countrycode(countryname(country), "country.name", "iso3c"))
write_csv(eu_1999, here("data-created/eu_1999.csv"))
