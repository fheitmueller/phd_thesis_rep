# makefile

# process raw datasets

library(here)

#historical Inclusive Framework composition
source(here("data-preparation/inclusive_framework_wayback_download.R"))

## the OECD peer review datasets

source(here("data-preparation/action6_peer_review_extract_clean.R"))
source(here("data-preparation/action13_peer_review_extract_clean.R"))
source(here("data-preparation/action14_peer_review_extract_clean.R"))
source(here("data-preparation/action14_bps_extract.R"))

# Creating data from the EY datasets, here the order is important
source(here("data-preparation/ey_tocs_extract.R")) # extracts the tables of contents
source(knitr::purl(here("data-preparation/ey_tables_extract.Rmd"), quiet = TRUE)) # extracts the tables and paragraphs
source(here("data-preparation/samples.R")) # identifying for which countries data is available in EY
source(knitr::purl(here("data-preparation/ey_withholding_taxes_clean.Rmd"), quiet = TRUE))
source(knitr::purl(here("data-preparation/ey_cap_gains_non_residents_clean.Rmd"), quiet = TRUE))
source(knitr::purl(here("data-preparation/ey_income_received_clean.Rmd"), quiet = TRUE))
source(knitr::purl(here("data-preparation/ey_special_regimes_clean.Rmd"), quiet = TRUE))
source(knitr::purl(here("data-preparation/tp_rules_assemble.Rmd"), quiet = TRUE))


# statutory rates
source(knitr::purl(here("data-preparation/statutory_rates_assemble.Rmd"), quiet = TRUE))

# creating a dataset with the different country groupings
source(here("data-preparation/country_groupings.R"))

# identifying conduit jurisdictions, later relevant in preparation of data for treaty shopping analysis
source(knitr::purl(here("analysis/identify_conduit_jurisdictions.Rmd"), quiet = TRUE))

# FDI and SPE data
source(knitr::purl(here("data-preparation/fdi_data_assemble.Rmd"), quiet = TRUE))
source(knitr::purl(here("data-preparation/spe_data_preparation.Rmd"), quiet = TRUE))

# MAP profiles
source(here("map_profiles_clean.R"))
source(knitr::purl(here("data-preparation/map_profiles_edit.Rmd"), quiet = TRUE))
source(here("map_stats_clean.R"))


# ICTD tax treaty data enhance
# For reasons of data ownership, the raw files necessary for this step are not included in the public version
#source(here("ictd_tax_treaties_enhance.R"))



#prepare treaty shopping data
source(here("data-preparation/treaty_shopping_data_preparation.R"))


# the analysis, here the order should not matter
source(knitr::purl(here("analysis/action6.Rmd"), quiet = TRUE))
source(knitr::purl(here("analysis/cbcr_implementation.Rmd"), quiet = TRUE))
source(knitr::purl(here("analysis/general_level_analysis.Rmd"), quiet = TRUE))
source(knitr::purl(here("analysis/dispute_resolution.Rmd"), quiet = TRUE))



