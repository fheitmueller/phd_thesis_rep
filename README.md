# README

This folder contains relevant material to reproduce figures and tables contained in Frederik Heitmüller's PhD dissertation. Everything is written in the R language. However, to run the pdf extraction procedures, Python needs to be installed as well.

The following version of R has been used: 4.2.3, as well as the following R packages with their respective versions:
1              OECD      0.2.5  
2               WDI      2.7.8  
3               XML  3.99-0.14  
4  archiveRetriever      0.3.1  
5             bea.R      1.0.6  
6             beepr        1.3  
7       countrycode      1.4.0  
8          devtools      2.4.5  
9             dplyr      1.1.2  
10         eurostat      3.8.2  
11      facetscales 0.1.0.9000  
12          forcats      1.0.0  
13        ggpattern      1.0.1  
14          ggplot2      3.4.2  
15           ggpubr      0.6.0  
16         gluedown      1.0.6   
17            haven      2.5.2  
18             here      1.0.1    
19             httr      1.4.5  
20       kableExtra      1.3.4  
21            knitr       1.42  
22        lubridate      1.9.2  
23         magrittr      2.0.3  
24     modelsummary      1.4.0  
25        patchwork      1.1.2  
26         pdftools      3.3.3  
27            purrr      1.0.1  
28             qpdf      1.3.2  
29            readr      2.1.4  
30           readxl      1.4.2  
31       reticulate       1.28  
32        rmarkdown       2.21  
33            rvest      1.0.3  
34           scales      1.2.1  
35          stringr      1.5.0  
36           tibble      3.2.1  
37            tidyr      1.3.0  
38        tidyverse      2.0.0  


This is what the different sub-folders are there for:

- fig: contains the figures and tables
- analysis: contains R Markdown scripts for producing the figures and tables, with some additional explanation included in the folders
- data-preparation: contains scripts to turn raw data into the data that is used for producing the figures and tables
- data-raw: contains raw datasets (including pdfs) downloaded from various sources on the web, which are used in the analysis. See below for a list with the links. 
- data-created: contains transformed versions of the datasets, including a few that were collected by hand
- R: contains a number of custom functions used in the various steps of the analysis
- man and cache: automatic folders used by R

The makefile contains the order in which files need to be produced to produce the different intermediary datasets and outputs.
.Rmd analysis files contain additional explanations.

Online sources of "raw" datasets:
* action_6_peer_review: https://www.oecd.org/tax/beps/prevention-of-tax-treaty-abuse-fourth-peer-review-report-on-treaty-shopping-3dc05e6a-en.htm 
* cbcr_peer_review: https://www.oecd.org/tax/beps/country-by-country-reporting-compilation-of-2022-peer-review-reports-5ea2ba65-en.htm
* cbcr_reporting_requirements: https://qdd.oecd.org/subject.aspx?Subject=CBCR_REQ
* corporate tax guides: https://www.ey.com/en_gl/tax-guides/tax-guide-library-archive
* dist_cepii: http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=6
* exchangerates: https://data.oecd.org/conversion/exchange-rates.htm
* fdi_data/C2Flujo_Paises_2007: https://www.banrep.gov.co/sites/default/files/paginas/C2Flujo_Paises_2007.xlsx
* fdi_data/C4PAISES_2007-ICE.csv: https://web.archive.org/web/20210412181621/https://www.banrep.gov.co/sites/default/files/paginas/C4PAISES_2007-ICE.xlsx
* fdi_data/CDIS: https://data.imf.org/regular.aspx?key=61227424
* fdi_data/consulta_datainvex.xls: http://datainvex.comercio.es/principal_invex.aspx 
* fdi_data/eurostat_spe_fdi: 
* fdi_data/oecd_fdi_general: https://data.oecd.org/fdi/fdi-stocks.htm
* fdi_data/oecd_spe_fdi: https://stats.oecd.org/Index.aspx?DataSetCode=FDI_POS_CTRY
* fdi_data/US_FdiFlowsStock_ST202110071148_v1: https://unctadstat.unctad.org/7zip/US_FdiFlowsStock.csv.7z
* fdi_data/value-of-investment-2012_2021i.xlsx: https://www.fscmauritius.org/en/statistics/statistics/global-business
* map_best_practices: https://www.oecd.org/tax/beps/beps-actions/action14/
* map_peer_review: https://www.oecd.org/tax/beps/beps-actions/action14/
* map_profiles: https://www.oecd.org/tax/dispute/country-map-profiles.htm
* map_statistics: https://www.oecd.org/tax/dispute/mutual-agreement-procedure-statistics.htm
* statutory_rates/1980-2021-Corporate-Tax-Rates-Around-the-World: https://taxfoundation.org/corporate-tax-rates-by-country-2021/
* eu_statutory_rates: https://ec.europa.eu/taxation_customs/document/download/6e2a5960-f13c-4aef-b115-60e3ba232966_en?filename=statutory_rates_2022.xlsx 
* IRPJ_Alicuotas_Maximas: https://ciatorg.sharepoint.com/:x:/s/cds/EfKIVrV_LDxPmvR0ApI7gSgB-Vnbpq6HZ85ChNbkQfOF8g?rtime=zXQyOOGN20g
* kpmg_corporate_tax_rates: https://home.kpmg/xx/en/home/services/tax/tax-tools-and-resources/tax-rates-online/corporate-tax-rates-table.html
* michigan: https://www.bus.umich.edu/otpr/otpr/default.asp
* oecd_rates: https://stats.oecd.org/index.aspx?DataSetCode=Table_II1
* included_countries_ictd: https://www.treaties.tax/en/faq/
* kaopen_2019: https://web.pdx.edu/~ito/Chinn-Ito_website.htm
* oecd_compare_action_14: https://www.compareyourcountry.org/tax-cooperation/en/2//category/
* taxSource: https://www.treaties.tax/en/data/ (on request)
* treaties: IBFD Tax research platform (search results)
* un_pop: https://data.un.org/_Docs/SYB/CSV/SYB65_1_202209_Population,%20Surface%20Area%20and%20Density.csv
* wdi: World Bank (from WDI package by Vincent Arel-Bundock)

