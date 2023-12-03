# countries contained in sample


# EY Corporate Tax Guides

tocs <- read_rds(here("data-created/tocs.rds"))
tocs %<>% group_by(iso3c) %>% summarise(n = n()) %>% filter(n == max(n)) %>% pull(iso3c)
write_rds(tocs, here("data-created/ey_all_years_av.rds"))

