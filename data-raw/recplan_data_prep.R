# recplan_data_prep.R

library(ecosscraper)
library(RPostgreSQL)
library(secret)
library(tidyverse)

con <- dbConnect(
  dbDriver("PostgreSQL"),
  dbname = "postgres",
  user = ESC_DB_USR,
  password = get_secret(ESC_DB_PW, key = priv_key, vault),
  host = ESC_DB
)

nmfs <- read_tsv("data-raw/NMFS_recplan_manual.tsv")
nmfs$Date <- lubridate::dmy(nmfs$Date)
rec <- dbSendQuery(con, "SELECT * FROM ecos_recovery_plan_docs") %>% dbFetch()
rec$date <- as.Date(rec$date)
names(nmfs) <- names(rec)
combo <- rbind(rec, nmfs)
tecp <- dbSendQuery(con, "SELECT * FROM tecp_table") %>% dbFetch() %>%
  filter_listed() %>%
  filter_domestic()
tecp$first_listed <- lubridate::mdy(tecp$first_listed)

have <- left_join(combo, tecp, by = c("species" = "scientific_name")) %>%
  distinct(date, title, species, where_listed, .keep_all = TRUE) %>%
  select(-c(3,5,8:9,14:15))
have$elapsed <- as.numeric(have$date - have$first_listed)
have$elapsed_years <- have$elapsed / 365
have$first_listed <- case_when(
  have$species == "Chelonia mydas" ~ as.Date("1978-07-28"),
  TRUE ~ have$first_listed
)
funny_ones <- filter(have, where_listed != "Wherever found")
funny_ones$dups <- duplicated(funny_ones$species)
funny_twos <- filter(funny_ones, dups)$species %>% unique()
dups_spp <- filter(funny_ones, species %in% funny_twos) %>%
  select(-dups)
keepers <- filter(have, !(species %in% funny_twos))

# First export, manually link plan to pop...
# rio::export(dups_spp, "data-raw/multiplace_manual.xlsx")
#... then read back in and bind
mult_man <- rio::import("data-raw/multiplace_manual_upd.xlsx")
upd_have <- rbind(keepers, mult_man)
uniq_comb <- distinct(upd_have, species, where_listed, .keep_all = TRUE)
dup_row <- dplyr::setdiff(upd_have, uniq_comb)
dup_df <- filter(upd_have, species %in% unique(dup_row$species))

get_closest <- function(x) {
  sub <- filter(dup_df, species == x)
  ls_yr <- min(sub$first_listed)
  sub$del <- as.numeric(sub$date - ls_yr)
  sub <- filter(sub, del > 0)
  min <- which(sub$del == min(sub$del))
  return(sub[min, ])
}
deltas <- map(unique(dup_df$species), get_closest)
closest <- bind_rows(deltas) %>%
  select(-del)
fin_have <- rbind(uniq_comb, closest) %>%
  distinct(title, species, where_listed, .keep_all = TRUE) %>%
  filter(!(grepl(title, pattern = "Phyllostegia hispida") &
             species != "Phyllostegia hispida"))

fin_have$elapsed <- as.numeric(fin_have$date - fin_have$first_listed)
fin_have$elapsed_years <- fin_have$elapsed / 365 #approx
final_have <- distinct(fin_have, species, where_listed, .keep_all = TRUE)

# saveRDS(have, "data/have_plans_2018-01-08.rds")
saveRDS(final_have, "data/fin_have_plans_2018-01-08.rds")

fin_have$combo <- str_c(fin_have$species, fin_have$where_listed)
tecp$combo <- str_c(tecp$scientific_name, tecp$where_listed)
miss <- filter(tecp, !(combo %in% fin_have$combo))
miss$list_age <- as.numeric(as.Date("2018-01-08") - miss$first_listed)
miss$list_age_yrs <- miss$list_age / 365 #approx
exempt_spp <- c(
  "Vermivora bachmanii",
  "Stygobromus hayi",
  "Palaemonetes cummingi",
  "Noturus trautmani",
  "Oncorhynchus aguabonita whitei",
  "Ribes echinellum", # two pops, N-FL & SC; reason for exemption unclear
  "Pritchardia aylmer-robinsonii", # cultivated plant, private island
  "Herpailurus (=Felis) yagouaroundi tolteca"
)
miss$eligible <- case_when(
  miss$scientific_name %in% exempt_spp ~ "no_exempt",
  miss$list_age_yrs <= 2.5 ~ "no_too_new",
  TRUE ~ "yes"
)
saveRDS(miss, "data/missing_plans_2018-01-08.rds")

saveRDS(tecp, "data/TECP_2018-01-08.rds")

neg_time <- filter(unique_taxa_have, elapsed < 0)
