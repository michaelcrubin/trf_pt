
## --------------- START-UP ------------------


library(here)
source(here("scripts","global.R"))
source(here("scripts","helpers.R"))



## --------------- MUNICIPALITY DF ------------------


# IMPORT nuts list
nut <- read_tsv(here("DB", "raw_data", "nut.csv"), locale = locale(encoding = "latin1"))
colnames(nut) <- c("municipality", "level_1", "level_2", "level_3", "district", "municipality2", "agro_region")
nut <- nut %>% add_mun_key()



# portugal_continental_geography <- RnData::GET_geography("POR", "municipality", return = "SF") %>%
#   dplyr::filter(!(district_name %in% c("Madeira", "Azores"))) %>%
#   dplyr::mutate(
#     mun_key = unfreak_port_letters(municipality_name)) %>%
#   dplyr::left_join(nut, by = join_by(mun_key)) %>%
#   mutate(agro_region = toupper(agro_region)) %>%
#   dplyr::select(municipality_id, municipality_name, district_id, district_name, level_1, level_2, level_3, agro_region, mun_key)

saveRDS(portugal_continental_geography, here("DB", "meta", "portugal_continental_geography.rds"))

## --------------- PRODUCAO ------------------

prod <- read.csv2(here("DB", "raw_data", "producao.csv"))
colnames(prod) <- c("year", "crop", "agro_region2", "municipality2", "sum_insured", "gros_premium", "subsidy")
# X %>% dplyr::select(municipality_name, level_1, level_2, level_3, district, agro_region) %>% saveRDS(here("DB", "meta", "production.rds"))
prod <- prod %>% add_mun_key()

portugal_continental_geography <- readRDS(here("DB", "meta", "portugal_continental_geography.rds"))


production <- prod %>% 
  dplyr::left_join(portugal_continental_geography, by = join_by(mun_key)) %>%
  mutate(crop_key = unfreak_port_letters(crop))%>%
  dplyr::select(municipality_id, year, crop_key, sum_insured, gros_premium, subsidy) %>%
  mutate(year = as.integer(gsub("\\D", "", year)), sum_insured = as.numeric(sum_insured), gros_premium = as.numeric(gros_premium),subsidy = as.numeric(subsidy))


saveRDS(production, here("DB", "input", "portugal_production.rds"))


## --------------- DAMAGE ------------------

damage <- read.csv2(here("DB", "raw_data", "sinistros.csv"))
colnames(damage)  <- c("year", "crop", "agro_region2", "municipality2", "claims_paid", "recoveries_claims", "loss_adj_expenses", "recovery_expenses")
damage <- damage %>% add_mun_key()

portugal_continental_geography <- readRDS(here("DB", "meta", "portugal_continental_geography.rds"))


portugal_damage <- damage %>% 
  dplyr::left_join(portugal_continental_geography, by = join_by(mun_key)) %>%
  mutate(crop_key = unfreak_port_letters(crop))%>%
  dplyr::select(municipality_id, year, crop_key, claims_paid, recoveries_claims, loss_adj_expenses, recovery_expenses) %>%
  mutate(year = as.integer(gsub("\\D", "", year)), claims_paid = as.numeric(claims_paid),recoveries_claims = as.numeric(recoveries_claims),loss_adj_expenses = as.numeric(loss_adj_expenses), recovery_expenses = as.numeric(recovery_expenses))

saveRDS(portugal_damage, here("DB", "input", "portugal_damage.rds"))

## --------------- ALL ------------------

all <- read.csv2(here("DB", "raw_data", "all.csv"))
colnames(all)  <- c("year", "crop", "crop2","agro_region2","municipality2", "Distrito","Nut2","Nut3", "tsi", "premium", "subsidy2")
all <- all %>% add_mun_key()
all$year%>%unique()
portugal_all$year%>%unique()
portugal_continental_geography <- readRDS(here("DB", "meta", "portugal_continental_geography.rds"))


portugal_all<- all %>% 
  dplyr::left_join(portugal_continental_geography, by = join_by(mun_key)) %>%
  mutate(crop_key = unfreak_port_letters(crop))%>%
  dplyr::select(municipality_id, year, crop_key, crop2, tsi, premium, subsidy2) %>%
  mutate(year = as.integer(gsub("\\D", "", year)), tsi = as.numeric(tsi),premium = as.numeric(premium),subsidy2 = as.numeric(subsidy2),)

saveRDS(portugal_all, here("DB", "input", "portugal_all.rds"))


## --------------- CLEAN CROPS ------------------


damage <- readRDS( here("DB", "input", "portugal_damage.rds")) %>% 
  select(crop) %>% mutate(crop_key = unfreak_port_letters(crop)) %>% dplyr::distinct() %>%
  tidyr::nest(variants_damage = crop)

prod <- readRDS( here("DB", "input", "portugal_production.rds")) %>% 
  select(crop)%>%
  mutate(crop_key = unfreak_port_letters(crop)) %>% dplyr::distinct() %>%
  tidyr::nest(variants_prod = crop)

all <- readRDS( here("DB", "input", "portugal_all.rds")) %>% select(crop)%>%
  mutate(crop_key = unfreak_port_letters(crop)) %>% dplyr::distinct() %>%
  tidyr::nest(variants_all = crop)


crop_list <- prod %>% full_join(damage) %>% full_join(all)

saveRDS(crop_list, here("DB", "meta", "crop_list.rds"))


A <- crop_list %>% 
  dplyr::select(crop_key, variants_prod)%>%
  unnest(cols = c(variants_prod))


