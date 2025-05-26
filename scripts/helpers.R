### Functions which help to do stuff
# only none specific functions



## --------------- DATA CLEANING ------------------

unfreak_port_letters <- function(x) {
  x %>%
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%
    tolower() %>%
    gsub("[^a-z]", "", .)
}

add_mun_key <- function(X){
  X %>%
    mutate(
      municipality2 = case_when(municipality2 == "Freixo Espada à Cinta" ~"Freixo de Espada à Cinta",
                                municipality2 == "Freixo Espada A Cinta" ~"Freixo de Espada à Cinta",
                                municipality2 == "Lagoa (Algarve)" ~ "Lagoa",
                                TRUE ~ municipality2),
      mun_key = unfreak_port_letters(municipality2))
  
}


## --------------- DATA IMPORT ------------------

get_geo <- function(){
  readRDS(here("DB", "meta", "portugal_continental_geography.rds"))
}

get_production <- function(){
  readRDS(here("DB", "input", "portugal_production.rds"))
}

get_damage <- function(){
  readRDS(here("DB", "input", "portugal_damage.rds")) %>%
    group_by(crop_key, year, municipality_id) %>% 
    summarise(
      claims_paid = sum(claims_paid),
      recoveries_claims = sum(recoveries_claims),
      loss_adj_expenses = sum(loss_adj_expenses),
      recovery_expenses = sum(recovery_expenses),
      .groups = "drop"
    )
}

get_all <- function(){
  readRDS(here("DB", "input", "portugal_all.rds"))
}

get_crop <- function(){
  readRDS(here("DB", "meta", "crop_list.rds"))
}


get_damage_grid <- function(){
  
  geo <- get_geo() %>% sf::st_drop_geometry()
  crop <- get_crop()
  year <- c(2014:2023)
  X <- tidyr::expand_grid(municipality_id = geo$municipality_id, crop_key = crop$crop_key, year = year)
  
  (length(unique(X$year)) * length(unique(X$crop_key)) * length(unique(X$municipality_id)))
  
  Y <- get_damage() %>% 
    group_by(municipality_id, crop_key, year)%>%
    summarise(n_data = n(), .groups = "drop")%>%
    mutate(
      has_data = 1
      ) %>%
    dplyr::select(municipality_id, crop_key, year, has_data, n_data)
  
  damage <- X %>% left_join(Y,  by = join_by(municipality_id, crop_key, year)) %>%
    replace_na(list(has_data=0, n_data=0)) %>%
    left_join(geo, by = join_by(municipality_id))%>%
    dplyr::select(crop_key, year, municipality_id, has_data, n_data, municipality_name, district_name, agro_region)
  
}


get_cell_grid <- function(){
  
  geo <- get_geo() %>% sf::st_drop_geometry()
  crop <- get_crop()
  X <- tidyr::expand_grid(municipality_id = geo$municipality_id, crop_key = crop$crop_key)
  
  (length(unique(X$year)) * length(unique(X$crop_key)) * length(unique(X$municipality_id)))
  
  Y <- get_damage() %>% 
    group_by(municipality_id, crop_key)%>%
    summarise(n_damage_data = n(), .groups = "drop")%>%
    mutate(
    ) %>%
    dplyr::select(municipality_id, crop_key, n_damage_data)
  
  Z <- get_production() %>% 
    group_by(municipality_id, crop_key)%>%
    summarise(n_insured_data = n(), .groups = "drop")%>%
    mutate(
    ) %>%
    dplyr::select(municipality_id, crop_key, n_insured_data) 
  X %>% 
    left_join(Y,  by = join_by(municipality_id, crop_key)) %>%
    left_join(Z, by = join_by(municipality_id, crop_key)) %>%
    left_join(geo, by = join_by(municipality_id)) %>%
    replace_na(list(n_insured_data = 0, n_damage_data=0)) %>%
    dplyr::select(crop_key, municipality_id, n_insured_data, n_damage_data, municipality_name, district_name, agro_region)
  
}



get_burn <- function(){
  
  damage <- get_damage()
  prod <- get_production()
  
  #X <-get_damage_grid()
  X <- prod %>% 
    # left_join(prod, by = join_by(crop_key, year, municipality_id)) %>%
    left_join(damage, by = join_by(crop_key, year, municipality_id))

  
  
  Y <- X %>% 
    replace_na(list(claims_paid = 0, loss_adj_expenses = 0, recoveries_claims = 0, recovery_expenses = 0)) %>%
    #group_by(municipality_id, crop_key) %>%
    # summarise(
    #   n_year = n(),
    #   sum_insured = sum(sum_insured),
    #   gros_premium = sum(gros_premium, na.rm = TRUE),
    #   subsidy = sum(subsidy, na.rm = TRUE),
    #   claims_paid = sum(claims_paid, na.rm = TRUE),
    #   recoveries_claims = sum(recoveries_claims, na.rm = TRUE),
    #   loss_adj_expenses = sum(loss_adj_expenses, na.rm = TRUE),
    #   recovery_expenses = sum(recovery_expenses, na.rm = TRUE),
    #   recoveries_claims = sum(recoveries_claims, na.rm = TRUE),
    #   .groups = "drop") %>%
    mutate(
      tot_claims = claims_paid - recoveries_claims + loss_adj_expenses - recovery_expenses,
      burn_cost = pmin(1, tot_claims / sum_insured)
      ) %>%
    dplyr::select(year, municipality_id, crop_key, sum_insured, tot_claims, burn_cost)
  return(Y)
}


## --------------- DATA CLEANING ------------------

