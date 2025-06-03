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


## --------------- DATA AGGREGATORS ------------------

damage_aggregate <- function(data, grouper_vars = c("year", "crop_key")){
  data %>%
    group_by(across(all_of(grouper_vars))) %>%
    summarise(
      sum_insured = sum(sum_insured,na.rm = TRUE),
      tot_claims = sum(tot_claims, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(burn_cost = tot_claims / sum_insured) %>%
    mutate(across(where(is.numeric), ~ ifelse(is.nan(.), 0, .)))
}


## --------------- DATA CLEANING ------------------


## --------------- PLOTTERS ------------------


plot_4_ranges <- function(data, cps = NA, col = "darkblue"){
  
  if (is.na(cps)){
    X <- data
    cps <- "all"
  } else {
    X <- data %>% dplyr::filter(crop_key %in% cps)
  }
  
  
  p1 <- X %>%
    ggplot(aes(x = burn_cost)) +
    geom_histogram(bins = 50, fill = col, color = "transparent", alpha=1) + 
    labs(title = "All Data") +
    theme_minimal()
  
  p2 <- X %>%
    drop_na() %>%
    ggplot(aes(x = burn_cost)) +
    geom_histogram(bins = 50, fill = col, color = "transparent", alpha=0.75) + 
    labs(title = "Data with production entries") +
    theme_minimal()
  
  p3 <- X %>%
    filter(burn_cost > 0.0) %>%
    ggplot(aes(x = burn_cost)) +
    geom_histogram(bins = 50, fill = col, color = "transparent", alpha=0.5) + 
    labs(title = "Data with Burn > 0") +
    theme_minimal()
  
  p4 <- X %>%
    filter(burn_cost > 0.1 & burn_cost<0.99) %>%
    ggplot(aes(x = burn_cost)) +
    geom_histogram(bins = 50, fill = col, color = "transparent", alpha=0.25) + 
    labs(title = "Data with Burn between 0.1 and 0.99") +
    theme_minimal()
  
  combined_plot <- (p1 + p2) / (p3 + p4) +
    plot_annotation(
      title = paste("Burn Cost on differnt Ranges for Crops", paste(cps, collapse = ", ")),
      theme = theme(
        plot.title = element_text(
          hjust = 0.5, 
        )
      )
    )
  return(combined_plot)
}


plot_8_years <- function(data, yrs = c(2014, 2015, 2016, 2019, 2020, 2021, 2022, 2023), cps = NA, lw = 0, up = 1.1, col = "darkblue"){
  
  if (is.na(cps)){
    X <- data %>% filter(burn_cost > lw & burn_cost < up)
    cps <- "all"
  } else {
    X <- data %>% dplyr::filter(crop_key %in% cps) %>% filter(burn_cost > lw & burn_cost < up)
  }
  
  mke_plot <- function(i, X, yrs, col){
    p <- X %>%
      dplyr::filter(year == yrs[i]) %>%
      ggplot(aes(x = burn_cost)) +
      geom_histogram(bins = 50, fill = col, color = "transparent", alpha=(1.1- i*0.1)) + 
      labs(title = yrs[i]) +
      theme_minimal()
    return(p)
  }
  
  p_list <- purrr::map(seq_along(yrs), ~mke_plot(.x, X, yrs, col))
  
  combined_plot <- (p_list[[1]] + p_list[[2]]) / (p_list[[3]] + p_list[[4]])/ (p_list[[5]] + p_list[[6]]) / (p_list[[7]]+ p_list[[8]]) +
    plot_annotation(
      title = paste("Burn Cost on differnt years for Crops", paste(cps, collapse = ", ")),
      theme = theme(
        plot.title = element_text(
          hjust = 0.5, 
          
        )
      )
    )
  return(combined_plot)
}



plot_3_aggregations <- function(data, cps = NA, lw = 0, up = 1.1,yrs = c(2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023),  col = "darkblue"){
  
  if (is.na(cps)){
    X <- data  %>% filter(burn_cost > lw & burn_cost < up)%>% dplyr::filter(year %in% yrs)
    cps <- "all"
  } else {
    X <- data %>% dplyr::filter(crop_key %in% cps)  %>% filter(burn_cost > lw & burn_cost < up) %>% dplyr::filter(year %in% yrs)
  }

  p1 <- X %>%
    ggplot(aes(x = burn_cost)) +
    geom_histogram(bins = 50, fill = col, color = "transparent", alpha=1) + 
    labs(title = "Municipality Level") +
    theme_minimal()
  
  p2 <- X %>%
    damage_aggregate(grouper_vars = c("year", "crop_key", "district_name")) %>%
    ggplot(aes(x = burn_cost)) +
    geom_histogram(bins = 50, fill = col, color = "transparent", alpha=0.75) + 
    labs(title = "District Level") +
    theme_minimal()
  
  p3 <- X %>%
    damage_aggregate(grouper_vars = c("year", "crop_key")) %>%
    ggplot(aes(x = burn_cost)) +
    geom_histogram(bins = 50, fill = col, color = "transparent", alpha=0.5) + 
    labs(title = "Country Level") +
    theme_minimal()
  
  combined_plot <- (p1) / (p2) / (p3 ) +
    plot_annotation(
      title = paste("Burn Cost by Aggregation level", paste(cps, collapse = ", ")),
      theme = theme(
        plot.title = element_text(
          hjust = 0.5, 
        )
      )
    )
  return(combined_plot)
}

