
## --------------- START-UP ------------------


library(here)
source(here("scripts","global.R"))
source(here("scripts","helpers.R"))
library(here)
invisible(capture.output(source(here("scripts", "global.R"))))
invisible(capture.output(source(here("scripts", "helpers.R"))))
invisible(capture.output(source(here("scripts", "bayes.R"))))

data <- get_burn()
x <- get_damage_grid()
data <- x %>% left_join(data) %>% replace_na(list(burn_cost = 0))


p1 <- data %>%
  ggplot(aes(x = burn_cost)) +
  geom_histogram(bins = 50, fill = "darkblue", color = "transparent", alpha=1) + 
  labs(title = "All 258'590 Muni x Crop Cells") +
  theme_minimal()



p2 <- data %>%
   drop_na() %>%
  #filter(has_data > 0) %>%
  ggplot(aes(x = burn_cost)) +
  geom_histogram(bins = 50, fill = "darkblue", color = "transparent", alpha=0.75) + 
  labs(title = "Muni x Crop with Production (7690)") +
  theme_minimal()


p3 <- data %>%
  filter(burn_cost > 0.0) %>%
  ggplot(aes(x = burn_cost)) +
  geom_histogram(bins = 50, fill = "darkblue", color = "transparent", alpha=0.5) + 
  labs(title = "All with Burn > 0") +
  theme_minimal()



p4 <- data %>%
  filter(burn_cost > 0.1 & burn_cost<0.99) %>%
  ggplot(aes(x = burn_cost)) +
  geom_histogram(bins = 50, fill = "darkblue", color = "transparent", alpha=0.25) + 
  labs(title = "Burn between 0.1 and 0.99") +
  theme_minimal()

library(ggplot2)
library(patchwork)

combined_plot <- (p1 + p2) / (p3 + p4) +
  plot_annotation(
    title = "Burn Cost on differnt Ranges",
    theme = theme(
      plot.title = element_text(
        hjust = 0.5, 

      )
    )
  )

# show it
combined_plot




theme_minimal(base_size = 12)
  pull(burn_cost)%>%
    
    
  hist(col = "purple",main = "between 0.1 and 0.99", breaks = 100) + 



ad <- data %>%
  group_by(municipality_id, crop_key) %>%
  summarise(n = n())


library(dplyr)
library(ggplot2)

data %>%
  filter(municipality_id %in% c("PRT.20.5_1")) %>%
  filter(burn_cost > 0.0, burn_cost < 1.99) %>%
  ggplot(aes(x = burn_cost)) +
  geom_histogram(bins = 50, fill = "darkblue", color = "transparent", alpha=0.5) +
  facet_wrap(~ year, scales = "free_y") +
  labs(title = "Burn cost per year (filtered 0.1 – 0.99)",
       x = "Burn cost", y = "Frequency") +
  theme_minimal(base_size = 12)


a<-data %>%
  filter(municipality_id %in% c("PRT.20.5_1")) %>%
  mutate(ab = burn_cost >0.99) %>%
  group_by(ab) %>%
  summarise(n = n(), ab = sum(ab)) %>% pull(n)
a[2]/a[1]
data %>%
  filter(municipality_id %in% c("PRT.20.5_1")) %>%
  filter(burn_cost > -10.0, burn_cost < 1.99) %>%
  ggplot(aes(x = burn_cost)) +
  geom_histogram(bins = 20, fill = "darkblue", color = "transparent", alpha=0.5) +
  labs(title = "Burn cost per year (filtered 0.1 – 0.99)",
       x = "Burn cost", y = "Frequency") +
  theme_minimal(base_size = 12)



library(dplyr)
library(ggplot2)

data %>%
  filter(burn_cost > 0.1, burn_cost < 1.99) %>%
  ggplot(aes(x = burn_cost)) +
  geom_histogram(bins = 50, fill = "purple", color = "black") +
  facet_wrap(~ year, ncol = 1, scales = "free_y") +
  labs(title = "Burn cost per year (filtered 0.1 – 0.99)",
       x = "Burn cost", y = "Frequency") +
  theme_minimal(base_size = 12)

## --------------- INSPECT DATA ------------------
# 
# 
# 
# 
# damage_grid <- get_damage_grid()
# 
# length(unique(damage_grid$crop_key))
# 
# 
# 
# damage_grid %>%
#   group_by(crop_key) %>%
#   summarise(n = sum(n_data), .groups = "drop") %>%
#   arrange(desc(n)) %>%
#   mutate(crop_key = factor(crop_key, levels = crop_key)) %>%
#   ggplot( aes(x = crop_key, y = n)) +
#   geom_col(fill = "darkblue", alpha = 0.5) +
#   labs(title =  element_blank(), x = "Crop", y = "Count") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# damage_grid %>%
#   group_by(year) %>%
#   summarise(n = sum(n_data), .groups = "drop") %>%
#   arrange(desc(year)) %>%
#   mutate(year = factor(year, levels = year)) %>%
#   ggplot( aes(x = year, y = n)) +
#   geom_col(fill = "darkred",  alpha = 0.5) +
#   labs(title = element_blank(), x = "Year", y = "Count") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# 
# 
# 
# 
# 
# 
# X <- damage_grid %>%
#   group_by(municipality_id) %>%
#   summarise(n_data = sum(n_data), .groups = "drop")
# # 
# damage_geo <- get_geo() %>% left_join(X, by = join_by(municipality_id)) %>%
#   replace_na(list(n_data=0))
# 
# 
# damage_geo %>% 
#   sf::st_drop_geometry()%>%
#   arrange(desc(n_data))%>%
#   mutate(municipality_id = factor(municipality_id, levels = municipality_id)) %>%
#   ggplot( aes(x = municipality_id, y = n_data)) +
#   geom_col(fill = "darkgreen",  alpha = 0.8) +
#   labs(title = "Top Years by Count", x = "Year", y = "Count") +
#   theme_minimal() +
#   theme(axis.text.x =  element_blank())
# 
# 
# 
# damage_geo %>%
#   sf::st_drop_geometry()%>%
#   group_by(agro_region) %>%
#   summarise(n_data = n(), .groups = "drop") %>%
#   arrange(desc(n_data))%>%
#   mutate(agro_region = factor(agro_region, levels = agro_region)) %>%
#   ggplot( aes(x = agro_region, y = n_data)) +
#   geom_col(fill = "darkgreen",  alpha = 0.3) +
#   labs(title = "Top Years by Count", x = "Agro Region", y = "Count") +
#   theme_minimal()

# 
# 
# 
# damage_geo %>%
#   dplyr::select(n_data) %>%
#   ggplot() +
#   geom_sf(aes(fill = n_data), color = "lightgray", size = 0.1) +
#   scale_fill_gradient(low = "white", high = "darkgreen") +
#   theme_minimal() +
#   labs(title = "Spatial Frequency Distribution", fill = "Freq")
# library(sf)
# library(dplyr)
# library(ggplot2)
# 
# library(plotly)
# 
# 
# damage_plot <- damage_grid %>%
#   group_by(agro_region, crop_key, year) %>%
#   summarise(size = sum(n_data), .groups = "drop") %>%
#   mutate(
#     geo = as.integer(as.factor(agro_region)),
#     crop = as.integer(as.factor(crop_key)),
#     year = as.integer(year)  # assuming year is already numeric
#   ) 
# 
# plot_ly(
#   data = damage_plot,
#   x = ~agro_region,
#   y = ~year,
#   z = ~crop_key,
#   type = 'scatter3d',
#   mode = 'markers',
#  #color = ~crop_key,  # Optional: color by crop
#   color = "red",
#   marker = list(size = ~size, opacity = 0.3),
#   showlegend = TRUE
# ) %>%
#   layout(
#     title = "Information Space",
#     scene = list(
#       xaxis = list(title = "Geography"),
#       yaxis = list(title = "Year"),
#       zaxis = list(title = "Crop")
#     )
#   )
# p


## --------------- BAYESIAN MODEL ------------------

# ─────────────────────────────────────────────────────────────┐
#   Toy example ─ staged BHM  (district → municipality)       │
#   paste into fresh R session                                 │
# ─────────────────────────────────────────────────────────────┘
library(tidyverse)
library(brms)
options(mc.cores = 2)     # keep it light

# ------------------------------------------------------------------
# 0.  MAKE SYNTHETIC YEAR-LEVEL DATA
# ------------------------------------------------------------------
set.seed(1)

crops      <- c("maca","pera","milho")
districts  <- paste0("D", 1:4)
munis      <- map(districts, ~ paste0(.x, "_M", 1:5)) |> unlist()
years      <- 2016:2020

# helper: true (unknown) rates
crop_rate   <- c(maca = 0.01, pera = 0.015, milho = 0.005)
dist_uplift <- rnorm(length(districts), 0, 0.3); names(dist_uplift) <- districts
muni_uplift <- rnorm(length(munis),    0, 0.4); names(muni_uplift)  <- munis

# build data frame
raw <- expand_grid(crop = crops, muni = munis, year = years) %>%
  mutate(
    district = substr(muni, 1, 2),
    sum_insured = runif(n(), 2e4, 1e5),
    true_rate   = crop_rate[crop] *
      exp(dist_uplift[district] + muni_uplift[muni]),
    # hurdle coin
    has_claim   = rbinom(n(), 1, pmin(true_rate*3, 0.4)),
    # Gamma claim size when it happens
    tot_claims  = if_else(has_claim == 1,
                          rgamma(n(), shape = 2, scale = true_rate*sum_insured/2),
                          0)
  )

yearly <- raw %>%
  mutate(offset_expo = log(sum_insured),
         crop  = factor(crop),
         muni  = factor(muni),
         district = factor(district))

cat("Synthetic rows =", nrow(yearly), "\n")

# ------------------------------------------------------------------
# 1.  STAGE-1  district model  (fast fit for demo)
# ------------------------------------------------------------------
fit_dist <- brm(
  bf(
    tot_claims ~ 1 + (1 | crop) + (1 | district) + offset(offset_expo),
    hu         ~ 1 + (1 | crop) + (1 | district)
  ),
  data   = yearly,
  family = hurdle_gamma(link = "log"),
  chains = 2, iter = 1000, seed = 1,
  control = list(adapt_delta = 0.9)
)

# posterior mean & sd of district intercepts
dist_post <- ranef(fit_dist)$district[, , "Intercept"]
dist_hu   <- ranef(fit_dist)$district[, , "Intercept", dpar = "hu"]

# ------------------------------------------------------------------
# 2.  BUILD priors for each municipality from its district post.
# ------------------------------------------------------------------
make_prior_lines <- function(re_table, dpar = "") {
  map2(
    rownames(re_table),     # districts
    seq_len(nrow(re_table)),
    ~ prior(normal(re_table[.y, "Estimate"], re_table[.y, "Est.Error"]),
            class = "b", coef = paste0(.x, "_M", 1:5), dpar = dpar, check = FALSE)
  ) |> unlist(recursive = FALSE)
}

prior_muni <- c(make_prior_lines(dist_post),
                make_prior_lines(dist_hu, dpar = "hu"))

# ------------------------------------------------------------------
# 3.  STAGE-2  municipality model (fast fit)
# ------------------------------------------------------------------
fit_muni <- brm(
  bf(
    tot_claims ~ 1 + (1 | crop) + (1 | muni) + offset(offset_expo),
    hu         ~ 1 + (1 | crop) + (1 | muni)
  ),
  data   = yearly,
  family = hurdle_gamma(link = "log"),
  prior  = prior_muni,
  chains = 2, iter = 1000, seed = 2,
  control = list(adapt_delta = 0.9)
)

# ------------------------------------------------------------------
# 4.  GRID & PREDICT  (exposure = €1 to get burn-rate)
# ------------------------------------------------------------------
grid <- expand_grid(crop = crops, muni = munis) %>%
  mutate(sum_insured = 1, offset_expo = 0,
         crop = factor(crop, levels = levels(yearly$crop)),
         muni = factor(muni, levels = levels(yearly$muni)))

draws <- posterior_predict(fit_muni, newdata = grid, allow_new_levels = TRUE)
rate_mean <- colMeans(draws)          # already €/€

tariff <- grid %>%
  mutate(rate = rate_mean,
         dist = substr(muni, 1, 2)) %>%
  slice_sample(n = 10)

print(tariff)

























ridgeline_plot <- function(X, crop = "maca"){
  browser()
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(ggridges)   # install.packages("ggridges")
  crop = "cereja"
  X <- readRDS(here("draw_df.rds"))

  
  Y <- X %>%
    dplyr::filter(crop_key == crop) %>%
    slice_max(n_damage_data, n = 5) %>%
    slice_sample(n = 5) %>%
    mutate(label = paste("max_data",  municipality_id, sep = "_")) %>%
    select(label, draws) %>%
    unnest_longer(draws, values_to = "rate")
  
  Z <- X %>%
    dplyr::filter(crop_key == crop) %>%
    slice_min(n_damage_data, n = 5) %>%
    slice_sample(n = 5) %>%
    mutate(label = paste("min_data",  municipality_id, sep = "_")) %>%
    select(label, draws) %>%
    unnest_longer(draws, values_to = "rate")

  Y$rate %>%hist()
  Z$rate %>%hist()
  plot_df <- rbind(maxs, mins)
  plot_df %>% filter(label == "max_data_PRT.6.3_1") %>%pull() %>% hist()
  plot_df %>% filter(label == "min_data_PRT.17.2_1") %>%pull()# %>% hist()
  
  ggplot(plot_df, aes(rate, label, fill = label)) +
    geom_density_ridges(alpha = 0.7, colour = NA, scale = 1.2) +
    coord_cartesian(xlim = c(0, 0.3)) +                 # visual clip
    scale_x_continuous(labels = scales::percent) +
    labs(x = "Burn-rate", y = NULL) +
    theme_minimal() + theme(legend.position = "none")
  p
  
  return(p)
}


RUN_bayes_hierarchical_model2 <- function(data, geo_group = "municipality_id", crop_group = "crop_key", retrain = TRUE,  ...){
  # 
  # browser()
  # 
  # grouper <- c("crop_key", "municipality_id")
  # data <- burn %>% drop_na()
  # geo_group = "municipality_id"
  # crop_group = "crop_key"
  
  X <- data %>%
    dplyr::mutate(
      geo = .data[[geo_group]],
      crop = .data[[crop_group]]
    ) %>%
    drop_na() %>%
    group_by(geo, crop) %>%
    
    #group_by(across(any_of(c(geo_group)))) %>%
    summarise(
      n_year = n(),
      sum_insured = sum(sum_insured),
      tot_claims = sum(tot_claims),
      y = pmin(1, tot_claims / sum_insured),
      .groups = "drop")
  
  Y <- X %>%
    mutate(
      geo = factor(geo),
      crop = factor(crop))
  
  # 2. Fit Bayesian hierarchical model ------------------------------------
  # zero inflated gamma as colonical link, random intercepts for muni, crop
  if (isTRUE(retrain)){
    print(paste("TRAIN MODEL ON",geo_group,crop_group))
    fit <- brm(
      y ~ 1
      + (1 | geo)
      + (1 | crop),
      data   = Y,
      family = hurdle_gamma(link = "log"),
      chains = 4,
      iter = 8000,
      cores = 6,
      seed = 123,
      control = list(adapt_delta = 0.95)
    )
    
    saveRDS(fit, here("DB", "model",paste0("bhm_", geo_group, "_", crop_group, ".rds")))
    
  } else {
    print(paste("LOAD MODEL",geo_group,crop_group))
    fit <- readRDS(here("DB", "model",paste0("bhm_", geo_group, "_", crop_group, ".rds")))
  }
  
  print(paste("R2 SCORE", bayes_R2(fit)[1,1] ))
  
  pred <- tidyr::expand_grid(
    geo = unique(X$geo),
    crop = unique(X$crop)) %>%
    mutate(
      geo = factor(geo, levels = levels(Y$geo)),
      crop = factor(crop, levels = levels(Y$crop))#,
    )
  
  print(paste("PREDICT"))
  browser()
  
  
  pred_summary <- predict(fit, newdata = pred, allow_new_levels = FALSE, summary = TRUE) %>%
    as.data.frame() %>%
    rename(
      y_hat = Estimate,
      lower = Q2.5,
      upper = Q97.5,
      se = Est.Error
    )
  
  pred <- bind_cols(pred, pred_summary)
  
  
  pred <- pred %>%
    mutate(
      width = upper - lower,         # Width of 95% interval
      rel_width = (upper - lower) / y_hat,  # Relative width
      flag_uncertain = rel_width > 1  # Flag if interval is wider than the mean
    )
  
  #pred$y_hat <- predict(fit, newdata = pred, allow_new_levels = F, summary = TRUE)[, "Estimate"]
  
  suffix <- paste(geo_group, crop_group, sep = "-")
  
  res <- pred %>%
    mutate(
      geo = as.character(geo),
      crop = as.character(crop)
    ) %>%
    left_join(X) %>%
    rename(
      {{ geo_group }} := geo,
      {{ crop_group }} := crop
    ) %>%
    rename_with(
      ~ paste0(., "-", suffix),
      .cols = -c(all_of(geo_group), all_of(crop_group))
    )
  return(res)
  
}



TRAIN_bayes_hierarchical_model <- function(){
  
  # ------------------------------------------------------------------
  # 2.  Fit hurdle-Gamma with crop + muni effects  (no year in formula)
  # ------------------------------------------------------------------
  
  if (isTRUE(retrain)){
    fit <- brm(
      bf(tot_claims ~ 1 + (1 | crop) + (1 | muni) + offset(log(sum_insured)),
         hu           ~ 1 + (1 | crop) + (1 | muni) ),
      data   = yearly,
      family = hurdle_gamma(link = "log"),
      chains = 4, iter = 4000, cores = 10,
      control = list(adapt_delta = 0.95)
    )
    saveRDS(fit, here("fit.rds"))
    
  } else {
    
    fit <- readRDS(here("fit.rds"))
  }
  
  
  
  
}


PREDICT_bayes_hierarchical_model <- function(){
  
  grid <- get_cell_grid() %>%
    mutate(muni = municipality_id, crop = crop_key) %>%
    left_join(
      yearly %>%
        group_by(crop, muni) %>%
        summarise(sum_insured_compare = sum(sum_insured), .groups = "drop"),
      by = c("crop", "muni")
    ) %>%
    mutate(
      # imputed_expo   = is.na(sum_insured),
      sum_insured    = 1,
      offset_expo    = log(1),
      crop = levels(crop),
      muni = levels(muni)
    )
  
  # ------------------------------------------------------------------
  # 4.  Posterior predictive draw → burn-rate
  # ------------------------------------------------------------------
  
  
  # one draw matrix: rows = grid rows, cols = posterior draws
  draws <- posterior_predict(fit, newdata = grid, allow_new_levels = TRUE)
  draw_df <- grid %>%
    mutate(draws = asplit(draws, 2) )
  
  saveRDS(draw_df, here("draws_df.rds"))
  draw_df <- readRDS(here("draws_df.rds"))
  
}



ANALYZE_probability <- function(){
  
  calc_stats <- function(vec, threshold = 0.3){
    vec <- unlist(vec)
    
    list(
      n = length(vec),
      mean = round(mean(vec), 4),
      sd = round(sd(vec), 4),
      q95 = round(quantile(vec, 0.95), 4),
      p_tail = round(mean(vec > threshold), 4),
      p_zero = round(mean(vec == 0), 4),
      rw95  = (quantile(vec,.975) - quantile(vec,.025))/mean(vec),
      esd   = mean(vec[vec > quantile(vec,.95)]) - quantile(vec,.95)
    )
  }
  
  RES <- draw_df %>%
    mutate(purrr::map_dfr(draws, ~calc_stats(.x))
    ) %>% select(-draws)
  saveRDS(RES, here("resstat.rds"))
  
  
}

RUN_bayes_hierarchical_model <- function(data, geo_group = "municipality_id", crop_group = "crop_key", retrain = TRUE,  ...){

  data <- get_burn()
  # grouper <- c("crop_key", "municipality_id")
  # data <- burn %>% drop_na()
  geo_group = "municipality_id"
  crop_group = "crop_key"
  retrain = F

  
   # ------------------------------------------------------------------
   # 1.  Prepare year-level data  (one row per crop × muni × year)
   # ------------------------------------------------------------------
   yearly <- data %>%                       # <- your original table
     filter(sum_insured > 0) %>%                # remove impossible exposures
     mutate(
       crop = factor(crop_key),
       muni = factor(municipality_id),
       offset_expo = log(sum_insured)           # offset column
     )
   

  
  saveRDS(draw_df, here("draw_df.rds"))
  
   p <- ridgeline_plot(draw_df, crop = "maca")


   
   draw_df %>% slice_max(n_damage_data) %>% slice_sample(n=1) %>%pull(draws) %>% unlist() %>%round(2)%>%hist(nclass=100, main = "many Data")
   draw_df %>% filter(n_damage_data == 0 & n_insured_data == 0) %>% slice_sample(n=1) %>%pull(draws) %>% unlist() %>%hist(nclass=100, main = "no Data")
   
   
  
   
   
   X %>% 
     ggplot( aes(x = mean, y = sum_insured)) +
     geom_point(alpha = 0.3, color = "darkblue", size = 2, stroke = 0, shape = 16)+
     geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", linewidth = 1.5) +
     scale_x_log10() + scale_y_log10() +  # log scale for better spread
     labs(
       title = paste("sum insured vs mean risk ")
     ) +
     theme_minimal()
   
   
   library(dplyr)
   library(GGally)
   
   # pick the numeric columns you care about
   vars <- c("sum_insured", "n_damage_data", "mean", "sd", "p_zero")
   
   p<-X %>%               # or risk_stats / info_df
     select(all_of(vars)) %>%
     ggpairs(
       progress = FALSE,             # suppress console spam
       lower = list(continuous = wrap("points", alpha = 0.3, size = 0.8)),
       upper = list(continuous = wrap("cor", size = 3)),
       diag  = list(continuous = "densityDiag")
     )
   
   X$mean%>%hist(nclass=1000)
   
   Y <- 

   
   df <- drid
   saveRDS(grid_with_draws, here("draws_df.rds"))
   draws <- readRDS( here("draws.rds"))
   
   draws[,4] %>%hist(nclass=100)
   
  #  
  #  x <- draws %>% as.data.frame() %>% summarise(across(everything(), ~sum(.x > 0)))
  #  hist(as.numeric(a), nclass=100)
  #  
  #  # draws: iterations  ×  observations
  #  #        1000        ×  21 567
  #  rate_draws <- draws / matrix(
  #    grid$sum_insured,
  #    nrow = nrow(draws),          # 1000
  #    ncol = ncol(draws),          # 21 567
  #    byrow = TRUE
  #  )
  #  
  #  # summary across iterations  (i.e. along rows = 1st dimension)
  #  mean_rate <- colMeans(rate_draws)
  #  lower     <- apply(rate_draws, 2, quantile, 0.025)
  #  upper     <- apply(rate_draws, 2, quantile, 0.975)
  #  
  #  summary_df <- tibble(
  #    crop  = grid$crop,
  #    muni  = grid$muni,
  #    mean_rate = mean_rate,       # length 21 567
  #    lower     = lower,
  #    upper     = upper,
  #    rel_width = (upper - lower) / mean_rate,
  #    imputed_expo = grid$imputed_expo
  #  ) %>% left_join(nys) %>% replace_na(list(n_year = 0))
  #  
  #  a <- draws %>% as.data.frame() %>% summarise(across(everything(), ~sum(.x > 0)))
  #  hist(as.numeric(a), nclass=100)
  #  apply(draws, 2, .x>0)
  #  summary_df %>% 
  #    ggplot( aes(x = n_year, y = rel_width)) +
  #    geom_point(alpha = 0.3, color = "darkblue", size = 5, stroke = 0, shape = 16)+
  #    # geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", linewidth = 1.5) +
  #    #  scale_x_log10() + scale_y_log10() +  # log scale for better spread
  #    labs(
  #      title = paste("Uncertainty vs observations ")
  #    ) +
  #    theme_minimal()
  #  
  #  summary_df$upper%>%hist()
  #  
  #  
  #  # ------------------------------------------------------------------
  #  # 5.  Example: view unstable cells (wide intervals or imputed expo)
  #  # ------------------------------------------------------------------
  #  summary_df %>% 
  #    filter(rel_width > 1 | imputed_expo) %>% 
  #    arrange(desc(rel_width)) %>% 
  #    glimpse()
  #  
  #  # 'summary_df' now holds:
  #  # crop, municipality, posterior mean burn-rate, 95 % CI, uncertainty width,
  #  # and a flag whether exposure had to be imputed.
  #  
  #  
  #  
  #  
  #  
  #  
  #  
  #  
  #  
  #  
  #  
  #  
  # browser()
  # data <- Y
  # grid <- data %>%
  #   dplyr::mutate(
  #     geo = .data[[geo_group]],
  #     crop = .data[[crop_group]]
  #     ) %>%
  #   drop_na(sum_insured)
  # 
  # 
  # 
  # X <- grid %>%
  #  # drop_na(tot_claims) #%>%
  #   group_by(geo, crop) %>%
  #   #group_by(across(any_of(c(geo_group)))) %>%
  #   summarise(
  #     n_year = n(),
  #     sum_insured = sum(sum_insured),
  #     tot_claims = sum(tot_claims, na.rm = TRUE),
  #     burn_cost = 
  #     offset_expo = log(sum_insured),
  #     y = tot_claims,
  #     .groups = "drop")
  # 
  # X <- grid %>%
  #   mutate(
  #     offset_expo = log(sum_insured),
  #     y = tot_claims, 
  #     
  #   )
  # Y <- X %>%
  #   mutate(
  #     geo = factor(geo),
  #     crop = factor(crop))
  # a<-Y%>%filter(y>0)#%>%pull(y)%>%hist()
  # a$y%>%hist()
  # # 2. Fit Bayesian hierarchical model ------------------------------------
  # # zero inflated gamma as colonical link, random intercepts for muni, crop
  # if (isTRUE(retrain)){
  #   print(paste("TRAIN MODEL ON",geo_group,crop_group))
  #   fit <- brm(
  #     y ~ 1 + (1 | geo) + (1 | crop) + offset(offset_expo),
  #     data   = Y,
  #     family = hurdle_gamma(link = "log"),
  #     chains = 4,
  #     iter = 4000,
  #     cores = 4,
  #     seed = 123,
  #     control = list(adapt_delta = 0.95)
  #   )
  #   
  #   saveRDS(fit, here("DB", "model",paste0("bhm_", geo_group, "_", crop_group, ".rds")))
  #   
  # } else {
  #   print(paste("LOAD MODEL",geo_group,crop_group))
  #   fit <- readRDS(here("DB", "model",paste0("bhm_", geo_group, "_", crop_group, ".rds")))
  # }
  # 
  # print(paste("R2 SCORE", bayes_R2(fit)[1,1] ))
  # 
  # orig <- X %>% 
  #   drop_na(sum_insured)
  #   group_by(geo, crop) %>%
  # 
  # pred <- X %>% 
  #   group_by(geo, crop) %>%
  #   summarise(n_year = n(), y = sum(y), sum_insured = sum(sum_insured)) %>%
  #   select(geo, crop, n_year, sum_insured)
  # 
  # pred1 <- tidyr::expand_grid(
  #   geo = unique(X$geo),
  #   crop = unique(X$crop)) %>%
  #   left_join(pred) %>% replace_na(list(n_year = 0))%>%
  #   mutate(
  #     geo = factor(geo, levels = levels(Y$geo)),
  #     crop = factor(crop, levels = levels(Y$crop))#,
  #   )
  # 
  # print(paste("PREDICT"))
  # browser()
  # 
  # 
  # pred_summary <- predict(fit, newdata = pred1, allow_new_levels = FALSE, summary = TRUE) %>%
  #   as.data.frame() %>%
  #   rename(
  #     y_hat = Estimate,
  #     lower = Q2.5,
  #     upper = Q97.5,
  #     se = Est.Error
  #   )
  # 
  # pred <- bind_cols(pred, pred_summary)
  # plot_match(pred, geo_group = "geo", crop_group = "crop")
  # 
  # pred <- pred %>%
  #   mutate(
  #     width = upper - lower,         # Width of 95% interval
  #     rel_width = (upper - lower) / y_hat,  # Relative width
  #     flag_uncertain = rel_width > 1  # Flag if interval is wider than the mean
  #   )
  # 
  # pred %>% 
  #   drop_na() %>%
  #   ggplot( aes(x = y_hat, y = y)) +
  #   geom_point(alpha = 0.3, color = "darkblue", size = 5, stroke = 0, shape = 16)+
  #   geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", linewidth = 1.5) +
  # #  scale_x_log10() + scale_y_log10() +  # log scale for better spread
  #   labs(
  #     title = paste("Predicted x Observed on ", suffix),
  #     x = "Predicted claims_paid",
  #     y = "Observed claims_paid"
  #   ) +
  #   theme_minimal()
  # 
  # 
  # #pred$y_hat <- predict(fit, newdata = pred, allow_new_levels = F, summary = TRUE)[, "Estimate"]
  # 
  # suffix <- paste(geo_group, crop_group, sep = "-")
  # 
  # res <- pred %>%
  #   mutate(
  #     geo = as.character(geo),
  #     crop = as.character(crop)
  #   ) %>%
  #   left_join(X) %>%
  #   rename(
  #     {{ geo_group }} := geo,
  #     {{ crop_group }} := crop
  #   ) %>%
  #   rename_with(
  #     ~ paste0(., "-", suffix),
  #     .cols = -c(all_of(geo_group), all_of(crop_group))
  #   )
  # return(res)
  
}


plot_match <- function(data, geo_group, crop_group){
  # 2. Scatter plot: predicted vs. observed$
  
  suffix <- paste(geo_group, crop_group, sep = "-")
  y_hat <- paste0("y_hat-", suffix)
  y <- paste0("y-", suffix)
  
  data %>% 
    drop_na() %>%
    ggplot( aes(x = .data[[y_hat]], y = .data[[y]])) +
    geom_point(alpha = 0.3, color = "darkblue", size = 5, stroke = 0, shape = 16)+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", linewidth = 1.5) +
    scale_x_log10() + scale_y_log10() +  # log scale for better spread
    labs(
      title = paste("Predicted x Observed on ", suffix),
      x = "Predicted claims_paid",
      y = "Observed claims_paid"
    ) +
    theme_minimal()

}


plot_map_datafill <- function(data, crop, geo_group, crop_group){
  # browser()
  suffix <- paste(geo_group, crop_group, sep = "-")
  y_hat <- paste0("y_hat-", suffix)
  y <- paste0("y-", suffix)
  
  X <- data %>% filter(crop_key == !!crop)
  Y <- get_geo() %>% left_join(X)

  # lims <- range(
  #   c(0,0.5),
  #   na.rm = TRUE
  # )
  lims <- c(0,0.5)

  p1 <- ggplot(Y) +
    geom_sf(aes(fill = .data[[y]]), color = "lightgray", size = 0.1) +
    scale_fill_gradient(
      low      = "#5ccd5c",
      high     = "#cd5c5c",
      na.value = "white",
      limits   = lims,
      name     = "Burn"
    ) +
    theme_minimal() +
    labs(title = "Raw Data")
  p2 <- ggplot(Y) +
    geom_sf(aes(fill = .data[[y_hat]]), color = "lightgray", size = 0.1) +
    scale_fill_gradient(
      low      = "#5ccd5c",
      high     = "#cd5c5c",
      na.value = "white",
      limits   = lims,
      name     = "Burn"
    ) +
    theme_minimal() +
    labs(title = "Bayesian Model Predict")
 
  combined <- p1 + p2 + 
    plot_annotation(title = paste(crop, "on", suffix))

  ggsave(filename = here("DB", "plots", "maps_fill",suffix , paste0(crop,".png")), plot = combined, width = 14, height = 7,dpi = 300)
  return(combined)

  
}


plot_all_crops <- function(data, burn, geo_group, crop_group){
  
  cps <- burn%>% drop_na()%>%group_by(crop_key) %>% summarise(n = n()) %>% pull(crop_key)
  purrr::walk(cps, ~plot_map_datafill(data, crop = .x, geo_group, crop_group))
  
}


# run by mun crop
geo_group = "municipality_id"
crop_group = "crop_key"
burn <- get_burn()
by_mun_crop <- RUN_bayes_hierarchical_model(burn, geo_group, crop_group, retrain = T)
plot_match(by_mun_crop, geo_group, crop_group)
plot_all_crops(by_mun_crop, burn, geo_group, crop_group)



# run by district crop
geo_group = "district_name"
crop_group = "crop_key"
burn <- get_burn()
by_dist_crop <- RUN_bayes_hierarchical_model(burn, geo_group, crop_group, retrain = T)
plot_match(by_dist_crop, geo_group, crop_group)
plot_all_crops(by_dist_crop, burn, geo_group, crop_group)
  


# run by agro_region crop
geo_group = "agro_region"
crop_group = "crop_key"
burn <- get_burn()
by_dist_crop <- RUN_bayes_hierarchical_model(burn, geo_group, crop_group, retrain = T)
plot_match(by_dist_crop, geo_group, crop_group)
plot_all_crops(by_dist_crop, burn, geo_group, crop_group)






library(dplyr)

# Get fitted estimates + 95% CI
preds <- fitted(fit, summary = TRUE) %>%
  as.data.frame() %>%
  rename(pred = Estimate, lower = Q2.5, upper = Q97.5)

# Combine with original data
X_pred <- bind_cols(X, preds)

library(ggplot2)

X_pred %>%
  group_by(municipality_id) %>%
  summarise(
    mean_pred = mean(pred),
    lower = mean(lower),
    upper = mean(upper)
  ) %>%
  ggplot(aes(x = reorder(municipality_id, mean_pred), y = mean_pred)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  coord_flip() +
  scale_y_log10() +
  labs(title = "Predicted Claims with Uncertainty by Municipality",
       x = "Municipality", y = "Predicted claims_paid (log scale)") +
  theme_minimal()

X_pred %>%
  group_by(crop_key) %>%
  summarise(
    mean_pred = mean(pred),
    lower = mean(lower),
    upper = mean(upper)
  ) %>%
  ggplot(aes(x = reorder(crop_key, mean_pred), y = mean_pred)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  coord_flip() +
  scale_y_log10() +
  labs(title = "Predicted Claims with Uncertainty by Crop",
       x = "Crop", y = "Predicted claims_paid (log scale)") +
  theme_minimal()




X_miss <- damage %>%
  left_join(get_geo()) %>%
  filter(agro_region == "D") %>%
  select(municipality_id, crop_key, year, claims_paid, geometry) %>%
  mutate(claims_paid = as.numeric(claims_paid))


# Identify missing
X_missing <- X_miss %>% filter(is.na(claims_paid))

# Predict missing
X_missing$claims_imputed <- predict(fit, newdata = X_missing, summary = TRUE)[, "Estimate"]


X_full <- X_miss %>%
  mutate(
    claims_filled = if_else(is.na(claims_paid), X_missing$claims_imputed, claims_paid)
  )


library(ggplot2)
library(sf)

X_full %>%
  filter(crop_key %in% c("ameixa", "pera", "alperce")) %>%
  ggplot() +
  geom_sf(aes(fill = claims_filled), color = NA) +
  facet_wrap(~crop_key) +
  scale_fill_viridis_c(trans = "log", name = "Imputed claims") +
  labs(title = "Geographical Distribution of Imputed Claims by Crop") +
  theme_minimal()










# 3. Impute missing via posterior predictive ----------------------------
# take the median of posterior draws for each missing row
missing_rows <- which(is.na(demo$loss))
post_draws  <- posterior_predict(fit, newdata = demo[missing_rows, ], draws = 1000)

demo$loss_imputed <- demo$loss
demo$loss_imputed[missing_rows] <- apply(post_draws, 2, median)

# 4. Inspect
demo %>% filter(is.na(loss)) %>% select(municipality,crop,year,loss_imputed)







library(tidyverse)

# assume `demo` and `demo$loss_imputed` from previous example

# 1. select one crop and build a “state” flag
df <- demo %>%
  filter(crop == "ameixa") %>%
  transmute(
    municipality, year,
    loss_before = loss,
    loss_after  = loss_imputed
  ) %>%
  pivot_longer(
    cols = starts_with("loss_"),
    names_to  = "state",
    values_to = "loss"
  ) %>%
  mutate(state = recode(state,
                        loss_before = "observed",
                        loss_after  = "observed + imputed"
  ))

# 2. plot
ggplot(df, aes(x = year, y = loss, color = municipality)) +
  geom_line(data = df %>% filter(state=="observed"),   aes(group=municipality), linetype=1) +
  geom_point(data = df %>% filter(state=="observed"),  size=2) +
  geom_line(data = df %>% filter(state=="observed + imputed"), aes(group=municipality), linetype=2) +
  geom_point(data = df %>% filter(state=="observed + imputed"), shape=1, size=2) +
  facet_wrap(~state, ncol=1) +
  labs(
    title = "Loss over Years for ‘ameixa’",
    subtitle = "Top: only observed data; Bottom: with imputed values",
    y = "loss",
    x = "year"
  ) +
  theme_minimal()




# 
# # install.packages(c("brms","tidyverse"))
# 
# #install.packages("rstudioapi")
# # 1. Demo data ----------------------------------------------------------
# set.seed(42)
# demo <- expand_grid(
#   municipality = paste0("M", 1:8),
#   crop         = c("alma","ameixa","pera","alperce"),
#   year         = 2018:2020
# ) %>%
#   mutate(
#     # simulate non‐negative “damage” via a Gamma
#     loss = rgamma(n(), shape = 2, scale = 500),
#     # randomly blank out 20% for imputation demo
#     loss = if_else(runif(n()) < 0.2, NA_real_, loss)
#   )
# fit_fast <- brm(
#   loss ~ 1 + (1|municipality) + (1|crop) + (1|year) + (1|municipality:crop),
#   data   = demo,
#   family = Gamma(link="log"),
#   chains = 2,    # instead of 4
#   cores  = 2,
#   iter   = 1000, # instead of 2000
#   warmup = 500,  # default is half of iter
#   seed   = 123,
#   refresh=0      # suppress progress output
# )
# 
# summary(fit_fast)               # Check R̂, ESS, group‐SDs, fixed effect
# bayes_R2(fit_fast)              # Bayesian R²  
# pp_check(fit_fast, nsamples=100)  # Posterior‐predictive overlay
# 
# 
# ranef(fit_fast)$municipality    # municipality shrinkage
# ranef(fit_fast)$crop            # crop‐level deviations
# ranef(fit_fast)$year            # year‐effects
# ranef(fit_fast)$`municipality:crop`
# 
# 
# demo2 <- demo
# missing <- which(is.na(demo2$loss))
# pp <- posterior_predict(fit_fast, newdata = demo2[missing, ], draws = 500)
# demo2$loss_imp <- demo2$loss
# demo2$loss_imp[missing] <- apply(pp, 2, median)
