

library(here)
invisible(capture.output(source(here("scripts", "global.R"))))
invisible(capture.output(source(here("scripts", "helpers.R"))))

# ------------START------------------------------------------------

library(MASS)
library(ggplot2)
library(patchwork)



# ------------FUNCS------------------------------------------------

# ------------------------------------------------------------
# SIMULATE MIX OF GAMMAS  —  clean, explicit, no hidden rate/scale swap
# ------------------------------------------------------------
sim_mix <- function(k, n_zero, m,
                    shape0, scale0,   # “near-zero” parameters
                    shape1, scale1) { # “positive”  parameters
  
  ## 1. DRAW THE k COMPONENTS FOR ALL m OBSERVATIONS -----------------
  draws <- matrix(NA_real_, nrow = m, ncol = k)
  
  if (n_zero > 0) {
    draws[, 1:n_zero] <- rgamma(m * n_zero,
                                shape = shape0,
                                scale = scale0)
  }
  if (k > n_zero) {
    draws[, (n_zero + 1):k] <- rgamma(m * (k - n_zero),
                                      shape = shape1,
                                      scale = scale1)
  }
  
  ## 2. AVERAGE ACROSS THE k COMPONENTS ------------------------------
  mix <- rowMeans(draws)
  
  ## 3. RETURN COMPONENTS AND A LONG DATA FRAME FOR PLOTTING ---------
  list(
    mix   = mix,                       # m-length vector of averages
    g0    = draws[, 1L, drop = TRUE],  # first “near-zero” column
    g1    = if (k > n_zero) draws[, k, drop = TRUE] else numeric(0),
    df_long = rbind(
      data.frame(value = mix, dist = "average"),
      data.frame(value = draws[, 1L],          dist = "near-zero"),
      if (k > n_zero)
        data.frame(value = draws[, k],         dist = "positive")
    )
  )
}


# ANALYTICALLY JOIN GAMMAS VIA MOMENT MATCHING
mom_gamma <- function(n_zero, k, shape0, scale0, shape1, scale1) {
  
  w0 <- n_zero / k;  w1 <- 1 - w0
  # single-draw mean/var
  mu  <- w0*shape0*scale0 + w1*shape1*scale1
  var <- w0*(shape0*scale0^2 + mu^2) + w1*(shape1*scale1^2 + mu^2) - mu^2
  # average of k divide var by k
  var <- var / k
  shape_mm <- mu^2 / var
  scale_mm <- var / mu
  c(shape = shape_mm, scale = scale_mm)
}

fit_gamma_mle <- function(x){
  
  fit <- fitdistr(sim$mix, densfun = "gamma")
  par_fit <- coef(fit)
  c(shape = par_fit[["shape"]], scale = 1 / par_fit[["rate"]])
  
  
}

plot_gammas <- function(sim, par_mm, par_mle, ks_mm, ks_mle){
  
  # A) sample densities
  p_samples <- ggplot(sim$df_long, aes(value, after_stat(density), colour = dist, fill = dist)) +
    geom_density(alpha = 0.25, adjust = 1.2) +
    coord_cartesian(xlim = c(0, quantile(sim$df_long$value, 0.99))) +
    labs(title = paste0("Gamma Mix with ", n_zero, " zero-Gammas"),
         x = "value", y = "density", colour = "", fill = "") +
    theme_minimal(base_size = 13)
  
  # B) analytical vs fitted curves
  grid <- data.frame(x = seq(0, quantile(sim$mix, 0.995), length = 400))
  dens_mm  <- dgamma(grid$x, par_mm["shape"],  scale = par_mm["scale"])
  dens_mle <- dgamma(grid$x, par_mle["shape"], scale = par_mle["scale"])
  
  p_curves <- ggplot() +
    geom_histogram(data = data.frame(x = sim$mix),
                   aes(x, after_stat(density)),
                   bins = 100, fill = "grey80", alpha = 0.6) +
    geom_line(data = grid, aes(x, dens_mm,  colour = paste("MM fit: p=",round(ks_mm$p.value, 2))), size = 1) +
    geom_line(data = grid, aes(x, dens_mle, colour = paste("MLE fit: p=",round(ks_mle$p.value, 2))), size = 1, linetype = 2) +
    labs(title = "Approximating the mix with a single Gamma",
         x = "value", y = "density", colour = "") +
    theme_minimal(base_size = 13)

  # combine
  return(p_samples / p_curves)
  
}

# ------------TOP LEVEL------------------------------------------------


# ==== 1. parameter sets ======================================

k <- 20 # components per observation
n_zero <- 10 # how many “small” Gammas
m <- 5000 # sample size
# small-Gamma params
shape0 <- 2
scale0 <- 0.025
shape0 * scale0
sqrt(shape0 * scale0^2)
# positive-Gamma params
shape1 <- 3
scale1 <- 0.2
shape1 * scale1
sqrt(shape1 * scale1^2)

sim <- sim_mix(k, n_zero, m, shape0, scale0, shape1, scale1)
par_mm <- mom_gamma(n_zero, k, shape0, scale0, shape1, scale1)
par_mm
par_mle <- fit_gamma_mle(sim$mix)
par_mle
ks_mm  <- ks.test(sim$mix, "pgamma", shape = par_mm["shape"], scale = par_mm["scale"])
ks_mle <- ks.test(sim$mix, "pgamma", shape = par_mle["shape"], scale = par_mle["scale"])

cat("KS p-value (moment-match) :", ks_mm$p.value, "\n")
cat("KS p-value (MLE fit):", ks_mle$p.value, "\n\n")
p <- plot_gammas(sim, par_mm, par_mle, ks_mm, ks_mle)
p  

df <- sim$df_long# %>% filter(dist != "average")
df%>%group_by(dist) %>%summarise(mn = mean(value))
ggplot(df, aes(value, after_stat(density), colour = dist, fill = dist)) +
  geom_density(alpha = 0.25, adjust = 1.2) +
  coord_cartesian(xlim = c(0, quantile(sim$df_long$value, 0.99))) +
  labs(title = paste0("Gamma Mix with ", n_zero, " zero-Gammas"),
       x = "value", y = "density", colour = "", fill = "") +
  theme_minimal(base_size = 13)




# --- settings -------------------------------------------------------



# --- build one data-frame with bin centres & residuals --------------

sim_bins <- function(shape, scale, burn, binwidth){
  # n_bins = 5
  # burn <- c(0,0,0, 2.6, 2.6, 0.8)
  # breaks <- seq(0, 1, length.out = n_bins)
  breaks   <- seq(0, 1.1, by = binwidth)
  
  df <- data.frame(
    lower = breaks[-length(breaks)],
    upper = breaks[-1]) %>%
    mutate(
      mid = (lower + upper) / 2,
      ) %>%
    rowwise() %>%
    mutate(
      obs = sum(burn >= lower & burn < upper),
      expected = length(burn) * (pgamma(upper, shape, scale = scale) - pgamma(lower, shape, scale = scale)),
      diff = obs - expected
    )
  return(df)
}

optimize <- function(shape, scale, burn, binwidth){
  df <- sim_bins(shape, scale, burn, binwidth)
  df <- df %>% filter(mid >= 0.125)
  sum(abs(df$diff))
  
}


x <- get_burn()
data <- get_damage_grid() %>% left_join(x) %>% replace_na(list(burn_cost = 0))
X <-data %>%
  filter(burn_cost > 0.0) %>%
  dplyr::select(obs = burn_cost)




burn <- X$obs                      # replace with your data vector

binwidth <- 0.01
grid <- tidyr::expand_grid(shape = seq(1, 3, by = 0.1), scale = seq(0.1, 0.8, by= 0.05)) %>%
  mutate(diff = purrr::pmap_dbl(., .f = optimize, burn = burn, binwidth = binwidth))

# --- combined plot --------------------------------------------------
mn <- grid %>% slice_min(diff)
shape <- mn$shape
scale <- mn$scale
df <- sim_bins(shape = shape, scale = scale, burn, binwidth)
ggplot(data = df) +
  ## histogram of observations (blue)
  # geom_histogram(data = data.frame(burn), aes(burn), binwidth = binwidth,  boundary = 0, closed = "left",
  #                fill = "slateblue", alpha = 0.35,
  #                colour = "grey50") +
  ## theoretical Gamma curve (red)
  geom_col(aes(x = mid, y = obs), fill = "lightgray", color = "darkgray", alpha = 0.3, width = binwidth) +
  ## residual bars (green)
  geom_col(aes(x = mid, y = diff), fill = "darkblue", alpha = 0.2, width = binwidth) +
  stat_function(fun = function(x) dgamma(x, shape = shape, scale = scale) *length(burn) * binwidth, colour = "red", size = 1) +
  
  labs(title = "Gamma Fit vs Burn residuals",
       subtitle = paste("Gamma(", shape, "/", scale,")"),
       x = "burn", y = "count") +
  theme_minimal(base_size = 13)

# --- combined plot --------------------------------------------------
mx <- grid %>% slice_max(diff)
shape <- mx$shape
scale <- mx$scale
df <- sim_bins(shape = shape, scale = scale, burn, binwidth)
ggplot(data = df) +
  ## histogram of observations (blue)
  # geom_histogram(data = data.frame(burn), aes(burn), binwidth = binwidth,  boundary = 0, closed = "left",
  #                fill = "slateblue", alpha = 0.35,
  #                colour = "grey50") +
  ## theoretical Gamma curve (red)
  geom_col(aes(x = mid, y = obs), fill = "lightgray", color = "darkgray", alpha = 0.3, width = binwidth) +
  ## residual bars (green)
  geom_col(aes(x = mid, y = diff), fill = "darkblue", alpha = 0.2, width = binwidth) +
  stat_function(fun = function(x) dgamma(x, shape = shape, scale = scale) *length(burn) * binwidth, colour = "red", size = 1) +
  
  labs(title = "Gamma Fit vs Burn residuals",
       subtitle = paste("Gamma(", shape, "/", scale,")"),
       x = "burn", y = "count") +
  theme_minimal(base_size = 13)

















get_1p_range <- function(){
  all_damage <- data %>% drop_na() %>%filter(burn_cost > 0.0)
  below1<-  data %>%filter(burn_cost > 0.0 & burn_cost<0.01)
  print(nrow(below1)/nrow(all_damage))
  
  
}

area <- readRDS(here("DB", "meta", "portugal_continental_geography.rds")) %>% sf::st_drop_geometry() %>% dplyr::select(municipality_id, area)
# area <- RnData::GET_geography("POR", "municipality", return = "sf") %>%
#   mutate(area = as.numeric(sf::st_area(geom))) %>% sf::st_drop_geometry() %>% dplyr::select(municipality_id, area)
  
si <- data %>% drop_na() %>% filter(burn_cost > 0.0) %>%
  group_by(municipality_id) %>%
  summarise(sum_insured = sum(sum_insured), .groups = "drop")

X <- data %>% drop_na() %>% filter(burn_cost > 0.0) %>%
  mutate(type = case_when(burn_cost <0.1~"small", TRUE ~"normal")) %>%
  group_by(municipality_id, type) %>%
  summarise(n = n(),
            .groups = "drop") %>%
  pivot_wider(names_from = "type", values_from = "n") %>%
  tidyr::replace_na(list(small = 0, normal = 0))%>%
  dplyr::filter(normal >=3)%>%
  # tidyr::replace_na(list(small = 0))%>%
  mutate(p_small = small / (normal+small))
df <- X %>% left_join(si)%>% left_join(area)


model <- lm(p_small ~ sum_insured + area, data = df)
summary(model)


ggplot(Y, aes(area, sum_insured)) +geom_point()
ggplot(Y, aes(sum_insured, p_small)) +geom_point()
ggplot(Y, aes(area, p_small)) +geom_point()

p + geom_point()



#-------------## DILUTIONAL EFFECT ------------------------

all_damage <- data %>% drop_na() %>% filter(burn_cost > 0)
ggplot(all_damage, aes(sum_insured, burn_cost)) +
  geom_point(color = "olivedrab", alpha = 0.3)+
labs(title = "Burn x Sum insured")+
  theme_minimal(base_size = 13)

# pre-compute quartile cut-points
qs   <- quantile(df$sum_insured, probs = seq(0, 1, 0.2), na.rm = TRUE)
all_damage %>% 
  filter(burn_cost > 0) %>% 
  mutate(quartile = (cut(sum_insured,
                        breaks = qs,
                        labels = paste("Q", names(qs)[-1]),
                        include.lowest = TRUE))) %>%
  ggplot(aes(burn_cost)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 100, fill = "olivedrab", alpha = 0.3) +
  facet_wrap(~ quartile, ncol = 1, scales = "free_y") +
  scale_x_continuous(limits = c(0, 1.01)) +
  labs(title = "Burn distribution by quartile of sum-insured",
       x = "burn", y = "cnt") +
  theme_minimal(base_size = 13)

X %>% group_by(quartile) %>% sum(n=n(), mx = max(burn_cost))


fit <- lm(log(burn_cost) ~ log(sum_insured), data = df, subset = burn_cost > 0)
summary(fit)
ggplot(df, aes(sum_insured, burn_cost)) +geom_point()



freq_fit <- glm(I(burn_cost > 0) ~ log(sum_insured),
                data = df, family = binomial)
summary(freq_fit)




library(dplyr)
library(ggplot2)
library(scales)  # for comma()

# 1. Pre-compute your 5-bin cut points and labels
qs   <- quantile(df$sum_insured, probs = seq(0, 1, 0.1), na.rm = TRUE)
q_lbl <- paste0(
  comma(head(qs, -1), accuracy = 1),
  " – ",
  comma(tail(qs, -1), accuracy = 1)
)

# 2. Compute the bin factor and plot boxplots
qs   <- quantile(all_damage$sum_insured, probs = seq(0, 1, 0.1), na.rm = TRUE)
all_damage %>% 
  filter(burn_cost > 0) %>% 
  mutate(quartile = cut(sum_insured,
                         breaks = qs,
                         labels = paste("Q", names(qs)[-1]),
                         include.lowest = TRUE)) %>%
  ggplot(aes(x = quartile, y = burn_cost)) +
  geom_boxplot(fill = "olivedrab3", color = "olivedrab", alpha = 0.6, outlier.size = 3) +
  scale_y_continuous(limits = c(0, 1.01)) +
  labs(
    title = "Dilution effect of burn cost",
    x = "Sum insured Q",
    y = "Burn cost"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



#-------------## DILUTIONAL EFFECT ON AREA ------------------------

area <- readRDS(here("DB", "meta", "portugal_continental_geography.rds")) %>% sf::st_drop_geometry() %>% dplyr::select(municipality_id, area)
# area <- RnData::GET_geography("POR", "municipality", return = "sf") %>%
#   mutate(area = as.numeric(sf::st_area(geom))) %>% sf::st_drop_geometry() %>% dplyr::select(municipality_id, area)

all_damage_area <- data %>% drop_na() %>% filter(burn_cost > 0) %>% left_join(area)
ggplot(all_damage_area, aes(area, burn_cost)) +
  geom_point(color = "salmon", alpha = 0.3)+
  labs(title = "Burn x Sum insured")+
  theme_minimal(base_size = 13)

# pre-compute quartile cut-points
qs <- quantile(all_damage_area$area, probs = seq(0, 1, 0.2), na.rm = TRUE)
all_damage_area %>% 
  filter(burn_cost > 0) %>% 
  mutate(quartile = (cut(area,
                         breaks = qs,
                         labels = paste("Q", names(qs)[-1]),
                         include.lowest = TRUE))) %>%
  ggplot(aes(burn_cost)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 100, fill = "salmon", alpha = 0.3) +
  facet_wrap(~ quartile, ncol = 1, scales = "free_y") +
  scale_x_continuous(limits = c(0, 1.01)) +
  labs(title = "Burn distribution by quartile of area",
       x = "burn", y = "cnt") +
  theme_minimal(base_size = 13)


# 2. Compute the bin factor and plot boxplots
qs   <- quantile(all_damage_area$area, probs = seq(0, 1, 0.1), na.rm = TRUE)
all_damage_area %>% 
  filter(burn_cost > 0) %>% 
  mutate(quartile = cut(area,
                        breaks = qs,
                        labels = paste("Q", names(qs)[-1]),
                        include.lowest = TRUE)) %>%
  ggplot(aes(x = quartile, y = burn_cost)) +
  geom_boxplot(fill = "salmon", color = "salmon3", alpha = 0.6, outlier.size = 3) +
  scale_y_continuous(limits = c(0, 1.01)) +
  labs(
    title = "Dilution effect of burn cost",
    x = "Area Q",
    y = "Burn cost"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



# ------------------------------------------------------------
#  Erlang-averaging demo
#  • k identical Exponential(θ) severities
#  • blue  = empirical density of their average
#  • red   = theoretical Gamma(k, θ/k) density
#  • grey  = each individual Exponential density
# ------------------------------------------------------------


invisible(capture.output(source(here("scripts", "gamma_funcs.R"))))

# ---- parameters---------------------------------------



# define grid and then run it
grid <- tidyr::expand_grid(theta = c( 0.3), alpha = c(1.3), k = c(27), n_sims = 50, rho = c(0, 0.2, 0.4, 0.6, 0.8, 0.98)) %>%
  mutate(label = paste(alpha, theta, as.integer(n_sims), k, rho, sep="_"))
plots <- grid %>%purrr::pmap(., .f = run_gamma_real) %>% setNames(grid$label)


combined_plot <- plots %>%
  wrap_plots(ncol = 2) +
  plot_annotation(
    title = paste("REal world"),
    theme = theme(
      plot.title = element_text(hjust = 0.5)
    )
  )
combined_plot

