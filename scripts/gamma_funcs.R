

### --------------------------- THEORETICAL EXP AGGREGATION SIM ----------------------------

# simulate: rows = sims, columns = k exposures to matrix
simulate_k_expos <- function(k, theta, n_sims = 50000, ...){
  
  draws <- matrix(rexp(n_sims * k, rate = 1/theta), nrow = n_sims)
  
}

run_exp_sim <- function(k, theta, n_sims, label){
  
  draws <- simulate_k_expos(k, theta, n_sims)
  emp_average  <- data.frame(value = rowMeans(draws))
  theory <- calc_theory(k, theta, n_sims)
  
  p <- plot_expo_average(draws, emp_average, theory, k, theta, n_sims)
  return(p)
  
}


plot_expo_average <- function(draws, emp_average, theory, k, theta, n_sims = 50000, ...){
  # turn to long table
  atom_long <- data.frame( 
    value = as.vector(draws),
    id = factor(rep(1:k, each = n_sims), labels = paste0("Exp", 1:k))
  )
  
  mean_expo   <- mean(atom_long$value)
  mean_theory <- theta
  mx_y <- theory$pdf%>%max()*2
  
  
  p <- ggplot() +
    geom_freqpoly(data = atom_long, 
                  aes(value, after_stat(density), group  = id, colour = "k draws Exp(1/θ)"),
                  binwidth = theta/50,
                  size = 0.2, show.legend = TRUE) +
    # 
    geom_density(data = emp_average,
                 aes(value, after_stat(density), colour = "Empirical average"),
                 size = 0.5, show.legend = TRUE) +
    
    geom_line(data = theory,
              aes(x, pdf, colour = "Gamma(k, θ/k)"),
              size = 0.5, show.legend = TRUE) +
    
    geom_vline(aes(xintercept = mean_expo, colour = "Mean Draws"), linetype = "dashed", size = 1) +
    geom_vline(aes(xintercept = theta, colour = "Theta"), linetype = "dashed", size = 1) +
    
    ## manual legend colours
    scale_colour_manual(name = "",
                        values = c("k draws Exp(1/θ)" = "grey70",
                                   "Empirical average" = "steelblue",
                                   "Gamma(k, θ/k)"     = "red",
                                   "Mean Draws" = "darkgray",
                                   "Theta" = "darkred"
                        )) +
    coord_cartesian(xlim = c(0.0, theta*5), ylim = c(0.0, mx_y)) +
    labs(title = paste0("Aggregation of k=", k, " Exp(", theta, ")"),
         subtitle = paste0("Monte Carlo ", n_sims, " Draws"),
         
         x = "Burn", y = "density") +
    theme_minimal()
  
  return(p)
}




### --------------------------- THEORETICAL GAMMA AGGREGATION SIM ----------------------------


# simulate: rows = sims, columns = k exposures to matrix
simulate_k_gammas <- function(k, alpha, theta, n_sims = 50000, ...){
  
  draws <- matrix(rgamma(n_sims * k, shape = alpha, scale = theta), nrow = n_sims)  
}


# theoretical Gamma(k, theta/k) for the average
calc_theory <- function(k, theta, n_sims = 50000, alpha = 1){
  
  x_grid <- seq(0, 2, length.out = 50000)
  theory <- data.frame(
    x = x_grid,
    pdf = dgamma(x_grid, shape = k * alpha, scale = theta / k)
  )
  
  return(theory)
}

plot_gamma_average <- function(draws, emp_average, theory, k, alpha, theta, n_sims = 50000, rho = 0, ...){
  # turn to long table
  atom_long <- data.frame( 
    value = as.vector(draws),
    id = factor(rep(1:k, each = n_sims), labels = paste0("Gamma", 1:k))
  )

  mean_emp <- mean(emp_average$value)
  sd_emp <- sd(emp_average$value)

  mean_theory <- theta*alpha
  sd_theory <- sqrt(alpha) * theta * (rho + (1-rho)/sqrt(k))
  mx_y <- theory$pdf%>%max()
  
  sd_delta = (sd_emp - sd_theory)/sd_theory
  
  p <- ggplot() +
    geom_freqpoly(data = atom_long, 
                  aes(value, after_stat(density), group  = id, colour = "k draws Gamma(α, θ)"),
                  binwidth = theta/50,
                  size = 0.2, show.legend = TRUE) +
    # 
    geom_density(data = emp_average,
                 aes(value, after_stat(density), colour = "Empirical average"),
                 size = 0.5, show.legend = TRUE) +

    geom_line(data = theory,
              aes(x, pdf, colour = "Gamma(k*α, θ/k)"),
              size = 0.5, show.legend = TRUE) +
    
    geom_vline(aes(xintercept = mean_emp, colour = "Mean Draws"), linetype = "dashed", size = 1) +
    geom_vline(aes(xintercept = mean_theory, colour = "αθ"), linetype = "dashed", size = 1) +
    annotate(
      "text",
      x = theta * 2,        # x-Position (nach Bedarf anpassen)
      y = mx_y,               # y-Position ganz oben
      label = paste(
        "sd empirical:", round(sd_emp, 3),
        "\nsd theory:", round(sd_theory, 3),
        "\nDelta:", round(sd_delta*100, 0),"%"
      ),
      hjust = 0,              # linksbündig
      vjust = 1,              # an der Oberkante
      size = 4,               # Textgröße
      fontface = "italic"
    ) +
    
    ## manual legend colours
    scale_colour_manual(name = "",
                        values = c("k draws Gamma(α, θ)" = "grey70",
                                   "Empirical average" = "steelblue",
                                   "Gamma(k*α, θ/k)"     = "red",
                                   "Mean Draws" = "darkgray",
                                   "αθ" = "darkred"
                        )) +
    coord_cartesian(xlim = c(0.0, theta*5), ylim = c(0.0, mx_y*2)) +
    labs(title = paste0("Aggregation of k=", k, " Gamma(", alpha,",", theta, ")"),
         subtitle = paste0("Monte Carlo ", n_sims, " Draws"),
         x = "Burn", y = "density") +
    theme_minimal()
  p
  return(p)
}


run_gamma_sim <- function(k, alpha, theta, n_sims, label){
  
  draws <- simulate_k_gammas(k, alpha, theta, n_sims)
  emp_average  <- data.frame(value = rowMeans(draws))
  theory <- calc_theory(k, theta, n_sims, alpha)
  
  p <- plot_gamma_average(draws, emp_average, theory, k, alpha, theta, n_sims)
  return(p)
  
}


### --------------------------- DEPENDENT GAMMA AGGREGATION SIM ----------------------------

simulate_k_dependent_gammas <- function(k, alpha, theta, rho = 0.05, n_sims  = 50000) {

  if (rho < 0 | rho > 1)
    stop("rho must be in [0, 1).")
  
  core <- matrix(rgamma(n_sims * k, shape = alpha, scale = theta), nrow = n_sims, ncol = k)
  
  # skip the stuff of independent
  eps <- 0.001
  if (rho > eps) {
    var_field   <- alpha * theta^2
    var_weather <- var_field * rho / (1 - rho)
    
    # keep mean(W) = 1
    scale_w <- sqrt(var_weather)
    shape_w <- 1 / scale_w
    
    W <- rgamma(n_sims, shape = shape_w, scale = scale_w)
    core <- core * W          # broadcast row-wise
  }
  
  return(core)
}


calc_theory_rho <- function(k, theta, rho = 0, alpha = 1, n_sims = 50000) {
  mu  <- alpha * theta # theoretical mean
  
  # adjusted variance with correlation
  var <- alpha * theta^2 * (rho + (1 - rho) / k)
  shape <- mu^2 / var
  scale <- var / mu
  
  xgrid <- seq(0, theta * 5, length.out = n_sims)
  theory <- data.frame(
    x = xgrid, 
    pdf = dgamma(xgrid, shape = shape, scale = scale)
    )
  return(theory)
}



plot_gamma_dependent <- function(draws, emp_average, theory, k, alpha, theta, n_sims = 50000, rho = 0, ...){
  # turn to long table
  atom_long <- data.frame( 
    value = as.vector(draws),
    id = factor(rep(1:k, each = n_sims), labels = paste0("Gamma", 1:k))
  )

  mean_emp <- mean(emp_average$value)
  sd_emp <- sd(emp_average$value)
  
  mean_theory <- theta*alpha
  sd_theory <- sqrt(alpha) * theta * (rho + (1-rho)/sqrt(k))
  mx_y <- theory$pdf%>%max()
  
  sd_delta = (sd_emp - sd_theory)/sd_theory
  
  # -------- plot ---------------------------------------------------
  p <- ggplot() +
    # geom_freqpoly(data = atom_long, 
    #               aes(value, after_stat(density), group  = id, colour = "k Fields Gamma(α,θ)"),
    #               binwidth = theta/50,
    #               size = 0.2, show.legend = TRUE) +
    # 
    geom_density(data = emp_average,
                 aes(value, after_stat(density), colour = "Empirical avg"),
                 size =1, show.legend = TRUE) +
    
    geom_line(data = theory,
              aes(x, pdf, colour = "Gamma MM"),
              size = 1, show.legend = TRUE) +
    
    geom_vline(aes(xintercept = mean_emp, colour = "Mean Draws"), linetype = "dashed", size = 1) +
    geom_vline(aes(xintercept = mean_theory, colour = "αθ"), linetype = "dashed", size = 1) +
    annotate(
      "text",
      x = theta * 2, 
      y = mx_y, 
      label = paste(
        "sd empirical:", round(sd_emp, 3),
        "\nsd theory:", round(sd_theory, 3),
        "\nDelta:", round(sd_delta*100, 0),"%"
      ),
      hjust = 0, 
      vjust = 1,  
      size = 4, 
      fontface = "italic"
    ) +
    
    ## manual legend colours
    scale_colour_manual(name = "",
                        values = c("k Fields Gamma(α,θ)" = "grey70",
                                   "Empirical avg" = "steelblue",
                                   "Gamma MM" = "red",
                                   "Mean Draws" = "darkgray",
                                   "αθ" = "darkred"
                        )) +
    coord_cartesian(xlim = c(0.0, theta*5), ylim = c(0.0, mx_y*2)) +
    labs(title = paste0("Aggreg k=", k, " Fields Gamma(", alpha,",", theta, ") Rho =",rho ),
         subtitle = paste0("Simulated ", n_sims, " years"),
         x = "Burn", y = "density") +
    theme_minimal()
  p
  return(p)
}


run_gamma_real <- function(k, alpha, theta, rho, n_sims, label){
  draws <- simulate_k_dependent_gammas(k, alpha, theta, rho, n_sims)
  emp_average  <- data.frame(value = rowMeans(draws))
  theory <- calc_theory_rho(k, theta, rho, alpha, n_sims)
  p <- plot_gamma_dependent(draws, emp_average, theory, k, alpha, theta, n_sims, rho)
  return(p)
  
}
