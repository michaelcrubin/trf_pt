



### --------------------------- THEORETICAL GAMMA AGGREGATION SIM ----------------------------




# simulate: rows = sims, columns = k exposures to matrix
simulate_k_expos <- function(k, theta, n_sims = 50000, ...){
  
  draws <- matrix(rexp(n_sims * k, rate = 1/theta), nrow = n_sims)
  
}

# theoretical Gamma(k, theta/k) for the average
calc_theory <- function(k, theta, n_sims = 50000){
  
  x_grid <- seq(0, 2, length.out = n_sims)
  theory <- data.frame(
    x = x_grid,
    pdf = dgamma(x_grid, shape = k, scale = theta / k)
  )
  
  return(theory)
}

plot_expo_average <- function(draws, emp_average, theory, k, theta, n_sims = 50000, binw = 0.01, ...){
  # turn to long table
  atom_long <- data.frame( 
    value = as.vector(draws),
    id = factor(rep(1:k, each = n_sims), labels = paste0("Exp", 1:k))
  )
  
  mean_expo   <- mean(atom_long$value)
  mean_theory <- theta

  # take average per draw
  
  p <- ggplot() +
    geom_freqpoly(data = atom_long, 
                  aes(value, after_stat(density), group  = id, colour = "k draws  Exp(1/θ)"),
                  binwidth = theta/50,
                  size = 0.2, show.legend = TRUE) +
    
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
                        values = c("k draws  Exp(1/θ)" = "grey70",
                                   "Empirical average" = "steelblue",
                                   "Gamma(k, θ/k)"     = "red",
                                   "Mean Draws" = "darkgray",
                                   "Theta" = "darkred"
                        )) +
    coord_cartesian(xlim = c(0.0, theta*5)) +
    labs(title = paste0("Aggregation of k=", k, " Exp(", theta, ")"),
         x = "Burn", y = "density") +
    theme_minimal()
  
  return(p)
}

run_exp_sim <- function(k, theta, n_sims, label){
  
  draws <- simulate_k_expos(k, theta, n_sims)
  emp_average  <- data.frame(value = rowMeans(draws))
  theory <- calc_theory(k, theta, n_sims)
  
  p <- plot_expo_average(draws, emp_average, theory, k,theta, n_sims)
  return(p)
  
}


### --------------------------- THEORETICAL GAMMA AGGREGATION SIM ----------------------------
