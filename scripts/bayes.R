# library(here)
# invisible(capture.output(source(here("scripts", "global.R"))))
# invisible(capture.output(source(here("scripts", "helpers.R"))))
# 
# library(brms)
# library(mice)
# imp <- mice(nhanes, m = 5, print = FALSE)
# data("nhanes", package = "mice")
# head(nhanes)
# fit_imp1 <- brm_multiple(bmi ~ age*chl, data = imp, chains = 2)
# summary(fit_imp1)
# 
# 
# plot(fit_imp1, variable = "^b", regex = TRUE)
# 
# 
# round(fit_imp1$rhats, 2)
# 
# fit_imp1$fit
# conditional_effects(fit_imp1, "age:chl")



plot_update_process <- function(df){
  
  off_cols <-  c("theta", "post_0")
  
  p <- df %>%
    pivot_longer(-all_of(off_cols), names_to = "stage", values_to = "prob") %>%
    mutate(
      theta = factor(theta),
      stage = factor(stage, levels = setdiff(colnames(df),off_cols)),
      fill_col = case_when(
        stage == "prior_1" ~ "prior",
        stage == paste0("post_", length(flips)) ~ "post",
        grepl("^lik", stage) & prob == max(prob) ~ "maxlik",
        TRUE ~ "base"
      ),
      alpha_col = ifelse(grepl("^lik", stage) & fill_col == "base", 0.3, 1)
    ) %>%
    ggplot(aes(theta, prob, fill = fill_col, alpha = alpha_col)) +
      geom_col(width = 0.9) +
      facet_wrap(~ stage, ncol = 3, scales = "free_y") +
      scale_fill_manual(values = c(
        base    = "lightgray",
        prior   = "darkblue",
        post    = "darkgreen",
        maxlik  = "darkred"
      ), guide = "none") +
      scale_alpha_identity() +
    scale_x_discrete(breaks = function(x) {
      if (length(x) > 20) {
        idx <- round(seq(1, length(x), length.out = 10))
        x[idx]
      } else {
        x
      }
    }) +
    theme_minimal() +
    theme(
      strip.text = element_text(face = "bold"),
      panel.grid = element_blank()
    )
  return(p)
}