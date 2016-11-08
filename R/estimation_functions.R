#------------------------------------------------------------------------------------
# A set of functions to hold the main estimation routines
#------------------------------------------------------------------------------------

# Use the the rethinking package to extract the posterior estimate
library(rethinking)

#########################
### UTILITY FUNCTIONS ###
#########################

# First difference in the predicted probability
first_diff <- function(x){
  control <- logistic(x[1])
  treat <- logistic(x[1] + x[2])
  return(treat - control)
}

# First difference in the median value
median_diff <- function(x, x_bar, stdev){
  # x: 2-D vector holding the coefficient estimates
  # x_bar: mean(log_wta)
  # stdev: sd(log_wta)
  control <- original_scale(x[1], x_bar, stdev)
  treat <- original_scale(x[1] + x[2], x_bar, stdev)
  return(treat - control)
}

# Exponentiate to get to orginal scale
original_scale <- function(z, x_bar, stdev) {
  # Unstandardize
  x <- z*stdev+ x_bar
  val <- exp(x)
  return(val - 1)
}

############################
### ESTIMATION FUNCTIONS ###
############################

# Extract the estimates for the "refuse" model and store as
# a data frame.
extract_refuse_estimates <- function(fit){
  # Extract  model estimates
  post_list <- extract.samples(fit)    # get posterior
  post <- as.data.frame(post_list$beta)
  names(post) <- c("b1", "b2")
  delta <- apply(post, 1, first_diff)  # estimate treatment effect
  x_bar <- median(delta)               # posterior mean
  ci <- as.vector(quantile(delta,      # credible interval
                                probs = c(0.025, .975)))
  # Plot data frame for ggplot
  df_plot <- as.data.frame(delta)
  df_plot$x_bar <- x_bar
  df_plot$low <- ci[1]
  df_plot$high <- ci[2]
  return(df_plot)
}

# Estimate the log-normal WTA model (both the complete sample
# estimate and the "trimmed" estimates)
est_wta_model <- function(df, model_stan, trim = F){
  # Check triming
  if (trim != F){
    df <- subset(df, wta < trim)
  }
  # Rescale WTA
  df$log_wta <- log(1 + df$wta)               # one person put 0
  df$log_wta_z <- (df$log_wta - mean(df$log_wta))/sd(df$log_wta)
  
  # Set up data for Stan
  N <- nrow(df)
  log_wta_z <- df$log_wta_z
  treat <- df$treat
  data_list <- c("N", "log_wta_z", "treat")
  
  # Fit one long chain for final model
  fit <- stan(model_code = model_stan, data=data_list,
              iter=10000, chains=1, seed = 1234)
  
  # Extract estimates
  post_list <- extract.samples(fit)            # getting posterior
  a <- post_list$a                             # extract constant
  b <- post_list$b                             # extract treatment effect
  sigma <- post_list$sigma
  post <- as.data.frame(cbind(a, b, sigma))
  x_bar <- mean(df$log_wta)
  stdev <- sd(df$log_wta)
  delta <- apply(post, 1, median_diff, 
                 x_bar = x_bar, stdev = stdev)  # estimate treatment effect
  x_bar <- median(delta)                        # posterior median
  ci <- as.vector(quantile(delta,               # credible interval
                                probs = c(0.025, .975)))
  
  df_res <- as.data.frame(delta)
  df_res$x_bar <- x_bar
  df_res$low <- ci[1]
  df_res$high <- ci[2]
  return(df_res)
}

# Estimate the log-normal WTA model with censoring
est_wta_censored <- function(df, model_stan, C){
  # Rescale WTA
  df$log_wta <- log(1 + df$wta)
  # Standardize
  x_bar <- mean(df$log_wta, na.rm = T)
  stdev <- sd(df$log_wta, na.rm = T)
  df$log_wta_z <- (df$log_wta - x_bar)/stdev
  # We need to place the censoring value on the modified log-standardized
  # scale.
  C_log <- log(1 + C)
  C_z <- (C_log - x_bar)/stdev # put on z scale, instead of original
  
  # Set up censoring
  df_cens <- subset(df, wta >= C, select = c(treat, wta, log_wta_z))
  df_obs <- subset(df, wta < C, select = c(treat, wta, log_wta_z))
  # data for censored model
  N_cens <- nrow(df_cens)
  treat_cens <- df_cens$treat
  # data for observed model
  N_obs <- nrow(df_obs)
  treat_obs <- df_obs$treat
  wta_obs <- df_obs$log_wta_z
  
  # Estimation
  data_list <- c("N_obs", "N_cens", "C_z", "wta_obs", "treat_obs", "treat_cens")
  fit <- stan(model_code = model_stan, data=data_list,
              iter=10000, chains=1)
  
  # Extract estimates
  post <- as.data.frame(extract.samples(fit))     # extract posterior
  post <- post[,1:3]
  delta <- apply(post, 1, median_diff, 
                 x_bar = x_bar, 
                 stdev = stdev)                   # estimate treatment effect
  post_median <- median(delta)                    # posterior median
  ci <- as.vector(quantile(delta,                 # credible interval
                           probs = c(0.025, .975)))
  
  # Save estimates to plot
  df_res <- as.data.frame(delta)
  df_res$x_bar <- post_median
  df_res$low <- ci[1]
  df_res$high <- ci[2]
  return(df_res)
}