#------------------------------------------------------------------------------------
# Stan models to estimate simple logit, "trimmed" log-normal regression, and
# "censored" log-normal regression. For more on the wonders of Stan, visit:
#
# http://mc-stan.org/
#------------------------------------------------------------------------------------

# Refusal to donate
refuse_logit.stan <- "
data {
  int<lower=0> N; // observations
  int<lower=0,upper=1> treat[N]; // treatment vector
  int<lower=0,upper=1> refuse[N]; // response vector
}
parameters {
  vector[2] beta;
}
model {
  // priors
  beta[1] ~ cauchy(0, 2.5);
  beta[2] ~ cauchy(0, 2.5);
  // likelihood
  for (i in 1:N) {
    refuse[i] ~ bernoulli_logit(beta[1] + beta[2] * treat[i]);
  }
}
"

# The trimmed model uses the standard normal model with a
# logged outcome variable.
trimmed.stan <- "
data {
  int<lower=0> N; // observations
  vector[N] treat; // treatment vector
  vector[N] log_wta_z; // response vector
}
parameters {
  real a;
  real b;
  real<lower=0> sigma;
}
model {
  // priors
  a ~ normal(0, 1);
  b ~ normal(0, 1);
  sigma ~ cauchy(0,5);
  // likelihood
  log_wta_z ~ normal(a + b * treat, sigma);
}
"

# Censored regression model
censored.stan <- "
data {
  int<lower=0> N_obs; // observations with WTA value
  int<lower=0> N_cens; // censored observations with WTA value
  vector[N_obs] wta_obs; // wta
  vector[N_obs] treat_obs; // treatment for wta
  vector[N_cens] treat_cens; // treatment for wta
  real<lower=max(wta_obs)> C_z; // censoring value on z scale
}
parameters {
  real b1;
  real b2;
  real<lower=0> sigma;
  vector<lower=C_z>[N_cens] wta_cens;
}
model {
  // priors
  b1 ~ normal(0, 1);
  b2 ~ normal(0, 1);
  sigma ~ cauchy(0,5);
  wta_cens ~ normal(0, 1);

  // likelihood for observed WTA
  wta_obs ~ normal(b1 + b2 * treat_obs, sigma);
  // likelihood for censored WTA
  wta_cens ~ normal(b1 + b2 * treat_cens, sigma);
}
"
