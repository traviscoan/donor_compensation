#------------------------------------------------------------------------------------
# This script replicates the main findings in Cohen, Coan, Ottey, and Boyd. (2016).
# For more details, please consult the paper:

# "Sperm Donor Anonymity and Compensation: An Experiment with American Sperm Donors.
# I. Glenn Cohen, Travis G. Coan, Michelle Ottey, and Christina Boyd. 2016. Journal
# of Law and the Biosciences."
#------------------------------------------------------------------------------------

# INITIALIZE
setwd('/home/tcoan/Dropbox (CEMAP)/cohen_coan/fairfax/donor_compensation') # CHANGE THIS DIRECTORY
library(plyr)
library(rstan)
rstan_options(auto_write = TRUE) # Use multiple cores for Stan
options(mc.cores = parallel::detectCores())
library(ggplot2)
library(grid)
library(gridExtra)
source("R/estimation_functions.R")
source("R/plot_functions.R")
source("R/stan_models.R")

###############################################################
### Figure 1. Assessing non-response among inactive donors. ###
###############################################################

# Load post-stratification summary data
load("data/post_stratification_summary.Rda")

# Plot Figure 1
fig1 <- plot_fig1(post_stratification_summary)

# Plot high resolution image per journal requirements
png(file="output/figure1.png",width=1000,height=1000, res = 135)
fig1
dev.off()

##############################################################################
### Figure 2. The effect of mandatory identification on refusal to donate. ###
##############################################################################

# Load analysis data
load("data/replication_data.Rda")

#------------------------------------------------------------------------------
# ESTIMATE "REFUSAL"

# Get logit model estimates for the "full" sample
N <- nrow(df) # prepare data for Stan
refuse <- df$refuse
treat <- df$treat
data_list <- c("N", "refuse", "treat")
# Estimate
fit_full <- stan(model_code = refuse_logit.stan, data=data_list,
                 iter=10000, chains=1) # switch to one long(ish) chain for final run

# Extract estimates for full sample and add a label
df_fig2_full <- extract_refuse_estimates(fit_full)
df_fig2_full$label <- "(a) Difference in refusal proportions (active and inactive donors)"

# Get logit model estimates for "active" donors only
df_active <- subset(df, active == 1, select= c(refuse, treat))
N <- nrow(df_active)
refuse <- df_active$refuse
treat <- df_active$treat
data_list <- c("N", "refuse", "treat")

fit_active <- stan(model_code = refuse_logit.stan, data=data_list,
                   iter=10000, chains=1) # switch to one long(ish) chain for final run

# Extract estimates for active sample and add a label
df_fig2_active <- extract_refuse_estimates(fit_active)
df_fig2_active$label <- "(b) Difference in refusal proportions (active donors only)"

#------------------------------------------------------------------------------
# PLOT

# Merge for combined plot data
df_fig2 <- as.data.frame(rbind(df_fig2_full, df_fig2_active))

# Get label information
label_full <- df_fig2_full[1,]
label_active <- df_fig2_active[1,]

# Plot
fig2 <- plot_fig2(df_fig2, label_full, label_active)

png(file="output/figure2.png", width = 1000, height = 1000, res = 135)
fig2
dev.off()

################################################################
### Figure 3. The effect of mandatory identification on WTA. ###
################################################################

# Remove respondents that "refused"
df <- na.omit(subset(df, refuse == 0, select = c(wta, treat)))

#------------------------------------------------------------------------------
# ESTIMATE "FULL SAMPLE" WTA (INCLUDES PROTEST BIDS)

# Estimate
df_fig3a <- est_wta_model(df, trimmed.stan)

# Add label for plot
df_fig3a$label <- "(a) Difference in Median WTA (full sample)"

#------------------------------------------------------------------------------
# ESTIMATE "TRIMMED"

# Sub-plot (c)
df_fig3c <- est_wta_model(df, trimmed.stan, trim = 500)

# Add label for plot
df_fig3c$label <- "(c) Difference in Median WTA (trimmed at $500)"

# Sub-plot (e)
df_fig3e <- est_wta_model(df, trimmed.stan, trim = 400)

# Add label for plot
df_fig3e$label <- "(e) Difference in Median WTA (trimmed at $400)"

#------------------------------------------------------------------------------
# ESTIMATE "CENSORED"

# Censor at $500
C = 500
df_fig3b <- est_wta_censored(df, censored.stan, C)

# Add label for plot
df_fig3b$label <- '(b) Difference in Median WTA (censored at $500)'

# Censor at $400
C = 400
df_fig3d <- est_wta_censored(df, censored.stan, C)

# Add label for plot
df_fig3d$label <- '(d) Difference in Median WTA (censored at $400)'

#------------------------------------------------------------------------------
# PLOT WTA RESULTS

# Plot the "full" results (upper plot), including protest bids
fig3_u <- plot_fig3_upper(df_fig3a)

# Append the trimmed and censored results.
df_fig3 <- as.data.frame(rbind(df_fig3b, df_fig3c, df_fig3d, df_fig3e))
# Get info for labels
post_medians <- ddply(df_fig3, .(label), summarise,
                      est = mean(x_bar))
# Plot the lower half of Figure 3
fig3_l <- plot_fig3_lower(df_fig3, post_medians)

# Use the grid package to create the final plot
png(file="output/figure3.png", width = 1200, height = 1500, res = 135)
grid.arrange(fig3_u, fig3_l, ncol=1, heights = c(.40, .60))
dev.off()
