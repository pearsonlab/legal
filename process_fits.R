library(MCMCglmm)
source('helpers.R')


########### fit processing ###########
thisobj <- fitobj

# coefficient plot of fixed effects
X <- extract_fixed(thisobj)
Z <- extract_random(thisobj)
R <- extract_resid(thisobj)

# process and assemble into dataframes and lists
fixed_effects <- cbind(process_fixed_names(X), X)
random_effects <- assemble_cov_mats(Z)
residual_effects <- assemble_cov_mats(R)

# separate out scenario and evidence variables
pred_vars <- levels(fixed_effects$predictor)
sc_vars <- pred_vars[grepl('scenario', pred_vars)]
ev_vars <- setdiff(pred_vars, sc_vars)

rm(list = c('thisobj'))
