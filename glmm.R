# use mixed models to analyze legal survey data

library(MCMCglmm)
library(plyr)
source('helpers.R')

# read data
data_file_names <- c('data/data_mq_deid.csv', 'data/data_cu_deid.csv', 'data/data_sq_deid.csv', 'data/data_nb_deid.csv')
dlist <- list()
for (ind in 1:length(data_file_names)) {
  dlist[[ind]] <- read.csv(data_file_names[[ind]])
}
df <- do.call('rbind.fill', dlist)

# make an integer ID column from the hash
df$ID <- as.integer(df$hashedID)

# make sure some variables are appropriately encoded
predictors <- c('ID', 'scenario', 'physical', 'history', 'witness', 'victim')
outcomes <- c('rating', 'rate_outrage', 'rate_punishment', 'rate_threat')
df <- convert_to_factor(df, predictors)

# upper and lower rating ranges
df$Rmin <- 0
df$Rmax <- 100

############# let's try some models

# single response
fit1 <- MCMCglmm(fixed = rating ~ -1 + scenario + physical + history + witness + victim, 
                 random = ~ scenario:ID, 
                 family = c('gaussian'), 
                 data = df)

# multiple responses; subject-specific baseline; independent random effects; independent residuals 
fit2 <- MCMCglmm(fixed = cbind(rating , rate_outrage ) ~ 
                   -1 + trait:(scenario + physical + history + witness + victim), 
                 random = ~ idh(trait):ID, 
                 rcov = ~ idh(trait):units, 
                 family = rep('gaussian', 2), 
                 data = df)

# multiple responses; random effect of scenario; independent random effects; correlated residuals 
fit3 <- MCMCglmm(fixed = cbind(rating , rate_outrage ) ~ 
                   -1 + trait:(scenario + physical + history + witness + victim), 
                 random = ~ idh(trait):ID, 
                 rcov = ~ us(trait):units, 
                 family = rep('gaussian', 2), 
                 data = df)

# multiple responses; correlated random effects of scenario; correlated residuals 
fit4 <- MCMCglmm(fixed = cbind(rating , rate_outrage ) ~ 
                   -1 + trait:(scenario + physical + history + witness + victim), 
                 random = ~ us(trait):ID, 
                 rcov = ~ us(trait):units, 
                 family = rep('gaussian', 2), 
                 data = df)

# all responses; correlated random effects of scenario; correlated residuals 
fit5 <- MCMCglmm(fixed = cbind(rating , rate_outrage, rate_punishment, rate_threat) ~ 
                   -1 + trait:(scenario + physical + history + witness + victim), 
                 random = ~ us(trait):ID, 
                 rcov = ~ us(trait):units, 
                 family = rep('gaussian', 4), 
                 data = df)

########### make some plots ###########

# coefficient plot of fixed effects
X <- extract_fixed(fit5)
Z <- extract_random(fit5)
R <- extract_resid(fit5)

# process and assemble into dataframes and lists
fixed_effects <- cbind(process_fixed_names(X), X)
random_effects <- assemble_cov_mats(Z)
residual_effects <- assemble_cov_mats(R)

library(ggplot2)

plt <- ggplot(data = fixed_effects)
plt + geom_pointrange(aes(x=predictor, y=post.mean, ymin=l95.CI, ymax=u95.CI, color=outcome)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
