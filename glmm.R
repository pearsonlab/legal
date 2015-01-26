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
Rmin <- 0
Rmax <- 100

# make upper and lower variables for each outcome
cens_names <- c()
for (oname in outcomes) {
  upname <- paste(oname, 'max', sep='')
  dnname <- paste(oname, '', sep='')
  df[[upname]] <- ifelse(df[[oname]] == Rmax, Inf, df[[oname]])
  df[[dnname]] <- ifelse(df[[oname]] == Rmin, -Inf, df[[oname]])
  cens_names <- c(cens_names, dnname, upname)
}

############# let's try some models

# single response
fit1 <- MCMCglmm(fixed = rating ~ -1 + scenario + physical + history + witness + victim, 
                 random = ~ idh(trait):ID, 
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

# all responses; correlated random effects of scenario; correlated residuals; censoring
form_string <- paste('cbind(', paste(cens_names, collapse=', '), ')', '~  -1 + trait:(scenario + physical + history + witness + victim)', collapse='')

fit6 <- MCMCglmm(fixed = as.formula(form_string), 
                 random = ~ us(trait):ID, 
                 rcov = ~ us(trait):units, 
                 family = rep('cengaussian', length(outcomes)), 
                 data = df)

########### fit processing ###########
thisobj <- fit6

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

########### make some plots ###########
library(ggplot2)

# evidence effects
fe <- fixed_effects[fixed_effects$predictor %in% ev_vars,]
plt <- ggplot(data = fe)
plt + geom_pointrange(aes(x=predictor, y=post.mean, ymin=l95.CI, ymax=u95.CI, color=outcome), size=1.75, position='jitter') + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave('evidence_effects.pdf', width=11, height=8.5, units='in')

# scenario effects
se <- fixed_effects[fixed_effects$predictor %in% sc_vars,]
se <- order_scenarios(se, 'rating')
plt <- ggplot(data = se)
plt + geom_pointrange(aes(x=predictor, y=post.mean, ymin=l95.CI, ymax=u95.CI, color=outcome), size=1.75) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave('scenario_effects.pdf', width=11, height=8.5, units='in')

library(corrplot)
corrplot(cov2cor(random_effects$post.mean), method='ellipse')
corrplot(cov2cor(residual_effects$post.mean), method='ellipse')
