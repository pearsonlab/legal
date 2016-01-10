# use mixed models to analyze legal survey data

library(MCMCglmm)
library(plyr)
source('helpers.R')

# change the variable below to include/remove law student population
use_law_students <- TRUE

# read data
datadir <- 'data/'
data_file_names <- paste(datadir, c('data_mq_nothreat_deid.csv', 'data_cu_deid.csv', 'data_sq_nothreat_deid.csv', 'data_nb_deid.csv', 'data_th_deid.csv'), sep="")
dlist <- list()
for (ind in 1:length(data_file_names)) {
  dlist[[ind]] <- read.csv(data_file_names[[ind]])
}
df <- do.call('rbind.fill', dlist)

if (use_law_students) {
  # add a group variable denoting that these data sets are general population
  df['group'] = 'genpop'
  
  lawstudents_df <- read.csv(paste(datadir, 'data_ipls.csv', sep=""))
  lawstudents_df['group'] <- 'legal'
  lawstudents_df['hashedID'] <- lawstudents_df['uid']
  
  # bind these groups together
  df <- rbind.fill(df, lawstudents_df)
  
  # output object name
  outobj <- 'turkers_only.obj'
} else {
  outobj <- 'turkers_and_law_students.obj'
}

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

# all responses; correlated random effects of scenario; correlated residuals; censoring
# if multiple groups included, use group as a factor as well
if (use_law_students) {
  form_string <- paste('cbind(', paste(cens_names, collapse=', '), ')', '~  -1 + trait:group:(scenario + physical + history + witness + victim)', collapse='') 
} else {
  form_string <- paste('cbind(', paste(cens_names, collapse=', '), ')', '~  -1 + trait:(scenario + physical + history + witness + victim)', collapse='') 
}

fit <- MCMCglmm(fixed = as.formula(form_string), 
                 random = ~ us(trait):ID, 
                 rcov = ~ us(trait):units, 
                 family = rep('cengaussian', length(outcomes)), 
                 data = df)
save(fit, file=outobj)

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
