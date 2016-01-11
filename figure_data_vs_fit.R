# here, we want to make a boxplot of confidence ratings for each combination of evidence variables
# and each scenario
# we also want to be able to plot a model prediction on top, along with data for each scenario
# this is figure 1B on the poster

################# load data and packages: all groups ####################
library(dplyr)
library(tidyr)
library(ggplot2)

load('data/fit_mt_ls_conf_pun.obj')
load('data/dat_rating_comb.rdata')
load('data/dat_rating_pun_comb.rdata')
load('data/dat_ipls.rdata')

fitobj <- fit_ms_ls_conf_pun
source('process_fits.R')

##### Get a prediction of confidence from the model for each scenario and variable combo
vars <- c('scenario', 'physical', 'history', 'witness')

# in this next bit, we'll figure out what the levels for each of the above variables is
# we will pass all these to expand.grid to get all the combinations
flist <- list()
for (v in vars) {
  this_fac <- fixed_effects %>% filter(outcome == 'rating', 
                                                predictor.1 == 'groupgenpop', 
                                                grepl(v, predictor.2)) %>% 
    select(predictor.2) %>% transmute(variable=as.character(predictor.2))
  
  # for variables other than scenario, we need to add level 0
  if (v != 'scenario') {
    this_fac <- rbind(paste(v, '0', sep=''), this_fac)
  }
  this_fac <- this_fac %>% transmute(variable=as.factor(variable))
  
  # change the name back to what it should be
  names(this_fac) <- v
  
  # append
  flist <- c(flist, this_fac)
}

# get a dataframe with all combos of factors
preds <- expand.grid(flist)

# convert that dataframe to a matrix
form <- ~ -1 + scenario + physical + history + witness
X <- model.matrix(form, data=preds)
beta <- fixed_effects %>% filter(outcome == 'rating',
                                 predictor.1 == 'groupgenpop') %>%
  select(post.mean) %>% rename(pred=post.mean)

# get predictions for each scenario and add to preds dataframe
pred.means <- as.matrix(X) %*% as.matrix(beta)
preds <- cbind(preds, pred.means)

##### make a dataframe of the average rating per combo of variables and scenario
dat_per_scen <- dat_rating_comb %>% group_by(physical, history, witness, scenario) %>%
  summarise(mean=mean(rating, na.rm=TRUE)) %>% ungroup()
for (v in c('physical', 'history', 'witness', 'scenario')) {
  dat_per_scen[[v]] <- factor(as.integer(dat_per_scen[[v]]), labels=levels(preds[[v]]))
}

##### merge this observation dataframe with the prediction dataframe
preds <- merge(preds, dat_per_scen)

##### now combine evidence variables into a single column and drop unneeded columns
preds <- preds %>% mutate(combo=paste(physical, witness, history, sep="")) %>%
  group_by(combo) %>% mutate(median=median(pred)) %>% ungroup() %>%
  mutate(combo=as.factor(combo)) %>%
  select(scenario, combo, pred, median, mean) 

##### plot boxplots and datapoints of mean values for each combination of scenario and evidence variables
# plot symbols for the median predicted value of the evidence combination across scenarios
plt <- ggplot() +
  geom_boxplot(data=preds, aes(x=combo, y=mean), outlier.colour = NA) +
  geom_point(data=preds, aes(x=combo, y=mean),position=position_jitter(width=0.1), color="grey") +
  geom_point(data=preds, aes(x=combo, y=median), shape=5, color="red", size=6) +
  theme(
    panel.grid=element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color="black"),
    legend.position=c(1,1),
    legend.justification=c(1,1))