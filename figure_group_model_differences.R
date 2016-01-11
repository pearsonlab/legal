# this code creates an illustrative plot showing evidence weight vs observed confidence
# for each group and each combination of evidence variables
# it is mostly to illustrate differences between model predictions for the groups
# was figure 2B in poster

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

########## FIGURE 2B - MODEL FIT TO OBSERVED CONFIDENCE
# calculate the evidence weight for each combination of variables
vars <- c('scenario', 'physical', 'history', 'witness')

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
grpdf <- data.frame(group=levels(fixed_effects$predictor.1))
preds <- expand.grid(c(grpdf, flist))

# convert that dataframe to a matrix
form <- ~ -1 + group:(scenario + physical + history + witness)
X <- model.matrix(form, data=preds)

# get beta, setting all scenario effects to 0, so we only add evidence
beta <- fixed_effects %>% filter(outcome == 'rating') %>%
  mutate(pred=replace(post.mean, grepl('scenario', predictor.2), 0)) %>%
  select(pred)

# get predictions for each scenario and add to preds dataframe
pred.means <- as.matrix(X) %*% as.matrix(beta)
preds <- cbind(preds, pred.means)

# get mean prediction across all scenarios
preds_mean <- preds %>% group_by(group, physical, history, witness) %>%
  summarise(mean_pred=mean(pred))

# get observed mean across all scenarios
cols_to_keep <- c('scenario', 'physical', 'history', 'witness', 'group', 'rating')
dat_all <- rbind(dat_rating_comb %>% mutate(group='groupgenpop') %>%
                   select(one_of(cols_to_keep)), 
                 dat_ipls %>% mutate(group='grouplegal') %>% 
                   select(one_of(cols_to_keep)))
dat_summary <- dat_all %>% group_by(group, physical, history, witness) %>%
  summarise(mean_obs=mean(rating, na.rm=TRUE)) 
for (v in c('physical', 'history', 'witness')) {
  dat_summary[[v]] <- factor(as.integer(dat_summary[[v]]), labels=levels(preds_mean[[v]]))
}

# merge into a single dataframe
preds_mean <- merge(preds_mean, dat_summary)

plt <- ggplot(data=preds_mean) +
  geom_point(aes(x=mean_pred, y=mean_obs, color=group),size=3) +
  geom_smooth(aes(x=mean_pred, y=mean_obs, color=group), method='lm', formula=y~x) +
  scale_color_discrete(labels=c("Law students", "mTurk")) +
  xlim(0,100) +
  ylim(0,100) +
  theme(
    panel.grid=element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color="black"),
    legend.position=c(1,1),
    legend.justification=c(1,1))
