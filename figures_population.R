#1A, 1B, 2A, 2B, 3A, 3B, 3C
#1A, 3B, 3C

library(dplyr)
library(tidyr)
library(ggplot2)

load('data/fit_mt_ls_conf_pun.obj')
load('data/dat_rating_comb.rdata')
load('data/dat_rating_pun_comb.rdata')
load('data/dat_ipls.rdata')

fitobj <- fit_ms_ls_conf_pun
source('process_fits.R')

################## FIGURE 1B - MODEL FIT TO OBSERVED RESULTS

# here, we want to make a boxplot of confidence ratings for each combination of evidence variables
# and each scenario
# we also want to be able to plot a model prediction on top, along with data for each scenario

# 1) Get a prediction of confidence from the model for each scenario and variable combo
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

# 2) make a dataframe of the average rating per combo of variables and scenario
dat_per_scen <- dat_rating_comb %>% group_by(physical, history, witness, scenario) %>%
  summarise(mean=mean(rating, na.rm=TRUE)) %>% ungroup()
for (v in c('physical', 'history', 'witness', 'scenario')) {
  dat_per_scen[[v]] <- factor(as.integer(dat_per_scen[[v]]), labels=levels(preds[[v]]))
}

# 3) merge this with the prediction dataframe
preds <- merge(preds, dat_per_scen)

# 4) now combine evidence variables into a single column, reorder this variable by its median
# value across scenarios, and drop unneeded columns
preds <- preds %>% mutate(combo=paste(physical, witness, history, sep="")) %>%
  group_by(combo) %>% mutate(median=median(pred)) %>% ungroup() %>%
  mutate(combo=as.factor(combo)) %>%
  select(scenario, combo, pred, median, mean) 

# 5) plot boxplots and datapoints of mean values for each combination of scenario and evidence variables
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

################### FIGURE 2A - EVIDENCE EFFECTS ON CONFIDENCE IN GUILT

fe <- fixed_effects %>% filter(outcome == 'rating', predictor.2 %in% ev_vars) %>%
  rename(group=predictor.1, predictor=predictor.2) %>%
  mutate(predictor=factor(predictor, levels=c('physical2', 'physical1','witness1','history2','history1')))

plt_ev <- ggplot(data = fe) +
  geom_pointrange(aes(x=predictor, y=post.mean, ymin=l95.CI, ymax=u95.CI, 
                      color=group), size=1.75, position=position_jitter(w=0.15)) + 
  scale_x_discrete(breaks=c("history1","history2","physical1","physical2","witness1"), 
                   labels=c("Unrelated \nprior crime", "Related \nprior crime", "Non-DNA \nphysical \nevidence", "DNA \nphysical \nevidence", "Witness \npresent")) +
  coord_cartesian(ylim=c(0,50)) +
  labs(title="A") +
  ylab("Effect size (points above baseline)") +
  xlab("") +
  geom_vline(xintercept=1.5, colour='grey') +
  geom_vline(xintercept=2.5, colour='grey') +
  geom_vline(xintercept=3.5, colour='grey') +
  geom_vline(xintercept=4.5, colour='grey') +
  theme(
    panel.grid=element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color="black"),
    axis.text.x = element_text(hjust = 0, size=rel(2), color='black'),
    axis.text.y = element_text(hjust = 1, size=rel(2.5), color='black'),
    axis.title.y = element_text(size=rel(1.5)),
    plot.title=element_text(size=20,vjust=2),
    legend.text = element_text(size=rel(1.5)),
    legend.title = element_text(size=rel(1.5)),
    legend.position=c(1,1),
    legend.justification=c(1,1))

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


##### FIGURE 3A BASELINE CONFIDENCE IN GUILT VARIES BY SCENARIO
se <- fixed_effects %>% filter(outcome == 'rating',
                               grepl('scenario', predictor.2))
plt <- ggplot() +
  geom_boxplot(data=se, aes(x=predictor.1, y=post.mean), outlier.colour = NA) +
  geom_point(data=se, aes(x=predictor.1, y=post.mean), 
             position=position_jitter(width=0.1), size=3, color="grey") +
  theme(
    panel.grid=element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color="black"),
    legend.position=c(1,1),
    legend.justification=c(1,1))




#=== correlation
library(corrplot)
df <- se %>% select(predictor.1, outcome, post.mean) %>% spread(outcome, post.mean)
names(df) <- c('Scenario', 'Confidence\n in guilt','Threat','Outrage','Deserved\n punishment')

pdf("figure4_corrmatrix.pdf", width=5, height=5)
corrplot.mixed(cor(df %>% select(-Scenario)), lower='ellipse', upper='number')
dev.off()

corr_grob <- corrplot.mixed(cor(df %>% select(-Scenario)), lower='ellipse', upper='number')

# combined plots correlations

pltgrp2 <- arrangeGrob(plt_se_noconf, corr_grob, ncol=2 ,widths=c(1,1))

ggsave(pltgrp2, file="figure3_combined.pdf", width=16, height=8, units='in', useDingbats=FALSE)


