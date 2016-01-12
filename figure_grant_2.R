# second figure from the grant application

library(ggplot2)
library(gridExtra)
library(dplyr)
library(gtable)

# load fit object containing mturkers only
load('data/fit.rdata')
load('data/dat_rating_comb.rdata')

color_outrage='#733238'
color_punish='#A69A60'
color_threat='#D9BF3D'
color_conf='#0656A3'

# change name of fit object and do some basic processing
fitobj <- fit
source('process_fits.R')

############### Panel 1: Effect sizes for confidence ##################################
# get evidence effects
fe <- fixed_effects %>% filter(predictor.1 %in% ev_vars, outcome == 'rating') %>%
  mutate(predictor=factor(predictor.1, levels=c('physical2', 'physical1','witness1','history2','history1')))

plt_1 <- ggplot(data=fe) +
  geom_pointrange(aes(x=predictor, y=post.mean, ymin=l95.CI, ymax=u95.CI, color=outcome), size=1.5) + 
  scale_x_discrete(breaks=c("history1","history2","physical1","physical2","witness1"), 
                   labels=c("Unrelated \nprior crime", "Related \nprior crime", 
                            "Non-DNA \nphysical \nevidence", "DNA \nphysical \nevidence", 
                            "Witness \npresent")) +
  scale_color_manual(values=c('rating'=color_conf,'rate_punishment'=color_punish,
                              'rate_threat'=color_threat,'rate_outrage'=color_outrage),
                      name='Rating',
                      breaks=c('rating','rate_outrage','rate_punishment','rate_threat'),
                      labels=c('Confidence','Outrage','Punishment','Threat')) +
  coord_cartesian(ylim=c(0,100)) +
  labs(title="A") +
  ylab("Confidence") +
  xlab("Evidence Effect") +
  geom_vline(xintercept=1.5, colour='grey') +
  geom_vline(xintercept=2.5, colour='grey') +
  geom_vline(xintercept=3.5, colour='grey') +
  geom_vline(xintercept=4.5, colour='grey') +
  theme(
    panel.grid=element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color="black"),
    axis.text.x = element_text(hjust = 0.5, size=rel(1), color='black'),
    axis.title.x = element_text(size=rel(1.5)),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(hjust = 1, size=rel(2.5), color='black'),
    axis.title.y = element_text(size=rel(1.5)),
    plot.title=element_text(size=20,vjust=2),
    legend.text = element_text(size=rel(1.5)),
    legend.title = element_text(size=rel(1.5)),
    legend.position='none')

############### Panel 2: Baseline effects ##################################
# get scenario effects
se <- fixed_effects %>% filter(predictor.1 %in% sc_vars, outcome == 'rating')

plt_2 <- ggplot(data = se, aes(x=outcome, y=post.mean)) +
  geom_boxplot(aes(stat="boxplot", color=factor(outcome)),lwd=1,fatten=2.5) +
  geom_point(position=position_jitter(width=0.01), size=rel(4), aes(color=factor(outcome),alpha=0.5)) +
  scale_x_discrete(breaks=c("rate_outrage","rate_punishment","rate_threat", "rating"), 
                   labels=c("Outrage", "Punishment", "Threat", "Confidence")) +
  scale_color_manual(values=c('rate_outrage'=color_outrage, 'rate_punishment'=color_punish, 
                              'rate_threat'=color_threat, 'rating'=color_conf)) +
  scale_fill_manual(values=c('rate_outrage'=color_outrage, 'rate_punishment'=color_punish, 
                             'rate_threat'=color_threat, 'rating'=color_conf)) +
  xlab("Baseline\nEffect") +
  coord_cartesian(ylim=c(0,100)) +
  labs(title="B", size=rel(3)) +
  ylab("Confidence") +
  theme(
    panel.grid=element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color="black"),
    axis.line.y = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_text(size=rel(1.5)),
    axis.ticks.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title=element_text(size=20,vjust=2),
    legend.position='none')

############### Panel 3: Illustration of range of confidences ##################################
# calculate the evidence weight for each combination of variables
vars <- c('scenario', 'physical', 'history', 'witness')

flist <- list()
for (v in vars) {
  this_fac <- fixed_effects %>% filter(outcome == 'rating', grepl(v, predictor.1)) %>% 
    select(predictor.1) %>% transmute(variable=as.character(predictor.1))
  
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
preds <- expand.grid(flist)

# convert that dataframe to a matrix
form <- ~ -1 + (scenario + physical + history + witness)
X <- model.matrix(form, data=preds)

# get beta, setting all scenario effects to 0, so we only add evidence
beta <- fixed_effects %>% filter(outcome == 'rating') %>%
  mutate(pred=replace(post.mean, grepl('scenario', predictor.1), 0)) %>%
  select(pred)

# get predictions for each scenario and add to preds dataframe
pred.means <- as.matrix(X) %*% as.matrix(beta)
preds <- cbind(preds, pred.means)

# get mean prediction across all scenarios
preds_mean <- preds %>% group_by(physical, history, witness) %>%
  summarise(mean_pred=mean(pred))

# get observed mean across all scenarios
cols_to_keep <- c('scenario', 'physical', 'history', 'witness', 'rating')
dat_all <- dat_rating_comb %>% select(one_of(cols_to_keep))
dat_summary <- dat_all %>% group_by(scenario, physical, history, witness) %>%
  summarise(mean_obs=mean(rating, na.rm=TRUE)) 
for (v in c('physical', 'history', 'witness')) {
  dat_summary[[v]] <- factor(as.integer(dat_summary[[v]]), labels=levels(preds_mean[[v]]))
}

# merge into a single dataframe
preds_mean <- merge(preds_mean, dat_summary)

plt_3 <- ggplot(data=preds_mean) +
  geom_point(aes(x=mean_pred, y=mean_obs), color=color_conf, size=3) +
  geom_smooth(aes(x=mean_pred, y=mean_obs), color=color_conf, method='lm', formula=y~x) +
  xlab("Weight of Model \nEvidence (points)") +
  coord_cartesian(xlim=c(0, 60), ylim=c(0,100)) +
  labs(title="C", size=rel(3)) +
  ylab("Confidence (observed)") +
  theme(
    panel.grid=element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color="black"),
    axis.text.x = element_text(hjust = 0.5, size=rel(2), color='black'),
    axis.title.x = element_text(size=rel(1.5)),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(hjust = 1, size=rel(2.5), color='black'),
    axis.title.y = element_text(size=rel(1.5)),
    plot.title=element_text(size=20,vjust=2),
    legend.position='none')
plt


############### Combine into a single figure ##################################
# make a list of panels
plt_list <- list(plt_1, plt_2, plt_3)

# convert ggplot objects to grobs and standardize heights so axes match
grob_list <- standardize_heights(plt_list)
plt_all <- arrangeGrob(grob_list[[1]], grob_list[[2]], grob_list[[3]], ncol=3, widths=c(1.1, 0.5, 1.25))

# save to disk
ggsave('figure_grant_2.pdf', plot=plt_all, width=11, height=4.5, units='in', useDingbats=FALSE)
