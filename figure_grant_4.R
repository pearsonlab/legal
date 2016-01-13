# figure 4 in grant

library(dplyr)
library(tidyr)
library(ggplot2)

load('data/fit_mt_ls_conf_pun.obj')
load('data/dat_rating_comb.rdata')
load('data/dat_rating_pun_comb.rdata')
load('data/dat_ipls.rdata')

fitobj <- fit_ms_ls_conf_pun
source('process_fits.R')

color_genpop='#0656A3'
color_lawstudents='#33a02c'
############### Panel 1: Effect sizes: group contrast ##################################
fe <- fixed_effects %>% filter(outcome == 'rating', predictor.2 %in% ev_vars) %>%
  rename(group=predictor.1, predictor=predictor.2) %>%
  mutate(predictor=factor(predictor, levels=c('physical2', 'physical1','witness1','history2','history1')))

plt_1 <- ggplot(data = fe) +
  geom_pointrange(aes(x=predictor, y=post.mean, ymin=l95.CI, ymax=u95.CI, 
                      color=group), size=1., position=position_jitter(w=0.15)) + 
  scale_x_discrete(breaks=c("history1","history2","physical1","physical2","witness1"), 
                   labels=c("Unrelated\nprior crime", "Related\nprior crime", 
                            "Non-DNA\nphysical \nevidence", "DNA\nphysical\nevidence", "Witness\npresent")) +
  scale_color_manual(values=c('groupgenpop'=color_genpop,'grouplegal'=color_lawstudents),
                      name='Group',
                      breaks=c('groupgenpop','grouplegal'),
                      labels=c('mTurk','Law Students')) +
  coord_cartesian(ylim=c(-20,50)) +
  labs(title="A") +
  ylab("Confidence") +
  xlab("Evidence Effects") +
  geom_vline(xintercept=1.5, colour='grey') +
  geom_vline(xintercept=2.5, colour='grey') +
  geom_vline(xintercept=3.5, colour='grey') +
  geom_vline(xintercept=4.5, colour='grey') +
  geom_hline(yintercept=0, colour='grey') +
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
    legend.position='none')

############### Panel 2: Baseline effects ##################################
# get scenario effects
se <- fixed_effects %>% filter(predictor.2 %in% sc_vars, outcome == 'rating')

plt_2 <- ggplot(data = se, aes(x=predictor.1, y=post.mean)) +
  geom_boxplot(aes(stat="boxplot", color=predictor.1),lwd=1,fatten=2.5) +
  geom_point(position=position_jitter(width=0.01), size=rel(4), 
             aes(color=predictor.1), alpha=0.5) +
  scale_x_discrete(breaks=c("groupgenpop","grouplegal"), 
                   labels=c("mTurk", "Law Students")) +
  scale_color_manual(values=c('groupgenpop'=color_genpop,'grouplegal'=color_lawstudents),
                      name='Group',
                      breaks=c('groupgenpop','grouplegal'),
                      labels=c('mTurk','Law Students')) +
  scale_fill_manual(values=c('groupgenpop'=color_genpop,'grouplegal'=color_lawstudents),
                      name='Group',
                      breaks=c('groupgenpop','grouplegal'),
                      labels=c('mTurk','Law Students')) +
  xlab("Baseline\nEffect") +
  coord_cartesian(ylim=c(-20, 50)) +
  labs(title="B", size=rel(3)) +
  ylab("Confidence") +
  geom_hline(yintercept=0, colour='grey') +
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
    legend.text = element_text(size=rel(1.5)),
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.position=c(-0.5,1),
    legend.justification=c(0,1))

############### Panel 3: Illustration of range of confidences ##################################
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

plt_3 <- ggplot(data=preds_mean) +
  geom_point(aes(x=mean_pred, y=mean_obs, color=group), size=3) +
  geom_smooth(aes(x=mean_pred, y=mean_obs, color=group), method='lm', formula=y~x) +
  scale_color_manual(values=c('groupgenpop'=color_genpop,'grouplegal'=color_lawstudents),
                      name='Group',
                      breaks=c('groupgenpop','grouplegal'),
                      labels=c('mTurk','Law Students')) +
  xlab("Weight of Model \nEvidence (points)") +
  coord_cartesian(xlim=c(0, 90), ylim=c(0,100)) +
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

############### Combine into a single figure ##################################
# make a list of panels
plt_list <- list(plt_1, plt_2, plt_3)

# convert ggplot objects to grobs and standardize heights so axes match
grob_list <- standardize_heights(plt_list)
plt_all <- arrangeGrob(grob_list[[1]], grob_list[[2]], grob_list[[3]], ncol=3, widths=c(1.1, 0.5, 1.25))

# save to disk
ggsave('figure_grant_4.pdf', plot=plt_all, width=11, height=4.5, units='in', useDingbats=FALSE)