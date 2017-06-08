# second figure from the paper

library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(gtable)
library(gridBase)

# load fit object containing mturkers only
#load('data/fit.rdata')
#load('data/dat_rating_comb.rdata')

color_genpop='#1b9e77'
color_lawstudents='#d95f02'
color_lsba ='#7570b3'
color_ilsa ='#9e331b'

color_outrage='#733238'
color_punish='#A69A60'
color_threat='#D9BF3D'
color_conf='#0656A3'

# get posterior means for effects
datfiles <- c('data/stan_model_output_hier_t_mturk.rdata')

renamer <- function(x) {
  gsub("\\[\\s*(\\d+)(,\\s*(\\d+))*\\s*\\]", "_\\3\\_\\1", x, perl=TRUE)
}
qprobs <- c(0.025, 0.5, 0.975)
eff_list <- list()
for (dd in 1:length(datfiles)) {
  load(datfiles[dd])
  
  # get matrix of summary statistics for each variable of interest
  ss <- data.frame(summary(fit, pars=c('mu', 'eta', 'gamma', 'tau', 'sigma'), probs=qprobs)$summary)
  
  # change rownames to make them easy to parse
  rownames(ss) <- sapply(rownames(ss), renamer)
  
  # make row names into a column
  ss$var <- rownames(ss)
  rownames(ss) <- NULL
  
  # make var into separate columns for variable, evidence code, and scenario
  ss <- ss %>% separate(var, into=c("variable", "evidence_num", "scenario"))
  
  # make group a character vector so we can merge without worrying about factor levels
  preds$group <- as.character(preds$group)
  
  # create a trivial evidence code column
  preds$evidence_num <- rownames(preds)
  
  # bind variables columnwise
  df <- left_join(ss, preds, by="evidence_num")
  df$group <- df$group[1]  # make sure group is present in all rows and the same
  eff_list[[length(eff_list) + 1]] <- df
}
effects <- bind_rows(eff_list) %>% mutate(group=factor(group)) %>%
  mutate(scenario=factor(as.numeric(scenario)))

# change name of fit object and do some basic processing
#fitobj <- fit
#source('process_fits.R')

############### Panel 1: Effect sizes for confidence ##################################
# get evidence effects
fe <- effects %>% filter(variable == 'mu', evidence != 'baseline') %>%
      select(mean, evidence, X2.5., X97.5.) %>%
      mutate(evidence=factor(evidence, levels=c("physicalDNA", 
                                                "physicalNon-DNA", 
                                                "witnessYes Witness", 
                                                "historyRelated", 
                                                "historyUnrelated")))

plt_1 <- ggplot(data=fe) +
  geom_pointrange(aes(x=evidence, y=mean, ymin=X2.5., ymax=X97.5.), color=color_genpop, size=1.) + 
  scale_x_discrete(breaks=c("physicalDNA", "physicalNon-DNA", "witnessYes Witness", "historyRelated", "historyUnrelated"), 
                   labels=c("DNA \nphysical \nevidence", 
                            "Non-DNA \nphysical \nevidence",  
                            "Witness \npresent", 
                            "Related \nprior crime", 
                            "Unrelated \nprior crime")) +
  coord_cartesian(ylim=c(0,40)) +
  labs(title="A") +
  ylab("Strength of Case (pooints)") +
  xlab("Evidence Effects") +
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
    plot.title=element_text(size=20, vjust=2, hjust=0.5),
    legend.text = element_text(size=rel(1.5)),
    legend.title = element_text(size=rel(1.5)),
    legend.position='none')


############### Panel 2: Baseline effects ##################################
# get scenario effects
se <- fixed_effects %>% filter(predictor.1 %in% sc_vars, outcome == 'rating')

plt_2 <- ggplot(data = se, aes(x=outcome, y=post.mean)) +
  geom_boxplot(aes(color=factor(outcome)),lwd=1,fatten=2.5) +
  geom_point(position=position_jitter(width=0.01), size=rel(4), aes(color=factor(outcome),alpha=0.5)) +
  scale_x_discrete(breaks=c("rate_outrage","rate_punishment","rate_threat", "rating"), 
                   labels=c("Outrage", "Punishment", "Threat", "Confidence")) +
  scale_color_manual(values=c('rate_outrage'=color_outrage, 'rate_punishment'=color_punish, 
                              'rate_threat'=color_threat, 'rating'=color_conf)) +
  scale_fill_manual(values=c('rate_outrage'=color_outrage, 'rate_punishment'=color_punish, 
                             'rate_threat'=color_threat, 'rating'=color_conf)) +
  xlab("Baseline\nEffect") +
  coord_cartesian(ylim=c(0,40)) +
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

# get observed mean for each scenario across all evidence conditions
# scen_mean <- dat_all %>% group_by(scenario) %>% summarise(sc_mean=mean(rating, na.rm=TRUE))
scen_mean <- se %>% mutate(scenario = as.numeric(sub('scenario','', predictor.1)),
                           sc_mean = post.mean) %>%
                    select(scenario, sc_mean)

# merge into a single dataframe
preds_mean <- merge(preds_mean, dat_summary)
preds_mean <- merge(preds_mean, scen_mean)

plt_3 <- ggplot(data=preds_mean) +
  geom_point(aes(x=mean_pred, y=mean_obs, color=sc_mean), size=3, alpha=0.5) +
  geom_smooth(aes(x=mean_pred, y=mean_obs), color=color_conf, method='lm', formula=y~x) +
  xlab("Weight of Model \nEvidence (points)") +
  coord_cartesian(xlim=c(-5, 60), ylim=c(0,100)) +
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

ggsave('figure_grant_2.pdf', plot=plt_all, width=11, height=4.5, units='in', useDingbats=FALSE)
