library(tidyr)
library(dplyr)
library(ggplot2)
color_genpop='#1b9e77'
color_lawstudents='#d95f02'
color_lsba ='#7570b3'
color_ilsa ='#9e331b'
# more colors
# #023bd9
# #b39470

# get posterior means for effects
datfiles <- c('data/stan_model_output_turk.rdata', 
              'data/stan_model_output_ls.rdata',
              'data/stan_model_output_lsba.rdata',
              'data/stan_model_output_ilsa.rdata')

eff_list <- list()
for (dd in 1:length(datfiles)) {
  load(datfiles[dd])
  samples <- rstan::extract(fit, pars=c('mu', 'tau', 'sigma'))
  mu <- colMeans(samples$mu)
  tau <- colMeans(samples$tau)
  sig <- mean(samples$sig)
  
  # bind to variable names in regression
  eff_list[[length(eff_list) + 1]] <- cbind(mu, tau, sig, preds)
}
effects <- bind_rows(eff_list)



# plot

# comparison of effects across populations
p <- ggplot(data=effects)
p <- p + geom_boxplot(aes(x=evidence, y=mu, color=group)) + xlab('Effect') + ylab('Effect size') +
  scale_color_manual(values=c('mturk'=color_genpop,
                              'legal'=color_lawstudents,
                              'lsba'=color_lsba,
                              'ilsa'=color_ilsa),
                      name='Group',
                      breaks=c('legal', 'lsba', 'ilsa', 'mturk'),
                      labels=c('Law Students', 'Louisiana Bar', 'Illinois Prosecutors', 'mTurk')) +
  scale_x_discrete(breaks=c("baseline", "historyUnrelated","historyRelated","physicalNon-DNA","physicalDNA","witnessYes Witness"),
                   labels=c("Baseline", "Unrelated\nprior crime", "Related\nprior crime",
                            "Non-DNA\nphysical \nevidence", "DNA\nphysical\nevidence", "Witness\npresent"))
ggsave('effects_per_scenario.pdf', plot=p, width=11, height=4.5, units='in', useDingbats=FALSE)

# correlation between baselines and "slopes"
# "slope" is a sum of effect sizes for evidence types
eff_slopes <- effects %>% group_by(scenario, group) %>%
  filter(evidence != 'baseline') %>%
  summarise(slope=mean(mu))

eff_baselines <- effects %>% group_by(scenario, group) %>%
  filter(evidence == 'baseline') %>% select(-evidence) %>%
  rename(baseline=mu)

eff_slope_and_baseline <- merge(eff_baselines, eff_slopes)

p <- ggplot(data=eff_slope_and_baseline)
p <- p + geom_point(aes(x=baseline, y=slope, color=group), size=4) + xlab('Baseline') + ylab('Evidence') +
  geom_smooth(aes(x=baseline, y=slope), method=lm, color="black") +
  scale_color_manual(values=c('mturk'=color_genpop,
                              'legal'=color_lawstudents,
                              'lsba'=color_lsba,
                              'ilsa'=color_ilsa),
                      name='Group',
                      breaks=c('legal', 'lsba', 'ilsa', 'mturk'),
                      labels=c('Law Students', 'Louisiana Bar', 'Illinois Prosecutors', 'mTurk'))
ggsave('evidence_vs_baseline.pdf', plot=p, width=8, height=5, units='in', useDingbats=FALSE)


# correlation of scenario baselines
baselines <- effects %>% select(-c(tau, sig)) %>% filter(evidence=='baseline') %>% spread(group, mu)
variances <- effects %>% select(-c(mu, sig)) %>% filter(evidence=='baseline') %>% spread(group, tau)
p <- ggplot(data=baselines)

# now pick one scatter
p <- p + geom_point(aes(x=legal, y=mturk))
p <- p + geom_point(aes(x=lsba, y=mturk))
p <- p + geom_point(aes(x=lsba, y=legal))
 
# correlation matrix
cor(baselines[,3:6], method = 'spearman')

# comparison of variability of baselines within and between groups
baseline_between <- cbind(baselines, std=apply(baselines[3:dim(baselines)[2]], 1, sd))
variance_comparison <- gather(cbind(variances, between=baseline_between$std), 
                              key=group, value=std, legal, lsba, ilsa, mturk, between) %>%
  mutate(scenario=as.numeric(sub("scenario","",scenario))) %>%
  arrange(scenario) %>%
  mutate(scenario=as.factor(scenario))
p <- ggplot(data=variance_comparison)
p <- p + geom_boxplot(aes(x=group, y=std, color=group)) + 
  scale_color_manual(values=c('mturk'=color_genpop,
                              'legal'=color_lawstudents,
                              'lsba'=color_lsba,
                              'ilsa'=color_ilsa,
                              'between'='black'),
                      name='Group',
                      breaks=c('legal', 'lsba', 'ilsa', 'mturk', 'between'),
                      guide=FALSE,
                      labels=c('Law Students', 'Louisiana Bar', 'Illinois Prosecutors', 'mTurk', 'Between Groups')) +
  scale_x_discrete(name='Group',
                      breaks=c('legal', 'lsba', 'ilsa', 'mturk', 'between'),
                      labels=c('Law Students', 'Louisiana Bar', 'Illinois Prosecutors', 'mTurk', 'Between Groups')) +
  ylab("Standard Deviation") +
  ggtitle("Population baseline variance\nbetween vs. within groups")
ggsave('within_vs_between_variance.pdf', plot=p, width=8, height=5, units='in', useDingbats=FALSE)