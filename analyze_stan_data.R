load('data/stan_model_output.rdata')
library(dplyr)
library(ggplot2)
color_genpop='#0656A3'
color_lawstudents='#33a02c'

# get posterior means for effects
mu_samples <- rstan::extract(fit, pars=c('mu'))$mu
mu <- colMeans(mu_samples)

# bind to variable names in regression
effects <- cbind(mu, preds)

# plot

# comparison of effects across populations
p <- ggplot(data=effects)
p <- p + geom_boxplot(aes(x=evidence, y=mu, color=group)) + xlab('Effect') + ylab('Effect size') +
  scale_color_manual(values=c('groupmturk'=color_genpop,'grouplegal'=color_lawstudents),
                      name='Group',
                      breaks=c('groupmturk','grouplegal'),
                      labels=c('mTurk','Law Students')) +
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
  scale_color_manual(values=c('groupmturk'=color_genpop,'grouplegal'=color_lawstudents),
                      name='Group',
                      breaks=c('groupmturk','grouplegal'),
                      labels=c('mTurk','Law Students'))
ggsave('evidence_vs_baseline.pdf', plot=p, width=8, height=5, units='in', useDingbats=FALSE)

