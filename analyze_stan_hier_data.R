library(tidyr)
library(dplyr)
library(ggplot2)
library(rstan)
color_genpop='#1b9e77'
color_lawstudents='#d95f02'
color_lsba ='#7570b3'
color_ilsa ='#9e331b'
# more colors
# #023bd9
# #b39470

# get posterior means for effects
datfiles <- c('data/stan_model_output_hier_t_mturk.rdata', 
              'data/stan_model_output_hier_t_ipls.rdata',
              'data/stan_model_output_hier_t_lsba.rdata',
              'data/stan_model_output_hier_t_ilsa.rdata')

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

# plot

# comparison of effects across populations
p <- ggplot(data=(effects %>% filter(variable=='mu')))
p <- p + geom_pointrange(aes(x=evidence, y=X50., ymin=X2.5., ymax=X97.5., color=group), 
                         position=position_dodge(width = 0.25)) + 
  xlab('Evidence') + ylab('Effect size (points)') +
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
ggsave('evidence_effects_hier.pdf', plot=p, width=11, height=4.5, units='in', useDingbats=FALSE)

# correlation between baselines and "slopes"
# "slope" is a sum of effect sizes for evidence types
eff_slopes <- effects %>% filter(variable=='gamma') %>% 
  group_by(scenario, group) %>%
  filter(evidence != 'baseline') %>%
  summarise(slope=mean(mean))

eff_baselines <- effects %>% filter(variable=='gamma', evidence=='baseline') %>%
  select(-evidence) %>%
  group_by(scenario, group) %>%
  rename(baseline=mean) %>%
  select(baseline, scenario, group)

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
ggsave('evidence_vs_baseline_hier.pdf', plot=p, width=8, height=5, units='in', useDingbats=FALSE)


# correlation of scenario baselines
baselines <- effects %>% filter(evidence=='baseline', variable=='gamma') %>% 
  select(mean, scenario, group) %>% spread(group, mean) %>% arrange(scenario)

p <- ggplot(data=baselines)
# now pick one scatter
p <- p + geom_point(aes(x=legal, y=mturk))
p <- p + geom_point(aes(x=lsba, y=mturk))
p <- p + geom_point(aes(x=lsba, y=legal))
 
# correlation matrix
cor(baselines[,-1], method = 'spearman')

# comparison of variability within and between groups
variance_comparison <- effects %>% 
  filter(variable %in% c('eta', 'tau', 'sigma'), (evidence=='baseline') | (variable == 'sigma')) %>% 
  select(X2.5., X50., X97.5., variable, scenario, group) %>%
  mutate(variable = factor(variable, levels=c('eta', 'tau', 'sigma')))

p <- ggplot() +
  geom_pointrange(data=variance_comparison %>% filter(variable %in% c('eta', 'sigma')),
                  aes(x=variable, y=X50., ymin=X2.5., ymax=X97.5., color=group), 
                  position=position_dodge(width=0.5)) +
  geom_boxplot(data=variance_comparison %>% filter(variable=='tau'),
               aes(x=variable, y=X50., color=group)) +
  scale_x_discrete(name='',
                      breaks=c('eta', 'tau', 'sigma'),
                      limits=c('eta', 'tau', 'sigma'),
                      labels=c('Across Scenarios', 'Across Subjects', 'Within Subjects')) +
  scale_color_manual(values=c('mturk'=color_genpop,
                              'legal'=color_lawstudents,
                              'lsba'=color_lsba,
                              'ilsa'=color_ilsa),
                      name='Group',
                      breaks=c('legal', 'lsba', 'ilsa', 'mturk'),
                      labels=c('Law Students', 'Louisiana Bar', 'Illinois Prosecutors', 'mTurk')) +
  ylim(0, 60) +
  ylab("Standard Deviation (points)")
ggsave('within_vs_between_variance_hier.pdf', plot=p, width=8, height=5, units='in', useDingbats=FALSE)
  