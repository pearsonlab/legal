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
datfiles <- c('data/stan_model_output_hier_mturk.rdata', 
              'data/stan_model_output_hier_ipls.rdata',
              'data/stan_model_output_hier_lsba.rdata',
              'data/stan_model_output_hier_ilsa.rdata')

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
# effects <- bind_rows(eff_list) %>% mutate(group=factor(group)) %>%
effects <- bind_rows(eff_list) %>% mutate(group=factor(group)) %>%
  mutate(scenario=factor(as.numeric(scenario)))

# plot

# comparison of effects across populations
p <- ggplot(data=(effects %>% filter(variable=='mu')))
p <- p + geom_jitter(aes(x=evidence, y=mean, color=group), width = 0.25) + xlab('Effect') + ylab('Effect size') +
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
#variances <- effects %>% select(-c(mu, sig)) %>% filter(evidence=='baseline') %>% spread(group, tau)
p <- ggplot(data=baselines)

# now pick one scatter
p <- p + geom_point(aes(x=legal, y=mturk))
p <- p + geom_point(aes(x=lsba, y=mturk))
p <- p + geom_point(aes(x=lsba, y=legal))
 
# correlation matrix
cor(baselines[,-1], method = 'spearman')

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