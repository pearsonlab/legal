# make a plot of the slope parameter for dependence of confidence on crime seriousness

load('data/stan_model_output.rdata')
library(dplyr)
library(tidyr)
library(ggplot2)
color_genpop='#0656A3'
color_lawstudents='#33a02c'

# get posterior means for effects
mu_samples <- rstan::extract(fit, pars=c('mu'))$mu

# select columns corresponding to seriousness
# this is currently wonky because preds variable is not handling seriousness
# regressor as intended
which_cols <- preds$group == 'seriousness'

df <- data.frame(mu_samples[, which_cols])
names(df) <- c('legal', 'mturk')
samples <- gather(df)

p <- ggplot(data=samples)
p <- p + geom_density(aes(x=value, color=key)) +
  scale_color_manual(values=c('mturk'=color_genpop,'legal'=color_lawstudents),
                      name='Group',
                      breaks=c('mturk','legal'),
                      labels=c('mTurk','Law Students'))


  geom_boxplot(aes(x=evidence, y=mu, color=group)) + xlab('Effect') + ylab('Effect size') +
  scale_x_discrete(breaks=c("baseline", "historyUnrelated","historyRelated","physicalNon-DNA","physicalDNA","witnessYes Witness"),
                   labels=c("Baseline", "Unrelated\nprior crime", "Related\nprior crime",
                            "Non-DNA\nphysical \nevidence", "DNA\nphysical\nevidence", "Witness\npresent"))