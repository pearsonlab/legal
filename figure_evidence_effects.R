# effects of evidence on guilt
# was figure 2A in poster

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

################### FIGURE 2A - EVIDENCE EFFECTS ON CONFIDENCE IN GUILT

fe <- fixed_effects %>% filter(outcome == 'rating', predictor.2 %in% ev_vars) %>%
  rename(group=predictor.1, predictor=predictor.2) %>%
  mutate(predictor=factor(predictor, levels=c('physical2', 'physical1','witness1','history2','history1')))

plt <- ggplot(data = fe) +
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