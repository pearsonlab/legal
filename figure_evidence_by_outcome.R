# plot effects of evidence variables by outcome type

library(ggplot2)
library(gridExtra)
library(dplyr)

# load fit object containing mturkers only
load('data/fit.rdata')

# change name of fit object and do some basic processing
fitobj <- fit
source('process_fits.R')

color_outrage='#733238'
color_punish='#A69A60'
color_threat='#D9BF3D'
color_conf='#0656A3'

fe <- fixed_effects %>% filter(predictor.1 %in% ev_vars) %>%
  mutate(predictor=factor(predictor.1, levels=c('physical2', 'physical1','witness1','history2','history1')))

plt_ev <- ggplot(data=fe) +
  geom_pointrange(aes(x=predictor, y=post.mean, ymin=l95.CI, ymax=u95.CI, color=outcome), 
                  size=1.75, position=position_jitter(w=0.15)) + 
  scale_x_discrete(breaks=c("history1","history2","physical1","physical2","witness1"), 
                   labels=c("Unrelated \nprior crime", "Related \nprior crime", 
                            "Non-DNA \nphysical \nevidence", "DNA \nphysical \nevidence", 
                            "Witness \npresent")) +
  scale_color_manual(values=c('rating'=color_conf,'rate_punishment'=color_punish,
                              'rate_threat'=color_threat,'rate_outrage'=color_outrage),
                      name='Rating',
                      breaks=c('rating','rate_outrage','rate_punishment','rate_threat'),
                      labels=c('Confidence','Outrage','Punishment','Threat')) +
  coord_cartesian(ylim=c(0,40)) +
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

ggsave('figure2_evidence_effects.jpg', width=11, height=8, units='in', dpi=200)