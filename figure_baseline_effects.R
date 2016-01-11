# plot baseline effects by outcome type (exclude confidence)

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

se <- fixed_effects %>% filter(predictor.1 %in% sc_vars)

plt_se <- ggplot(data = se, aes(x=outcome, y=post.mean)) +
  geom_boxplot(aes(stat="boxplot", color=factor(outcome)),lwd=1,fatten=2.5) +
  geom_point(position=position_jitter(width=0.01), size=rel(4), aes(color=factor(outcome),alpha=0.5)) +
  scale_x_discrete(breaks=c("rate_outrage","rate_punishment","rate_threat", "rating"), 
                   labels=c("Outrage", "Punishment", "Threat", "Confidence")) +
  scale_color_manual(values=c('rate_outrage'=color_outrage, 'rate_punishment'=color_punish, 
                              'rate_threat'=color_threat, 'rating'=color_conf)) +
  scale_fill_manual(values=c('rate_outrage'=color_outrage, 'rate_punishment'=color_punish, 
                             'rate_threat'=color_threat, 'rating'=color_conf)) +
  xlab("") +
  coord_cartesian(ylim=c(0,100)) +
  labs(title="A", size=rel(3)) +
  ylab("Baseline rating (points)") +
  theme(
    panel.grid=element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color="black"),
    axis.text.x = element_text(hjust = 0, size=rel(2), color='black'),
    axis.text.y = element_text(hjust = 1, size=rel(2.5), color='black'),
    axis.title.y = element_text(size=rel(1.5)),
    plot.title=element_text(size=20,vjust=2),
    legend.position='none')

ggsave('figure3_scenario_effects.pdf', width=11, height=8.5, units='in', useDingbats=FALSE)