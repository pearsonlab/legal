# fourth figure from the paper
library(tidyverse)
library(grid)
library(gridExtra)
library(gtable)
library(gridBase)

source('ggplot_setup.R')
load('data/stan_hier_postprocess_multi_all.rdata')

############### Panel 1: mTurk effects by outcome type ##################################
# get evidence effects
fe <- effects %>% filter(variable == 'mu', evidence != 'baseline') %>%
      select(mean, evidence, X2.5., X97.5., group, outcome) %>%
      mutate(evidence=factor(evidence, levels=c("physicalDNA", 
                                                "physicalNon-DNA", 
                                                "witnessYes Witness", 
                                                "historyRelated", 
                                                "historyUnrelated"))) %>%
      mutate(outcome=factor(outcome, levels=c("rating", 
                                              "rate_punishment", 
                                              "rate_outrage", 
                                              "rate_threat")))

plt_1 <- ggplot(data=fe) +
  geom_hline(yintercept=0, colour='grey') +
  geom_pointrange(aes(x=evidence, y=mean, ymin=X2.5., ymax=X97.5., color=outcome), size=1.,
                  position=position_dodge(width = 0.75)) + 
  evidence_x_axis +
  outcome_color_scale +
  coord_cartesian(ylim=c(0,100)) +
  labs(title="A") +
  ylab("Strength of Case (points)") +
  xlab("Evidence Effects") +
  geom_vline(xintercept=1.5, colour='grey') +
  geom_vline(xintercept=2.5, colour='grey') +
  geom_vline(xintercept=3.5, colour='grey') +
  geom_vline(xintercept=4.5, colour='grey') +
  th + 
  theme(
    axis.text.x = element_text(hjust = 0.5, size=rel(1), color='black')
  )


############### Panel 2: Baseline effects ##################################
# get scenario effects
se <- effects %>% filter(variable == 'gamma', evidence == 'baseline') %>% 
                  select(scenario, mean, group, outcome) %>%
      mutate(outcome=factor(outcome, levels=c("rating", 
                                              "rate_punishment", 
                                              "rate_outrage", 
                                              "rate_threat")))

plt_2 <- ggplot(data = se, aes(x=outcome, y=mean)) +
  geom_hline(yintercept=0, colour='grey') +
  geom_boxplot(aes(color=outcome), lwd=1, fatten=1, outlier.size=0, outlier.stroke=0) +
  outcome_x_axis +
  outcome_color_scale +
  outcome_fill_scale +
  xlab("Baseline\nEffect") +
  coord_cartesian(ylim=c(0,100)) +
  labs(title="B", size=rel(3)) +
  ylab("Confidence") +
  th +
  theme(
    axis.line.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position=c(1.1,1),
    legend.justification=c(0,1))
    
############### Panel 3a: Punishment and case strength effect correlations ##################################
plt_3a <- ggplot(data=(effects %>% filter(variable=='rho'))) +
  geom_pointrange(aes(x=evidence, y=X50., ymin=X2.5., ymax=X97.5.), 
                         position=position_dodge(width = 0.5)) + 
  xlab('Evidence') + ylab('Strength of Case -\nPunishmnet Correlation') +
  outcome_color_scale +
  evidence_plus_baseline_x_axis +
  labs(title="C", size=rel(3)) +
  geom_vline(xintercept=1.5, colour='grey') +
  geom_vline(xintercept=2.5, colour='grey') +
  geom_vline(xintercept=3.5, colour='grey') +
  geom_vline(xintercept=4.5, colour='grey') +
  geom_vline(xintercept=5.5, colour='grey') +
  th + 
  theme(
    axis.text.x = element_text(hjust = 0.5, size=rel(1), color='black'),
    legend.position=c(1.25, 1)
  )

load('data/stan_hier_postprocess_multi.rdata')

############### Panel 3: Punishment and case strength effect correlations ##################################
plt_3 <- ggplot(data=(effects %>% filter(variable=='rho'))) +
  geom_pointrange(aes(x=evidence, y=X50., ymin=X2.5., ymax=X97.5., color=group), 
                         position=position_dodge(width = 0.5)) + 
  xlab('Evidence') + ylab('Strength of Case -\nPunishmnet Correlation') +
  group_color_scale +
  evidence_plus_baseline_x_axis +
  labs(title="C", size=rel(3)) +
  geom_vline(xintercept=1.5, colour='grey') +
  geom_vline(xintercept=2.5, colour='grey') +
  geom_vline(xintercept=3.5, colour='grey') +
  geom_vline(xintercept=4.5, colour='grey') +
  geom_vline(xintercept=5.5, colour='grey') +
  th + 
  theme(
    axis.text.x = element_text(hjust = 0.5, size=rel(1), color='black'),
    legend.position=c(1.25, 1)
  )

############### Combine into a single figure ##################################
# make a list of panels
plt_list <- list(plt_1, plt_2, plt_3)

# convert to grobs
grob_list <- lapply(plt_list, ggplotGrob)

# make sure axes align
max_heights <- do.call(unit.pmax, lapply(grob_list, function(x) {x$heights}))
grob_list <- lapply(grob_list, function(x) {x$heights <- max_heights; x})

# arrange with differing widths
lay <- rbind(c(1,2),
             c(3, 3))
plt_all <- do.call(arrangeGrob, c(grob_list, ncol=3, layout_matrix=list(lay),
                                  widths=list(c(1.1, 0.5, 1.25))))

# save to disk
ggsave('figure_paper_4.pdf', plot=plt_all, width=11, height=8.5, units='in', useDingbats=FALSE)