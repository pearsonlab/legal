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
                                              "rate_threat",
                                              "rate_threat_2"))) %>%
     filter(outcome != "rate_threat")

plt_1 <- ggplot(data=fe) +
  geom_hline(yintercept=0, colour='grey') +
  geom_pointrange(aes(x=evidence, y=mean, ymin=X2.5., ymax=X97.5., color=outcome), size=1.,
                  position=position_dodge(width = 0.75)) + 
  evidence_x_axis +
  outcome_color_scale +
  coord_cartesian(ylim=c(0,100)) +
  labs(title="A") +
  ylab("Effect Size (points)") +
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
                                              "rate_threat",
                                              "rate_threat_2"))) %>%
     filter(outcome != "rate_threat")

plt_2 <- ggplot(data = se, aes(x=outcome, y=mean)) +
  geom_hline(yintercept=0, colour='grey') +
  geom_boxplot(aes(color=outcome), lwd=1, fatten=1, outlier.size=0, outlier.stroke=0) +
  outcome_x_axis +
  outcome_color_scale +
  xlab("Crime Effect") +
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
    legend.position=c(-0.5,1),
    legend.justification=c(0,1))
    
############### Panel 3: Punishment and case strength effect correlations ##################################
outcomes <- levels(effects$outcome)
Nr <- length(outcomes)
releveler <- function(x) {
  # replace outcome numbers with names
  xfac <- factor(x, levels=1:length(outcomes), labels=outcomes)
  
  # now reorder levels so plot looks right
  factor(as.character(xfac), levels=c("rating", "rate_punishment", "rate_outrage", "rate_threat", "rate_threat_2"))
}
corrs <- effects %>% filter(grepl('rho', variable)) %>% 
                     separate(variable, into=c("variable", "outcome1", "outcome2")) %>%
                     mutate_at(c("outcome1", "outcome2"), releveler) %>%
                     filter(evidence=='baseline') %>% 
                     filter(outcome1 != "rate_threat", outcome2 != "rate_threat") %>%
                     unite(col=contrast, outcome1, outcome2, sep='-') %>%
                     mutate(contrast=factor(contrast, levels=outcome_corr_levels,
                                                      labels=outcome_corr_labels))

plt_3 <- ggplot(data = corrs) +
  geom_hline(yintercept=0, colour='grey') +
  geom_pointrange(aes(x=contrast, y=X50., ymin=X2.5., ymax=X97.5.)) + 
  ylab('Crime Effect Correlation') +
  xlab('Outcome Pair') +
  labs(title="C", size=rel(3)) +
  coord_cartesian(ylim=c(-1,1)) +
  th +
  theme(
    axis.text.x = element_text(hjust = 0.5, size=rel(0.8), color='black')
    # axis.title.x = element_blank()
  )
  

############### Panel 4: Punishment and case strength effect correlations ##################################
load('data/stan_hier_postprocess_multi.rdata')

plt_4 <- ggplot(data=(effects %>% filter(variable=='rho', evidence=='baseline'))) +
  geom_hline(yintercept=0, colour='grey') +
  geom_pointrange(aes(x=evidence, y=X50., ymin=X2.5., ymax=X97.5., color=group), 
                         position=position_dodge(width = 0.5)) + 
  xlab('Evidence') + ylab('\nConfidence in Guilt /\nPunishmnet Correlation') +
  group_color_scale +
  evidence_plus_baseline_x_axis +
  labs(title="D", size=rel(3)) +
  th + 
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    plot.margin=unit(c(25.5, 100.5, 25.5, 0), "points"),
    legend.position=c(1.1, 0.8)
  )

############### Combine into a single figure ##################################
# make a list of panels
plt_list <- list(plt_1, plt_2, plt_3, plt_4)

# convert to grobs
grob_list <- lapply(plt_list, ggplotGrob)

# make sure axes align
max_heights <- do.call(unit.pmax, lapply(grob_list, function(x) {x$heights}))
grob_list <- lapply(grob_list, function(x) {x$heights <- max_heights; x})

# arrange with differing widths
lay <- rbind(c(1, 1, 2, NA),
             c(3, 3, 4, 4))
plt_all <- do.call(arrangeGrob, c(grob_list, ncol=4, layout_matrix=list(lay),
                                  widths=list(c(0.55, 0.55, 0.4, 0.4))))

# save to disk
ggsave('figure_paper_4.pdf', plot=plt_all, width=13, height=10.5, units='in', useDingbats=FALSE)
