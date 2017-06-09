# third figure from the paper
library(tidyr)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(gtable)
library(gridBase)
library(broom)

color_genpop='#0656A3'
color_lawstudents='#d95f02'
color_lsba ='#7570b3'
color_ilsa ='#9e331b'

color_outrage='#733238'
color_punish='#A69A60'
color_threat='#D9BF3D'
color_conf='#0656A3'

load('data/stan_hier_postprocess.rdata')

############### Panel 1: Effect sizes for confidence ##################################
# get evidence effects
fe <- effects %>% filter(variable == 'mu', evidence != 'baseline') %>%
      select(mean, evidence, X2.5., X97.5., group) %>%
      mutate(evidence=factor(evidence, levels=c("physicalDNA", 
                                                "physicalNon-DNA", 
                                                "witnessYes Witness", 
                                                "historyRelated", 
                                                "historyUnrelated")))

plt_1 <- ggplot(data=fe) +
  geom_hline(yintercept=0, colour='grey') +
  geom_pointrange(aes(x=evidence, y=mean, ymin=X2.5., ymax=X97.5., color=group), size=1.,
                  position=position_dodge(width = 0.5)) + 
  scale_x_discrete(breaks=c("physicalDNA", "physicalNon-DNA", "witnessYes Witness", "historyRelated", "historyUnrelated"), 
                   labels=c("DNA \nphysical \nevidence", 
                            "Non-DNA \nphysical \nevidence",  
                            "Witness \npresent", 
                            "Related \nprior crime", 
                            "Unrelated \nprior crime")) +
  scale_color_manual(values=c('mturk'=color_genpop,
                              'legal'=color_lawstudents,
                              'lsba'=color_lsba,
                              'ilsa'=color_ilsa),
                      name='Group',
                      breaks=c('legal', 'lsba', 'ilsa', 'mturk'),
                      labels=c('Law Students', 'Louisiana Bar', 'Illinois Prosecutors', 'mTurk')) +
  coord_cartesian(ylim=c(-10,60)) +
  labs(title="A") +
  ylab("Strength of Case (pooints)") +
  xlab("Evidence Effects") +
  geom_vline(xintercept=1.5, colour='grey') +
  geom_vline(xintercept=2.5, colour='grey') +
  geom_vline(xintercept=3.5, colour='grey') +
  geom_vline(xintercept=4.5, colour='grey') +
  theme(
    plot.margin=unit(c(5.5, 5.5, 5.5, 5.5), "points"),
    panel.grid=element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color="black"),
    axis.text.x = element_text(hjust = 0.5, size=rel(1), color='black'),
    axis.title.x = element_text(size=rel(1.5)),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(hjust = 1, size=rel(2.5), color='black'),
    axis.title.y = element_text(size=rel(1.5)),
    plot.title=element_text(size=20, vjust=2, hjust=0.5),
    legend.text = element_text(size=rel(1.5)),
    legend.title = element_text(size=rel(1.5)),
    legend.position='none')


############### Panel 2: Baseline effects ##################################
# get scenario effects
se <- effects %>% filter(variable == 'gamma', evidence == 'baseline') %>% 
                  select(scenario, mean, group) %>%
                  mutate(outcome='rating')

plt_2 <- ggplot(data = se, aes(x=group, y=mean)) +
  geom_hline(yintercept=0, colour='grey') +
  geom_boxplot(aes(color=group), lwd=1, fatten=1, outlier.size=0, outlier.stroke=0) +
  # geom_point(position=position_jitter(width=0.2), size=rel(2), aes(color=group), alpha=0.5) +
  scale_x_discrete(breaks=c('legal', 'lsba', 'ilsa', 'mturk'),
                      labels=c('Law Students', 'Louisiana Bar', 'Illinois Prosecutors', 'mTurk')) +
  scale_color_manual(values=c('mturk'=color_genpop,
                              'legal'=color_lawstudents,
                              'lsba'=color_lsba,
                              'ilsa'=color_ilsa),
                      name='Group',
                      breaks=c('legal', 'lsba', 'ilsa', 'mturk'),
                      labels=c('Law Students', 'Louisiana Bar', 'Illinois Prosecutors', 'mTurk')) +
  scale_fill_manual(values=c('mturk'=color_genpop,
                              'legal'=color_lawstudents,
                              'lsba'=color_lsba,
                              'ilsa'=color_ilsa)) +
  xlab("Baseline\nEffect") +
  coord_cartesian(ylim=c(-10,60)) +
  labs(title="B", size=rel(3)) +
  ylab("Confidence") +
  theme(
    plot.margin=unit(c(5.5, 20, 5.5, 5.5), "points"),
    panel.grid=element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color="black"),
    axis.line.y = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_text(size=rel(1.5)),
    axis.ticks.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title=element_text(size=20,vjust=2, hjust=0.5),
    legend.text = element_text(size=rel(1.5)),
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.position=c(-0.5,1),
    legend.justification=c(0,1))
    

############### Panel 3: Illustration of range of confidences ##################################
# calculate mean rating for session and evidence combinations
sc_means <- dat %>% group_by(scenario, physical, history, witness, group) %>% 
                    summarise(rating=mean(rating)) %>%
                    arrange(scenario, physical, witness, history) %>%
                    ungroup()

# get unique variable code for each evidence combination
ev_codes <- sc_means %>% select(physical, witness, history) %>% 
                         distinct(physical, witness, history) %>%
                         mutate(code=as.factor(row_number()))

# add code column to scenario means
sc_means <- sc_means %>% merge(ev_codes) %>% arrange(code, scenario)

# calculate model prediction
# get mean values of coefficients across scenarios
betas <- effects %>% filter(variable == 'gamma') %>% 
         select(scenario, evidence, mean, group) %>%
         group_by(evidence, group) %>%
         summarise(effect=mean(mean)) %>%
         arrange(evidence)

# generate model matrix for predictions         
Xmat <- model.matrix(form, ev_codes)
colnames(Xmat)[1] <- 'baseline'
Xmat <- Xmat[, sort(colnames(Xmat), index.return=TRUE)$ix]

# remove baseline, calculate evidence
Xmat_no_baseline <- Xmat[,2:dim(Xmat)[2]]
betas_no_baseline <- betas %>% filter(evidence != 'baseline')

# mean evidence per code
pred_evidence <- betas_no_baseline %>% group_by(group) %>% 
                                       do(data.frame(code=ev_codes$code, 
                                             evidence=Xmat_no_baseline %*% .$effect)) 
# mean rating per code
mean_by_code <- dat %>% merge(ev_codes) %>% group_by(group, code) %>% 
                    summarise(code_mean=mean(rating)) %>%
                    ungroup()

# prediction dataframe
pred_evidence <- pred_evidence %>% merge(mean_by_code)

plt_3 <- ggplot(data=pred_evidence) +
  geom_point(aes(x=evidence, y=code_mean, color=group), size=3, alpha=0.5) +
  geom_smooth(aes(x=evidence, y=code_mean, color=group), method='lm', formula=y~x) +
  scale_color_manual(values=c('mturk'=color_genpop,
                              'legal'=color_lawstudents,
                              'lsba'=color_lsba,
                              'ilsa'=color_ilsa),
                      name='Group',
                      breaks=c('legal', 'lsba', 'ilsa', 'mturk'),
                      labels=c('Law Students', 'Louisiana Bar', 'Illinois Prosecutors', 'mTurk')) +
  xlab("Weight of Model \nEvidence (points)") +
  coord_cartesian(xlim=c(0, 85), ylim=c(0,100)) +
  labs(title="C", size=rel(3)) +
  ylab("Strength of Case (observed)") +
  theme(
    plot.margin=unit(c(5.5, 5.5, 5.5, 5.5), "points"),
    panel.grid=element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color="black"),
    axis.text.x = element_text(hjust = 0.5, size=rel(2), color='black'),
    axis.title.x = element_text(size=rel(1.5)),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(hjust = 1, size=rel(2.5), color='black'),
    axis.title.y = element_text(size=rel(1.5)),
    plot.title=element_text(size=20, vjust=2, hjust=0.5),
    legend.position='none')

############### Combine into a single figure ##################################
# make a list of panels
plt_list <- list(plt_1, plt_2, plt_3)

# convert to grobs
grob_list <- lapply(plt_list, ggplotGrob)

# make sure axes align
max_heights <- do.call(unit.pmax, lapply(grob_list, function(x) {x$heights}))
grob_list <- lapply(grob_list, function(x) {x$heights <- max_heights; x})

# arrange with differing widths
plt_all <- do.call(arrangeGrob, c(grob_list, ncol=3, widths=list(c(1.1, 0.5, 1.25))))

# save to disk
ggsave('figure_paper_3.pdf', plot=plt_all, width=11, height=4.5, units='in', useDingbats=FALSE)
