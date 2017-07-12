# third figure from the paper
library(tidyr)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(gtable)
library(gridBase)
library(broom)

source('ggplot_setup.R')
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
                  position=position_dodge(width = 0.75)) + 
  evidence_x_axis +
  group_color_scale +
  coord_cartesian(ylim=c(-10,60)) +
  labs(title="A") +
  ylab("Case Strength (points)") +
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
                  select(scenario, mean, group) %>%
                  mutate(outcome='rating')

plt_2 <- ggplot(data = se, aes(x=group, y=mean)) +
  geom_hline(yintercept=0, colour='grey') +
  geom_boxplot(aes(color=group), lwd=1, fatten=1, outlier.size=0, outlier.stroke=0) +
  # geom_point(position=position_jitter(width=0.2), size=rel(2), aes(color=group), alpha=0.5) +
  group_x_axis +
  group_color_scale +
  group_fill_scale +
  xlab("Baseline\nEffect") +
  coord_cartesian(ylim=c(-10,60)) +
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
  group_color_scale +
  xlab("Weight of Model \nEvidence (points)") +
  coord_cartesian(xlim=c(0, 85), ylim=c(0,100)) +
  labs(title="C", size=rel(3)) +
  ylab("Case Strength (observed)") +
  th 

############### Panel 4: Evidence vs Baseline #################################
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

plt_4 <- ggplot(data=eff_slope_and_baseline) +
  geom_smooth(aes(x=baseline, y=slope), method=lm, color="black", fill="black") +
  geom_point(aes(x=baseline, y=slope, color=group), size=4) + 
  group_color_scale +
  xlab('Baseline') + ylab('Evidence') +
  labs(title="D", size=rel(3)) +
  th

############### Panel 5: Variance Comparison #################################
variance_comparison <- effects %>%
  filter(variable %in% c('eta', 'tau', 'sigma'), (evidence=='baseline') | (variable == 'sigma')) %>%
  select(X2.5., X50., X97.5., variable, scenario, group) %>%
  mutate(variable = factor(variable, levels=c('eta', 'tau', 'sigma')))

plt_5 <- ggplot() +
  geom_pointrange(data=variance_comparison %>% filter(variable %in% c('eta', 'sigma')),
                  aes(x=variable, y=X50., ymin=X2.5., ymax=X97.5., color=group),
                  position=position_dodge(width=0.5), size=1) +
  geom_boxplot(data=variance_comparison %>% filter(variable=='tau'),
               aes(x=variable, y=X50., color=group), lwd=1, fatten=1, 
               position=position_dodge(width=0.85)) +
  variance_x_axis +
  group_color_scale +
  ylim(0, 50) +
  ylab("Standard Deviation (points)") + 
  xlab("Variance") +
  labs(title="E", size=rel(3)) +
  th +
  theme(
    axis.text.x = element_text(hjust = 0.5, size=rel(1), color='black')
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
lay <- rbind(c(1, 1, 2),
             c(3, 4, NA))
plt_all <- do.call(arrangeGrob, c(grob_list, ncol=3, layout_matrix=list(lay),
                                  widths=list(c(0.55, 0.55, 0.3))))

# plt_all <- do.call(arrangeGrob, c(grob_list, ncol=3, layout_matrix=list(lay),
#                                   widths=list(c(1.1, 0.5, 1.25))))
# save to disk
# ggsave('figure_paper_3.pdf', plot=plt_all, width=11, height=4.5, units='in', useDingbats=FALSE)
ggsave('figure_paper_3.pdf', plot=plt_all, width=11, height=8.5, units='in', useDingbats=FALSE)
