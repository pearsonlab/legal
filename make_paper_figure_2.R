# second figure from the paper
library(tidyr)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(gtable)
library(gridBase)

source('ggplot_setup.R')
load('data/stan_hier_postprocess.rdata')
effects <- effects %>% filter(group=='mturk')
dat <- dat %>% filter(group=='mturk')

############### Panel 1: Effect sizes for confidence ##################################
# get evidence effects
fe <- effects %>% filter(variable == 'mu', evidence != 'baseline') %>%
      select(mean, evidence, X2.5., X97.5.) %>%
      mutate(evidence=factor(evidence, levels=c("physicalDNA", 
                                                "physicalNon-DNA", 
                                                "witnessYes Witness", 
                                                "historyRelated", 
                                                "historyUnrelated")))

plt_1 <- ggplot(data=fe) +
  geom_pointrange(aes(x=evidence, y=mean, ymin=X2.5., ymax=X97.5.), color=color_conf, size=1.) + 
  evidence_x_axis +
  coord_cartesian(ylim=c(0,40)) +
  labs(title="A") +
  ylab("Case Strength (points)") +
  xlab("Evidence Effects") +
  geom_vline(xintercept=1.5, colour='grey') +
  geom_vline(xintercept=2.5, colour='grey') +
  geom_vline(xintercept=3.5, colour='grey') +
  geom_vline(xintercept=4.5, colour='grey') +
  th +
  theme(
    axis.text.x = element_text(hjust = 0.5, size=rel(1), color='black'))


############### Panel 2: Crime effects ##################################
# get scenario effects
se <- effects %>% filter(variable == 'gamma', evidence == 'baseline') %>% 
                  select(scenario, mean, group) %>%
                  mutate(outcome='rating')

plt_2 <- ggplot(data = se, aes(x=group, y=mean)) +
  geom_boxplot(aes(color=group), lwd=1, fatten=2.5, outlier.size=0, outlier.stroke=0) +
  geom_point(position=position_jitter(width=0.2), size=rel(4), aes(color=group), alpha=0.5) +
  group_x_axis +
  group_color_scale +
  group_fill_scale +
  xlab("Crime Effect") +
  coord_cartesian(ylim=c(0,40)) +
  labs(title="B", size=rel(3)) +
  ylab("Confidence") +
  th +
  theme(
    plot.margin=unit(c(5.5, 20, 5.5, 5.5), "points"),
    axis.line.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank())

############### Panel 3: Illustration of range of confidences ##################################
# calculate mean rating for session and evidence combinations
sc_means <- dat %>% group_by(scenario, physical, history, witness) %>% 
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
         select(scenario, evidence, mean) %>%
         group_by(evidence) %>%
         summarise(effect=mean(mean)) %>%
         arrange(evidence)

# generate model matrix for predictions         
Xmat <- model.matrix(form, ev_codes)
colnames(Xmat)[1] <- 'baseline'
Xmat <- Xmat[, sort(colnames(Xmat), index.return=TRUE)$ix]

# remove baseline, calculate evidence
Xmat_no_baseline <- Xmat[,2:dim(Xmat)[2]]
betas_no_baseline <- betas %>% filter(evidence != 'baseline')

# mean evidence per scenario
pred_evidence <- data.frame(code=ev_codes$code, 
                            evidence=Xmat_no_baseline %*% betas_no_baseline$effect) %>%
                 merge(sc_means) 

# mean rating per scenario
mean_by_scenario <- dat %>% group_by(scenario) %>% 
                    summarise(sc_mean=mean(rating)) %>%
                    ungroup()

# prediction dataframe
pred_evidence <- pred_evidence %>% merge(mean_by_scenario)

plt_3 <- ggplot(data=pred_evidence) +
  geom_point(aes(x=evidence, y=rating, color=sc_mean), size=3, alpha=0.5) +
  geom_smooth(aes(x=evidence, y=rating), color=color_conf, method='lm', formula=y~x) +
  xlab("Weight of Model \nEvidence (points)") +
  coord_cartesian(xlim=c(-5, 70), ylim=c(0,100)) +
  labs(title="C", size=rel(3)) +
  ylab("Case Strength (observed)") +
  th

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
ggsave('figure_paper_2.pdf', plot=plt_all, width=13, height=4.5, units='in', useDingbats=FALSE)
