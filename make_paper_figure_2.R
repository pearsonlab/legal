# second figure from the paper
library(tidyr)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(gtable)
library(gridBase)

color_genpop='#0656A3'
color_lawstudents='#d95f02'
color_lsba ='#7570b3'
color_ilsa ='#9e331b'

color_outrage='#733238'
color_punish='#A69A60'
color_threat='#D9BF3D'
color_conf='#0656A3'

# get posterior means for effects
datfiles <- c('data/stan_model_output_hier_t_mturk.rdata')

renamer <- function(x) {
  gsub("\\[\\s*(\\d+)(,\\s*(\\d+))*\\s*\\]", "_\\3\\_\\1", x, perl=TRUE)
}
qprobs <- c(0.025, 0.5, 0.975)
eff_list <- list()
for (dd in 1:length(datfiles)) {
  load(datfiles[dd])
  
  # get matrix of summary statistics for each variable of interest
  ss <- data.frame(rstan::summary(fit, pars=c('mu', 'eta', 'gamma', 'tau', 'sigma'), probs=qprobs)$summary)
  
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
effects <- bind_rows(eff_list) %>% mutate(group=factor(group)) %>%
  mutate(scenario=factor(as.numeric(scenario)))

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
  scale_x_discrete(breaks=c("physicalDNA", "physicalNon-DNA", "witnessYes Witness", "historyRelated", "historyUnrelated"), 
                   labels=c("DNA \nphysical \nevidence", 
                            "Non-DNA \nphysical \nevidence",  
                            "Witness \npresent", 
                            "Related \nprior crime", 
                            "Unrelated \nprior crime")) +
  coord_cartesian(ylim=c(0,40)) +
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
                  select(scenario, mean) %>%
                  mutate(outcome='rating')

plt_2 <- ggplot(data = se, aes(x=outcome, y=mean)) +
  geom_boxplot(aes(color=factor(outcome)), lwd=1, fatten=2.5, outlier.size=0, outlier.stroke=0) +
  geom_point(position=position_jitter(width=0.2), size=rel(4), aes(color=factor(outcome),alpha=0.5)) +
  scale_x_discrete(breaks=c("rate_outrage","rate_punishment","rate_threat", "rating"), 
                   labels=c("Outrage", "Punishment", "Threat", "Confidence")) +
  scale_color_manual(values=c('rate_outrage'=color_outrage, 'rate_punishment'=color_punish, 
                              'rate_threat'=color_threat, 'rating'=color_conf)) +
  scale_fill_manual(values=c('rate_outrage'=color_outrage, 'rate_punishment'=color_punish, 
                             'rate_threat'=color_threat, 'rating'=color_conf)) +
  xlab("Baseline\nEffect") +
  coord_cartesian(ylim=c(0,40)) +
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
    legend.position='none')

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
ggsave('figure_paper_2.pdf', plot=plt_all, width=11, height=4.5, units='in', useDingbats=FALSE)
