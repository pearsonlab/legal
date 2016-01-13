# third figure from the grant application

library(ggplot2)
library(gridExtra)
library(dplyr)
library(gtable)

# load fit object containing mturkers only
load('data/fit.rdata')
load('data/dat_rating_comb.rdata')

color_outrage='#733238'
color_punish='#A69A60'
color_threat='#D9BF3D'
color_conf='#0656A3'

# change name of fit object and do some basic processing
fitobj <- fit
source('process_fits.R')

############### Panel 1: Effect sizes for confidence ##################################
# get evidence effects
fe <- fixed_effects %>% filter(predictor.1 %in% ev_vars) %>%
  mutate(predictor=factor(predictor.1, levels=c('physical2', 'physical1','witness1','history2','history1')))

plt_1 <- ggplot(data=fe) +
  geom_pointrange(aes(x=predictor, y=post.mean, ymin=l95.CI, ymax=u95.CI, color=outcome), 
                  size=1., position=position_jitter(w=0.15)) + 
  scale_x_discrete(breaks=c("history1","history2","physical1","physical2","witness1"), 
                   labels=c("Unrelated \nprior crime", "Related \nprior crime", 
                            "Non-DNA \nphysical \nevidence", "DNA \nphysical \nevidence", 
                            "Witness \npresent")) +
  scale_color_manual(values=c('rating'=color_conf,'rate_punishment'=color_punish,
                              'rate_threat'=color_threat,'rate_outrage'=color_outrage),
                      name='Rating',
                      breaks=c('rating','rate_outrage','rate_punishment','rate_threat'),
                      labels=c('Confidence','Outrage','Punishment','Threat')) +
  coord_cartesian(ylim=c(0,100)) +
  labs(title="A") +
  ylab("Rating") +
  xlab("Evidence Effects") +
  geom_vline(xintercept=1.5, colour='grey') +
  geom_vline(xintercept=2.5, colour='grey') +
  geom_vline(xintercept=3.5, colour='grey') +
  geom_vline(xintercept=4.5, colour='grey') +
  theme(
    panel.grid=element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color="black"),
    axis.text.x = element_text(hjust = 0.5, size=rel(1), color='black'),
    axis.title.x = element_text(size=rel(1.5)),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(hjust = 1, size=rel(2.5), color='black'),
    axis.title.y = element_text(size=rel(1.5)),
    plot.title=element_text(size=20,vjust=2),
    legend.text = element_text(size=rel(1.5)),
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.position=c(1,1),
    legend.justification=c(1,1))

############### Panel 2: Baseline effects ##################################
# get scenario effects
se <- fixed_effects %>% filter(predictor.1 %in% sc_vars)

plt_2 <- ggplot(data = se, aes(x=outcome, y=post.mean)) +
  geom_boxplot(aes(stat="boxplot", color=factor(outcome)),lwd=1,fatten=2.5) +
  geom_point(position=position_jitter(width=0.01), size=rel(4), aes(color=factor(outcome),alpha=0.5)) +
  scale_x_discrete(breaks=c("rate_outrage","rate_punishment","rate_threat", "rating"), 
                   labels=c("Outrage", "Punishment", "Threat", "Confidence")) +
  scale_color_manual(values=c('rate_outrage'=color_outrage, 'rate_punishment'=color_punish, 
                              'rate_threat'=color_threat, 'rating'=color_conf)) +
  scale_fill_manual(values=c('rate_outrage'=color_outrage, 'rate_punishment'=color_punish, 
                             'rate_threat'=color_threat, 'rating'=color_conf)) +
  xlab("Baseline Effect") +
  coord_cartesian(ylim=c(0,100)) +
  labs(title="B", size=rel(3)) +
  ylab("Confidence") +
  theme(
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
    plot.title=element_text(size=20,vjust=2),
    legend.position='none')

############### Panel 3: Illustration of range of confidences ##################################
# calculate a correlation matrix between response types
library(corrplot)
se <- fixed_effects %>% filter(grepl('scenario', predictor.1))
df <- se %>% select(predictor.1, outcome, post.mean) %>% spread(outcome, post.mean)
names(df) <- c('Scenario', 'Confidence\nin guilt','Threat','Outrage','Deserved\n punishment')

# make a correlation matrix to be saved as pdf
plotname <- "figure3_corrmatrix.pdf"
pdf(plotname, width=5, height=5)
corrplot.mixed(cor(df %>% select(-Scenario)), lower='ellipse', upper='number')
dev.off()

plt_3 <- ggplot(df) + geom_blank() + 
  labs(title="C", size=rel(3)) +
  theme(plot.title=element_text(size=20,vjust=2),
        panel.background = element_blank())

############### Combine into a single figure ##################################
# make a list of panels
plt_list <- list(plt_1, plt_2)

# convert ggplot objects to grobs and standardize heights so axes match
grob_list <- standardize_heights(plt_list)
plt_all <- arrangeGrob(grob_list[[1]], grob_list[[2]], ggplotGrob(plt_3), ncol=3, widths=c(1.2, 0.75, 1))

# save to disk
ggsave('figure_grant_3.pdf', plot=plt_all, width=11, height=4.5, units='in', useDingbats=FALSE)
