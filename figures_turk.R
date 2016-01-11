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

#=== broken down plots

se <- fixed_effects %>% filter(predictor.1 %in% sc_vars)
se_noconf <- se %>% filter(outcome != 'rating')

plt_se_noconf <- ggplot(data = se_noconf, aes(x=outcome, y=post.mean)) +
  geom_boxplot(aes(stat="boxplot", color=factor(outcome)),lwd=1,fatten=2.5) +
  geom_point(position=position_jitter(width=0.01), size=rel(4), aes(color=factor(outcome),alpha=0.5)) +
  scale_x_discrete(breaks=c("rate_outrage","rate_punishment","rate_threat"), 
                   labels=c("Outrage", "Punishment", "Threat")) +
  scale_color_manual(values=c('rate_outrage'=color_outrage, 'rate_punishment'=color_punish, 
                              'rate_threat'=color_threat)) +
  scale_fill_manual(values=c('rate_outrage'=color_outrage, 'rate_punishment'=color_punish, 
                             'rate_threat'=color_threat)) +
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

ggsave('figure3_scenario_effects_noconf.pdf', width=11, height=8.5, units='in', useDingbats=FALSE)


se_confonly <- se %>% filter(outcome == 'rating')

plt_se_confonly <- ggplot(data = se_confonly, aes(x=outcome, y=post.mean)) +
  geom_boxplot(aes(stat="boxplot", fill=factor(outcome), alpha=0.5)) +
  geom_point(position=position_jitter(width=0.01), size=rel(3), aes(color=factor(outcome),alpha=0.5)) +
  scale_x_discrete(breaks=c("rating"), labels=c("Scenario\n\n")) +
  scale_color_manual(values=c('rating'=color_conf)) +
  scale_fill_manual(values=c('rating'=color_conf)) +
  xlab("") +
  coord_cartesian(ylim=c(0,40)) +
  labs(title="B") +
  ylab("Baseline rating") +
  theme(
    panel.grid=element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color="black"),
    axis.text.x = element_text(hjust = 1, size=rel(2), color='black'),
    axis.text.y = element_text(hjust = 0, size=rel(2.5), color='black'),
    axis.title.y = element_text(size=rel(1.5)),
    plot.title=element_text(size=20,vjust=2),
    legend.position='none')

ggsave('figure3_scenario_effects_confonly.jpg', width=3, height=8.5, units='in')

#=== combined plots

pltgrp <- arrangeGrob(plt_ev, plt_se_confonly, ncol=2 ,widths=c(3,1))

ggsave(pltgrp, file="figure2_combined.pdf", width=12, height=7, units='in', useDingbats=FALSE)




#covariance matrix plots


corr_rand <- cov2cor(random_effects$post.mean)
colnames(corr_rand) <- c('Confidence','Threat','Outrage','Punishment')
rownames(corr_rand) <- c('Confidence','Threat','Outrage','Punishment')

pdf("random_effects_covmatrix.pdf", width=7, height=7)
corrplot.mixed(corr_rand, lower='ellipse', upper='number')
dev.off()

corr_resid <- cov2cor(residual_effects$post.mean)
colnames(corr_resid) <- c('Confidence','Threat','Outrage','Punishment')
rownames(corr_resid) <- c('Confidence','Threat','Outrage','Punishment')

pdf("residual_effects_covmatrix.pdf", width=7, height=7)
corrplot.mixed(corr_resid, lower='ellipse', upper='number')
dev.off()



#=== correlation

#dfconfthreat<-subset(se,outcome==c("rating","rate_threat"))

# pltcor<-ggplot(data=se,aes(x=predictor, y=post.mean))
# pltcor+
#   geom_point(aes(color=outcome))

df<-dcast(se, predictor~outcome, value.var="post.mean")

# pltcor<-ggplot(data=df,aes(x=rate_threat, y=rate_punishment))
# pltcor+
#   geom_point()

dfcorr<-dcast(se[c("outcome","predictor","post.mean")], predictor~outcome, value.var="post.mean")

thr_pun<-dfcorr[c("predictor","rate_threat","rate_punishment")]
thr_pun["contrast"]<-"Threat vs. Punishment"
names(thr_pun)<-c("scenario","x","y",'contrast')
  
thr_out<-dfcorr[c("predictor","rate_threat","rate_outrage")]
thr_out["contrast"]<-"Threat vs. Outrage"
names(thr_out)<-c("scenario","x","y",'contrast')

thr_con<-dfcorr[c("predictor","rating","rate_threat")]
thr_con["contrast"]<-"Confidence vs. Threat"
names(thr_con)<-c("scenario","x","y",'contrast')

pun_out<-dfcorr[c("predictor","rate_punishment","rate_outrage")]
pun_out["contrast"]<-"Punish vs. Outrage"
names(pun_out)<-c("scenario","x","y",'contrast')

pun_con<-dfcorr[c("predictor","rating", "rate_punishment")]
pun_con["contrast"]<-"Confidence vs. Punishment"
names(pun_con)<-c("scenario","x","y",'contrast')

out_con<-dfcorr[c("predictor","rating", "rate_outrage")]
out_con["contrast"]<-"Confidence vs. Outrage"
names(out_con)<-c("scenario","x","y",'contrast')

allcorr<-rbind(thr_pun, thr_out, thr_con, pun_out, pun_con, out_con)

ggplot(data=allcorr)+
  geom_point(aes(x=x, y=y, color=contrast))+
  stat_smooth(aes(x=x, y=y, color=contrast), method="lm")

ggsave('figure4_correlations.pdf', width=11, height=8.5, units='in')

dfcorr2<-dfcorr
dfcorr2$predictor<-NULL

dfcorr3<-dfcorr2[c('rating','rate_threat','rate_outrage','rate_punishment')]

names(dfcorr3)<-c('Confidence\n in guilt','Threat','Outrage','Deserved\n punishment')


pdf("figure4_corrmatrix.pdf", width=5, height=5)
corrplot.mixed(cor(dfcorr3), lower='ellipse', upper='number')
dev.off()

# combined plots correlations

pltgrp2<-
  arrangeGrob(plt_se_noconf, plt_se_noconf, ncol=2 ,widths=c(1,1))

ggsave(pltgrp2, file="figure3_combined.pdf", width=16, height=8, units='in', useDingbats=FALSE)