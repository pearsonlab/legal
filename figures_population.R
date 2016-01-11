#1A, 1B, 2A, 2B, 3A, 3B, 3C
#1A, 3B, 3C

library(dplyr)
library(tidyr)
library(ggplot2)

load('data/fit_mt_ls_conf_pun.obj')
load('data/dat_rating_comb.rdata')
load('data/dat_rating_pun_comb.rdata')
load('data/dat_ipls.rdata')

fitobj <- fit_ms_ls_conf_pun
source('process_fits.R')

# dfpop <- fixed_effects[,c(1:6)]
# rownames(dfpop) <- seq(length=nrow(dfpop)) 
# dfpopsc <- dfpop[grepl('scenario',dfpop$factor),]
# 
# dfpopconf <- dfpopsc[dfpopsc$outcome=="rating",]
# dfpoppun <- dfpopsc[dfpopsc$outcome=="rate_punishment",]
# 
# ev_vars <- c("physical1","physical2","history1","history2","witness1")
# dfev <- dfpop[dfpop$factor %in% ev_vars,]
# 
# dfevconf <- dfev[dfev$outcome=="rating",]
# dfevconfgen <- dfevconf[dfevconf$predictor=="groupgenpop",]
# dfevconfleg <- dfevconf[dfevconf$predictor=="grouplegal",]
# 
# # confidence
# 
# dfpopconfgen <- dfpopconf[dfpopconf$predictor=="groupgenpop",]
# dfpopconfleg <- dfpopconf[dfpopconf$predictor=="grouplegal",]
# 
# sorted <- dfpopconfgen[order(dfpopconfgen$post.mean),]
# sorted["order"]<-seq(length=nrow(sorted))
# 
# dfpopconfsort <- merge(sorted, dfpopconfleg, by="factor")
# 
# dfpopconfsort <- dfpopconfsort[order(dfpopconfsort$order),]
# 
# dfpopconfsort$outcome.x<-NULL
# dfpopconfsort$outcome.y<-NULL
# 
# dfpopconfsortb<-dfpopconfsort[c(1,3,6,8)]
# colnames(dfpopconfsortb) <- c("scenario","genpop","order","lspop")

################## FIGURE 1B - MODEL FIT TO OBSERVED RESULTS

# here, we want to make a boxplot of confidence ratings for each combination of evidence variables
# and each scenario
# we also want to be able to plot a model prediction on top, along with data for each scenario

# 1) Get a prediction of confidence from the model for each scenario and variable combo
vars <- c('scenario', 'physical', 'history', 'witness')

# in this next bit, we'll figure out what the levels for each of the above variables is
# we will pass all these to expand.grid to get all the combinations
flist <- list()
for (v in vars) {
  this_fac <- fixed_effects %>% filter(outcome == 'rating', 
                                                predictor.1 == 'groupgenpop', 
                                                grepl(v, predictor.2)) %>% 
    select(predictor.2) %>% transmute(variable=as.character(predictor.2))
  
  # for variables other than scenario, we need to add level 0
  if (v != 'scenario') {
    this_fac <- rbind(paste(v, '0', sep=''), this_fac)
  }
  this_fac <- this_fac %>% transmute(variable=as.factor(variable))
  
  # change the name back to what it should be
  names(this_fac) <- v
  
  # append
  flist <- c(flist, this_fac)
}

# get a dataframe with all combos of factors
preds <- expand.grid(flist)

# convert that dataframe to a matrix
form <- ~ -1 + scenario + physical + history + witness
X <- model.matrix(form, data=preds)
beta <- fixed_effects %>% filter(outcome == 'rating',
                                 predictor.1 == 'groupgenpop') %>%
  select(post.mean) %>% rename(pred=post.mean)

# get predictions for each scenario and add to preds dataframe
pred.means <- as.matrix(X) %*% as.matrix(beta)
preds <- cbind(preds, pred.means)

# 2) make a dataframe of the average rating per combo of variables and scenario
dat_per_scen <- dat_rating_comb %>% group_by(physical, history, witness, scenario) %>%
  summarise(mean=mean(rating, na.rm=TRUE)) %>% ungroup()
for (v in c('physical', 'history', 'witness', 'scenario')) {
  dat_per_scen[[v]] <- factor(as.integer(dat_per_scen[[v]]), labels=levels(preds[[v]]))
}

# 3) merge this with the prediction dataframe
preds <- merge(preds, dat_per_scen)

# 4) now combine evidence variables into a single column, reorder this variable by its median
# value across scenarios, and drop unneeded columns
preds <- preds %>% mutate(combo=paste(physical, witness, history, sep="")) %>%
  group_by(combo) %>% mutate(median=median(pred)) %>% ungroup() %>%
  mutate(combo=as.factor(combo)) %>%
  select(scenario, combo, pred, median, mean) 

# 5) plot boxplots and datapoints of mean values for each combination of scenario and evidence variables
# plot symbols for the median predicted value of the evidence combination across scenarios
plt <- ggplot() +
  geom_boxplot(data=preds, aes(x=combo, y=mean), outlier.colour = NA) +
  geom_point(data=preds, aes(x=combo, y=mean),position=position_jitter(width=0.1), color="grey") +
  geom_point(data=preds, aes(x=combo, y=median), shape=5, color="red", size=6) +
  theme(
    panel.grid=element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color="black"),
    legend.position=c(1,1),
    legend.justification=c(1,1))

################### FIGURE 2A - EVIDENCE EFFECTS ON CONFIDENCE IN GUILT

fe <- fixed_effects %>% filter(outcome == 'rating', predictor.2 %in% ev_vars) %>%
  rename(group=predictor.1, predictor=predictor.2) %>%
  mutate(predictor=factor(predictor, levels=c('physical2', 'physical1','witness1','history2','history1')))

plt_ev <- ggplot(data = fe) +
  geom_pointrange(aes(x=predictor, y=post.mean, ymin=l95.CI, ymax=u95.CI, 
                      color=group), size=1.75, position=position_jitter(w=0.15)) + 
  scale_x_discrete(breaks=c("history1","history2","physical1","physical2","witness1"), 
                   labels=c("Unrelated \nprior crime", "Related \nprior crime", "Non-DNA \nphysical \nevidence", "DNA \nphysical \nevidence", "Witness \npresent")) +
  coord_cartesian(ylim=c(0,50)) +
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

########## FIGURE 2B - MODEL FIT TO OBSERVED CONFIDENCE
# calculate the evidence weight for each combination of variables
vars <- c('scenario', 'physical', 'history', 'witness')

flist <- list()
for (v in vars) {
  this_fac <- fixed_effects %>% filter(outcome == 'rating', 
                                                predictor.1 == 'groupgenpop', 
                                                grepl(v, predictor.2)) %>% 
    select(predictor.2) %>% transmute(variable=as.character(predictor.2))
  
  # for variables other than scenario, we need to add level 0
  if (v != 'scenario') {
    this_fac <- rbind(paste(v, '0', sep=''), this_fac)
  }
  this_fac <- this_fac %>% transmute(variable=as.factor(variable))
  
  # change the name back to what it should be
  names(this_fac) <- v
  
  # append
  flist <- c(flist, this_fac)
}
grpdf <- data.frame(group=levels(fixed_effects$predictor.1))
preds <- expand.grid(c(grpdf, flist))

# convert that dataframe to a matrix
form <- ~ -1 + group:(scenario + physical + history + witness)
X <- model.matrix(form, data=preds)

# get beta, setting all scenario effects to 0, so we only add evidence
beta <- fixed_effects %>% filter(outcome == 'rating') %>%
  mutate(pred=replace(post.mean, grepl('scenario', predictor.2), 0)) %>%
  select(pred)

# get predictions for each scenario and add to preds dataframe
pred.means <- as.matrix(X) %*% as.matrix(beta)
preds <- cbind(preds, pred.means)

# get mean prediction across all scenarios
preds_mean <- preds %>% group_by(group, physical, history, witness) %>%
  summarise(mean_pred=mean(pred))

# get observed mean across all scenarios
cols_to_keep <- c('scenario', 'physical', 'history', 'witness', 'group', 'rating')
dat_all <- rbind(dat_rating_comb %>% mutate(group='groupgenpop') %>%
                   select(one_of(cols_to_keep)), 
                 dat_ipls %>% mutate(group='grouplegal') %>% 
                   select(one_of(cols_to_keep)))
dat_summary <- dat_all %>% group_by(group, physical, history, witness) %>%
  summarise(mean_obs=mean(rating, na.rm=TRUE)) 
for (v in c('physical', 'history', 'witness')) {
  dat_summary[[v]] <- factor(as.integer(dat_summary[[v]]), labels=levels(preds_mean[[v]]))
}

# merge into a single dataframe
preds_mean <- merge(preds_mean, dat_summary)

plt <- ggplot(data=preds_mean) +
  geom_point(aes(x=mean_pred, y=mean_obs, color=group),size=3) +
  geom_smooth(aes(x=mean_pred, y=mean_obs, color=group), method='lm', formula=y~x) +
  scale_color_discrete(labels=c("Law students", "mTurk")) +
  xlim(0,100) +
  ylim(0,100) +
  theme(
    panel.grid=element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color="black"),
    legend.position=c(1,1),
    legend.justification=c(1,1))


##### FIGURE 3A BASELINE CONFIDENCE IN GUILT VARIES BY SCENARIO
se <- fixed_effects %>% filter(outcome == 'rating',
                               grepl('scenario', predictor.2))
plt <- ggplot() +
  geom_boxplot(data=se, aes(x=predictor.1, y=post.mean), outlier.colour = NA) +
  geom_point(data=se, aes(x=predictor.1, y=post.mean), 
             position=position_jitter(width=0.1), size=3, color="grey") +
  theme(
    panel.grid=element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color="black"),
    legend.position=c(1,1),
    legend.justification=c(1,1))





########### make some plots ###########
# evidence effects

color_outrage='#733238'
color_punish='#A69A60'
color_threat='#D9BF3D'
color_conf='#0656A3'

pred_vars <- levels(fixed_effects$predictor)
sc_vars <- pred_vars[grepl('scenario', pred_vars)]
ev_vars <- setdiff(pred_vars, sc_vars)

se <- fixed_effects[fixed_effects$predictor %in% sc_vars,]
se <- order_scenarios(se, 'rating')





#=== broken down plots

se_noconf<-subset(se,se$outcome!="rating")

plt_se_noconf <- ggplot(data = se_noconf, aes(x=outcome, y=post.mean))+
  geom_boxplot(aes(stat="boxplot", color=factor(outcome)),lwd=1,fatten=2.5)+
  geom_point(position=position_jitter(width=0.01), size=rel(4), aes(color=factor(outcome),alpha=0.5))+
  scale_x_discrete(breaks=c("rate_outrage","rate_punishment","rate_threat"), labels=c("Outrage", "Punishment", "Threat"))+
  scale_color_manual(values=c('rate_outrage'=color_outrage, 'rate_punishment'=color_punish, 'rate_threat'=color_threat))+
  scale_fill_manual(values=c('rate_outrage'=color_outrage, 'rate_punishment'=color_punish, 'rate_threat'=color_threat))+
  xlab("")+
  coord_cartesian(ylim=c(0,100))+
  labs(title="A", size=rel(3))+
  ylab("Baseline rating (points)")+
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


se_confonly<-subset(se,se$outcome=="rating")

plt_se_confonly <- ggplot(data = se_confonly, aes(x=outcome, y=post.mean))+
  geom_boxplot(aes(stat="boxplot", fill=factor(outcome), alpha=0.5))+
  geom_point(position=position_jitter(width=0.01), size=rel(3), aes(color=factor(outcome),alpha=0.5))+
  scale_x_discrete(breaks=c("rating"), labels=c("Scenario\n\n"))+
  scale_color_manual(values=c('rating'=color_conf))+
  scale_fill_manual(values=c('rating'=color_conf))+
  xlab("")+
  coord_cartesian(ylim=c(0,40))+
  labs(title="B")+
  ylab("Baseline rating")+
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

pltgrp<-
  arrangeGrob(plt_ev, plt_se_confonly, ncol=2 ,widths=c(3,1))

ggsave(plt_ev, file="ev_lawstudents.pdf", width=12, height=7, units='in', useDingbats=FALSE)




#covariance matrix plots


corr_rand<-cov2cor(random_effects$post.mean)
colnames(corr_rand)<-c('Confidence','Threat','Outrage','Punishment')
rownames(corr_rand)<-c('Confidence','Threat','Outrage','Punishment')

pdf("random_effects_covmatrix.pdf", width=7, height=7)
corrplot.mixed(corr_rand, lower='ellipse', upper='number')
dev.off()

corr_resid<-cov2cor(residual_effects$post.mean)
colnames(corr_resid)<-c('Confidence','Threat','Outrage','Punishment')
rownames(corr_resid)<-c('Confidence','Threat','Outrage','Punishment')

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

library(corrplot)

pdf("figure4_corrmatrix.pdf", width=5, height=5)
corrplot.mixed(cor(dfcorr3), lower='ellipse', upper='number')
dev.off()

# combined plots correlations

pltgrp2<-
  arrangeGrob(plt_se_noconf, plt_se_noconf, ncol=2 ,widths=c(1,1))

ggsave(pltgrp2, file="figure3_combined.pdf", width=16, height=8, units='in', useDingbats=FALSE)


#=== 12/5/2015

dfpopsc
dfpopscgen <- dfpopsc[dfpopsc$predictor=="groupgenpop",]
dfpopscgenrate <- dfpopscgen[dfpopscgen$outcome=="rating",]
dfpopscgenrate[,3]<-c(1:33)
dfpopscgenrate<-dfpopscgenrate[,3:6]

dfpopscleg <- dfpopsc[dfpopsc$predictor=="grouplegal",]
dfpopsclegrate <- dfpopscleg[dfpopscleg$outcome=="rating",]
dfpopsclegrate[,3]<-c(1:33)
dfpopsclegrate<-dfpopsclegrate[,3:6]


#=====
load('./rdata/dat_rating_pun_comb.rdata')
load('./rdata/dat_ipls.rdata')

#=== genpop

sc_pun_gen <- aggregate(rate_punishment ~ scenario, FUN=mean, data=dat_rating_pun_comb)
sc_pun_gen_ord<-sc_pun_gen[order(sc_pun_gen[,2]),]

#sc_pun_low<-c(3,6,27,1,2,25,13,19)
#sc_pun_high<-c(26,32,7,11,10,28,23,16)

sc_pun_gen_low<-as.data.frame(head(sc_pun_gen_ord[,1],8))
sc_pun_gen_high<-as.data.frame(tail(sc_pun_gen_ord[,1],8))

beta_gen_low<-dfpopscgenrate[(dfpopscgenrate[,1] %in% sc_pun_gen_low[,1]),]
beta_gen_high<-dfpopscgenrate[(dfpopscgenrate[,1] %in% sc_pun_gen_high[,1]),]

#=== lspop

sc_pun_leg <- aggregate(rate_punishment ~ scenario, FUN=mean, data=dat_ipls)
sc_pun_leg_ord<-sc_pun_leg[order(sc_pun_leg[,2]),]

#leg low: (27,6,1,3,25,2,15,4)
#leg high: (21,32,20,16,26,10,28,23)

sc_pun_leg_low<-as.data.frame(head(sc_pun_leg_ord[,1],8))
sc_pun_leg_high<-as.data.frame(tail(sc_pun_leg_ord[,1],8))

beta_leg_low<-dfpopsclegrate[(dfpopsclegrate[,1] %in% sc_pun_leg_low[,1]),]
beta_leg_high<-dfpopsclegrate[(dfpopsclegrate[,1] %in% sc_pun_leg_high[,1]),]

beta_gen_low[5]<-'GenLowPun'
beta_gen_high[5]<-'GenHighPun'
beta_leg_low[5]<-'LegalLowPun'
beta_leg_high[5]<-'LegalHighPun'

beta_pun_comb<-rbind(beta_gen_low,beta_gen_high,beta_leg_low,beta_leg_high)

colnames(beta_pun_comb)<-c('scenario','conf','low95','up95','group')

library(ggplot2)

pd <- position_dodge(width=0.1)

ggplot(data=beta_pun_comb, aes(x=group, y=conf))+
  geom_errorbar(aes(ymin=low95, ymax=up95), width=.2, position=pd)+
  geom_point(size=2)+
  geom_text(aes(label=scenario,hjust=0,vjust=0))


  
    geom_errorbar(aes(ymin=low95, ymax=up95), width=.1, position=pd) 

  geom_line(position=pd) +
  geom_point(position=pd)

plt
