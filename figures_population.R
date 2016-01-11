#1A, 1B, 2A, 2B, 3A, 3B, 3C
#1A, 3B, 3C

#library(reshape2)
library(dplyr)
library(tidyr)
library(ggplot2)
source('helpers.R')

load('data/fit_mt_ls_conf_pun.obj')
load('data/dat_rating_comb.rdata')
load('data/dat_rating_pun_comb.rdata')
load('data/dat_ipls.rdata')

fitobj <- fit_ms_ls_conf_pun
source('process_fits.R')

# give factor column a name
names(fixed_effects)[3]<-paste("factor")

dfpop <- fixed_effects[,c(1:6)]
rownames(dfpop) <- seq(length=nrow(dfpop)) 
dfpopsc <- dfpop[grepl('scenario',dfpop$factor),]

dfpopconf <- dfpopsc[dfpopsc$outcome=="rating",]
dfpoppun <- dfpopsc[dfpopsc$outcome=="rate_punishment",]

ev_vars <- c("physical1","physical2","history1","history2","witness1")
dfev <- dfpop[dfpop$factor %in% ev_vars,]

dfevconf <- dfev[dfev$outcome=="rating",]
dfevconfgen <- dfevconf[dfevconf$predictor=="groupgenpop",]
dfevconfleg <- dfevconf[dfevconf$predictor=="grouplegal",]

# confidence

dfpopconfgen <- dfpopconf[dfpopconf$predictor=="groupgenpop",]
dfpopconfleg <- dfpopconf[dfpopconf$predictor=="grouplegal",]

sorted <- dfpopconfgen[order(dfpopconfgen$post.mean),]
sorted["order"]<-seq(length=nrow(sorted))

dfpopconfsort <- merge(sorted, dfpopconfleg, by="factor")

dfpopconfsort <- dfpopconfsort[order(dfpopconfsort$order),]

dfpopconfsort$outcome.x<-NULL
dfpopconfsort$outcome.y<-NULL

dfpopconfsortb<-dfpopconfsort[c(1,3,6,8)]
colnames(dfpopconfsortb) <- c("scenario","genpop","order","lspop")

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
                                                predictor == 'groupgenpop', 
                                                grepl(v, factor)) %>% 
    select(factor) %>% transmute(variable=as.character(factor))
  
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
                                 predictor == 'groupgenpop') %>%
  select(post.mean) %>% rename(pred=post.mean)

# get predictions for each scenario and add to preds dataframe
pred.means <- as.matrix(X) %*% as.matrix(beta)
preds <- cbind(preds, pred.means)

# make a dataframe of the average rating per combo of variables and scenario
dat_per_scen <- dat_rating_comb %>% group_by(physical, history, witness, scenario) %>%
  summarise(mean=mean(rating, na.rm=TRUE)) %>% ungroup()
for (v in c('physical', 'history', 'witness', 'scenario')) {
  dat_per_scen[[v]] <- factor(as.integer(dat_per_scen[[v]]), labels=levels(preds[[v]]))
}

# merge this with the prediction dataframe
preds <- merge(preds, dat_per_scen)

# now combine evidence variables into a single column, reorder this variable by its median
# value across scenarios, and drop unneeded columns
preds <- preds %>% mutate(combo=paste(physical, history, witness, sep="")) %>%
  group_by(combo) %>% mutate(median=median(pred)) %>% ungroup() %>%
  mutate(combo=reorder(combo, median)) %>%
  select(scenario, combo, pred, median, mean) 

# plot boxplots and datapoints of mean values for each combination of scenario and evidence variables
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

#fe <- fixed_effects[fixed_effects$predictor %in% ev_vars,]
fe<-dfevconf
fe$factor <- factor(fe$factor, levels=c('physical2', 'physical1','witness1','history2','history1'))

plt_ev <- ggplot(data = fe) +
  geom_pointrange(aes(x=factor, y=post.mean, ymin=l95.CI, ymax=u95.CI, color=predictor), size=1.75, position=position_jitter(w=0.15)) + 
  scale_x_discrete(breaks=c("history1","history2","physical1","physical2","witness1"), 
                   labels=c("Unrelated \nprior crime", "Related \nprior crime", "Non-DNA \nphysical \nevidence", "DNA \nphysical \nevidence", "Witness \npresent"))+
  coord_cartesian(ylim=c(0,50))+
  labs(title="A")+
  ylab("Effect size (points above baseline)")+
  xlab("")+
  geom_vline(xintercept=1.5, colour='grey')+
  geom_vline(xintercept=2.5, colour='grey')+
  geom_vline(xintercept=3.5, colour='grey')+
  geom_vline(xintercept=4.5, colour='grey')+
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

dmg000 <- aggregate (cbind(rating) ~ scenario, 
                     data = subset(dat_rating_comb, 
                                   physical == 'No Physical' 
                                   & history == 'No History' 
                                   & witness == 'No Witness'),
                     FUN=mean, na.rm=TRUE)
dmg000mean <- mean(dmg000[0:nrow(dmg000),2])
dmg000beta <- 0

dmg001 <- aggregate (cbind(rating) ~ scenario, 
                     data = subset(dat_rating_comb, 
                                   (physical == 'No Physical') 
                                   & (history == 'No History') 
                                   & witness == 'Yes Witness'),
                     FUN=mean, na.rm=TRUE)
dmg001mean <- mean(dmg001[0:nrow(dmg001),2])
dmg001beta <- bw1

dmg010 <- aggregate (cbind(rating) ~ scenario, 
                     data = subset(dat_rating_comb, 
                                   (physical == 'No Physical') 
                                   & (history == 'Unrelated') 
                                   & witness == 'No Witness'),
                     FUN=mean, na.rm=TRUE)
dmg010mean <- mean(dmg010[0:nrow(dmg010),2])
dmg010beta <- bh1

dmg011 <- aggregate (cbind(rating) ~ scenario, 
                     data = subset(dat_rating_comb, 
                                   (physical == 'No Physical') 
                                   & (history == 'Unrelated') 
                                   & witness == 'Yes Witness'),
                     FUN=mean, na.rm=TRUE)
dmg011mean <- mean(dmg011[0:nrow(dmg011),2])
dmg011beta <- bh1 + bw1

dmg020 <- aggregate (cbind(rating) ~ scenario, 
                     data = subset(dat_rating_comb, 
                                   (physical == 'No Physical') 
                                   & (history == 'Related') 
                                   & witness == 'No Witness'),
                     FUN=mean, na.rm=TRUE)
dmg020mean <- mean(dmg020[0:nrow(dmg020),2])
dmg020beta <- bh2

dmg021 <- aggregate (cbind(rating) ~ scenario, 
                     data = subset(dat_rating_comb, 
                                   (physical == 'No Physical') 
                                   & (history == 'Related') 
                                   & witness == 'Yes Witness'),
                     FUN=mean, na.rm=TRUE)
dmg021mean <- mean(dmg021[0:nrow(dmg021),2])
dmg021beta <- bh2 + bw1

dmg100 <- aggregate (cbind(rating) ~ scenario, 
                     data = subset(dat_rating_comb, 
                                   (physical == 'Non-DNA') 
                                   & (history == 'No History') 
                                   & witness == 'No Witness'),
                     FUN=mean, na.rm=TRUE)
dmg100mean <- mean(dmg100[0:nrow(dmg100),2])
dmg100beta <- bp1

dmg101 <- aggregate (cbind(rating) ~ scenario, 
                     data = subset(dat_rating_comb, 
                                   (physical == 'Non-DNA') 
                                   & (history == 'No History') 
                                   & witness == 'Yes Witness'),
                     FUN=mean, na.rm=TRUE)
dmg101mean <- mean(dmg101[0:nrow(dmg101),2])
dmg101beta <- bp1 + bw1

dmg110 <- aggregate (cbind(rating) ~ scenario, 
                     data = subset(dat_rating_comb, 
                                   (physical == 'Non-DNA') 
                                   & (history == 'Unrelated') 
                                   & witness == 'No Witness'),
                     FUN=mean, na.rm=TRUE)
dmg110mean <- mean(dmg110[0:nrow(dmg110),2])
dmg110beta <- bp1 + bh1

dmg111 <- aggregate (cbind(rating) ~ scenario, 
                     data = subset(dat_rating_comb, 
                                   (physical == 'Non-DNA') 
                                   & (history == 'Unrelated') 
                                   & witness == 'Yes Witness'),
                     FUN=mean, na.rm=TRUE)
dmg111mean <- mean(dmg111[0:nrow(dmg111),2])
dmg111beta <- bp1 + bh1 + bw1

dmg120 <- aggregate (cbind(rating) ~ scenario, 
                     data = subset(dat_rating_comb, 
                                   (physical == 'Non-DNA') 
                                   & (history == 'Related') 
                                   & witness == 'No Witness'),
                     FUN=mean, na.rm=TRUE)
dmg120mean <- mean(dmg120[0:nrow(dmg120),2])
dmg120beta <- bp1 + bh2

dmg121 <- aggregate (cbind(rating) ~ scenario, 
                     data = subset(dat_rating_comb, 
                                   (physical == 'Non-DNA') 
                                   & (history == 'Related') 
                                   & witness == 'Yes Witness'),
                     FUN=mean, na.rm=TRUE)
dmg121mean <- mean(dmg121[0:nrow(dmg121),2])
dmg121beta <- bp1 + bh2 + bw1

dmg200 <- aggregate (cbind(rating) ~ scenario, 
                     data = subset(dat_rating_comb, 
                                   (physical == 'DNA') 
                                   & (history == 'No History') 
                                   & witness == 'No Witness'),
                     FUN=mean, na.rm=TRUE)
dmg200mean <- mean(dmg200[0:nrow(dmg200),2])
dmg200beta <- bp2

dmg201 <- aggregate (cbind(rating) ~ scenario, 
                     data = subset(dat_rating_comb, 
                                   (physical == 'DNA') 
                                   & (history == 'No History') 
                                   & witness == 'Yes Witness'),
                     FUN=mean, na.rm=TRUE)
dmg201mean <- mean(dmg201[0:nrow(dmg201),2])
dmg201beta <- bp2 + bw1

dmg210 <- aggregate (cbind(rating) ~ scenario, 
                     data = subset(dat_rating_comb, 
                                   (physical == 'DNA') 
                                   & (history == 'Unrelated') 
                                   & witness == 'No Witness'),
                     FUN=mean, na.rm=TRUE)
dmg210mean <- mean(dmg210[0:nrow(dmg210),2])
dmg210beta <- bp2 + bh1

dmg211 <- aggregate (cbind(rating) ~ scenario, 
                     data = subset(dat_rating_comb, 
                                   (physical == 'DNA') 
                                   & (history == 'Unrelated') 
                                   & witness == 'Yes Witness'),
                     FUN=mean, na.rm=TRUE)
dmg211mean <- mean(dmg211[0:nrow(dmg211),2])
dmg211beta <- bp2 + bh1 + bw1

dmg220 <- aggregate (cbind(rating) ~ scenario, 
                     data = subset(dat_rating_comb, 
                                   (physical == 'DNA') 
                                   & (history == 'Related') 
                                   & witness == 'No Witness'),
                     FUN=mean, na.rm=TRUE)
dmg220mean <- mean(dmg220[0:nrow(dmg220),2])
dmg220beta <- bp2 + bh2

dmg221 <- aggregate (cbind(rating) ~ scenario, 
                     data = subset(dat_rating_comb, 
                                   (physical == 'DNA') 
                                   & (history == 'Related') 
                                   & witness == 'Yes Witness'),
                     FUN=mean, na.rm=TRUE)
dmg221mean <- mean(dmg221[0:nrow(dmg221),2])
dmg221beta <- bp2 + bh2 + bw1

dmgconf <- as.data.frame(rbind (dmg000mean, dmg001mean, dmg010mean, dmg011mean, dmg020mean, dmg021mean,
                                dmg100mean, dmg101mean, dmg110mean, dmg111mean, dmg120mean, dmg121mean,
                                dmg200mean, dmg201mean, dmg210mean, dmg211mean, dmg220mean, dmg221mean))

dmgbeta <- as.data.frame(rbind (dmg000beta, dmg001beta, dmg010beta, dmg011beta, dmg020beta, dmg021beta,
                                dmg100beta, dmg101beta, dmg110beta, dmg111beta, dmg120beta, dmg121beta,
                                dmg200beta, dmg201beta, dmg210beta, dmg211beta, dmg220beta, dmg221beta))

dmg <- cbind(dmgconf, dmgbeta)
colnames(dmg)<-c('mt_mean_rating','mt_comb_beta')

#=== LS population

bp1ls<-44.0
bp2ls<-43.9
bh1ls<-3.01
bh2ls<-13.2
bw1ls<-29.4

lsg000 <- aggregate (cbind(rating) ~ scenario, 
                     data = subset(dat_ipls, 
                                   physical == 'No Physical' 
                                   & history == 'No History' 
                                   & witness == 'No Witness'),
                     FUN=mean, na.rm=TRUE)
lsg000mean <- mean(lsg000[0:nrow(lsg000),2])
lsg000beta <- 0

lsg001 <- aggregate (cbind(rating) ~ scenario, 
                     data = subset(dat_ipls, 
                                   (physical == 'No Physical') 
                                   & (history == 'No History') 
                                   & witness == 'Yes Witness'),
                     FUN=mean, na.rm=TRUE)
lsg001mean <- mean(lsg001[0:nrow(lsg001),2])
lsg001beta <- bw1ls

lsg010 <- aggregate (cbind(rating) ~ scenario, 
                     data = subset(dat_ipls, 
                                   (physical == 'No Physical') 
                                   & (history == 'Unrelated') 
                                   & witness == 'No Witness'),
                     FUN=mean, na.rm=TRUE)
lsg010mean <- mean(lsg010[0:nrow(lsg010),2])
lsg010beta <- bh1ls

lsg011 <- aggregate (cbind(rating) ~ scenario, 
                     data = subset(dat_ipls, 
                                   (physical == 'No Physical') 
                                   & (history == 'Unrelated') 
                                   & witness == 'Yes Witness'),
                     FUN=mean, na.rm=TRUE)
lsg011mean <- mean(lsg011[0:nrow(lsg011),2])
lsg011beta <- bh1ls + bw1ls

lsg020 <- aggregate (cbind(rating) ~ scenario, 
                     data = subset(dat_ipls, 
                                   (physical == 'No Physical') 
                                   & (history == 'Related') 
                                   & witness == 'No Witness'),
                     FUN=mean, na.rm=TRUE)
lsg020mean <- mean(lsg020[0:nrow(lsg020),2])
lsg020beta <- bh2ls

lsg021 <- aggregate (cbind(rating) ~ scenario, 
                     data = subset(dat_ipls, 
                                   (physical == 'No Physical') 
                                   & (history == 'Related') 
                                   & witness == 'Yes Witness'),
                     FUN=mean, na.rm=TRUE)
lsg021mean <- mean(lsg021[0:nrow(lsg021),2])
lsg021beta <- bh2ls + bw1ls

lsg100 <- aggregate (cbind(rating) ~ scenario, 
                     data = subset(dat_ipls, 
                                   (physical == 'Non-DNA') 
                                   & (history == 'No History') 
                                   & witness == 'No Witness'),
                     FUN=mean, na.rm=TRUE)
lsg100mean <- mean(lsg100[0:nrow(lsg100),2])
lsg100beta <- bp1ls

lsg101 <- aggregate (cbind(rating) ~ scenario, 
                     data = subset(dat_ipls, 
                                   (physical == 'Non-DNA') 
                                   & (history == 'No History') 
                                   & witness == 'Yes Witness'),
                     FUN=mean, na.rm=TRUE)
lsg101mean <- mean(lsg101[0:nrow(lsg101),2])
lsg101beta <- bp1ls + bw1ls

lsg110 <- aggregate (cbind(rating) ~ scenario, 
                     data = subset(dat_ipls, 
                                   (physical == 'Non-DNA') 
                                   & (history == 'Unrelated') 
                                   & witness == 'No Witness'),
                     FUN=mean, na.rm=TRUE)
lsg110mean <- mean(lsg110[0:nrow(lsg110),2])
lsg110beta <- bp1ls + bh1ls

lsg111 <- aggregate (cbind(rating) ~ scenario, 
                     data = subset(dat_ipls, 
                                   (physical == 'Non-DNA') 
                                   & (history == 'Unrelated') 
                                   & witness == 'Yes Witness'),
                     FUN=mean, na.rm=TRUE)
lsg111mean <- mean(lsg111[0:nrow(lsg111),2])
lsg111beta <- bp1ls + bh1ls + bw1ls

lsg120 <- aggregate (cbind(rating) ~ scenario, 
                     data = subset(dat_ipls, 
                                   (physical == 'Non-DNA') 
                                   & (history == 'Related') 
                                   & witness == 'No Witness'),
                     FUN=mean, na.rm=TRUE)
lsg120mean <- mean(lsg120[0:nrow(lsg120),2])
lsg120beta <- bp1ls + bh2ls

lsg121 <- aggregate (cbind(rating) ~ scenario, 
                     data = subset(dat_ipls, 
                                   (physical == 'Non-DNA') 
                                   & (history == 'Related') 
                                   & witness == 'Yes Witness'),
                     FUN=mean, na.rm=TRUE)
lsg121mean <- mean(lsg121[0:nrow(lsg121),2])
lsg121beta <- bp1ls + bh2ls + bw1ls

lsg200 <- aggregate (cbind(rating) ~ scenario, 
                     data = subset(dat_ipls, 
                                   (physical == 'DNA') 
                                   & (history == 'No History') 
                                   & witness == 'No Witness'),
                     FUN=mean, na.rm=TRUE)
lsg200mean <- mean(lsg200[0:nrow(lsg200),2])
lsg200beta <- bp2ls

lsg201 <- aggregate (cbind(rating) ~ scenario, 
                     data = subset(dat_ipls, 
                                   (physical == 'DNA') 
                                   & (history == 'No History') 
                                   & witness == 'Yes Witness'),
                     FUN=mean, na.rm=TRUE)
lsg201mean <- mean(lsg201[0:nrow(lsg201),2])
lsg201beta <- bp2ls + bw1ls

lsg210 <- aggregate (cbind(rating) ~ scenario, 
                     data = subset(dat_ipls, 
                                   (physical == 'DNA') 
                                   & (history == 'Unrelated') 
                                   & witness == 'No Witness'),
                     FUN=mean, na.rm=TRUE)
lsg210mean <- mean(lsg210[0:nrow(lsg210),2])
lsg210beta <- bp2ls + bh1ls

lsg211 <- aggregate (cbind(rating) ~ scenario, 
                     data = subset(dat_ipls, 
                                   (physical == 'DNA') 
                                   & (history == 'Unrelated') 
                                   & witness == 'Yes Witness'),
                     FUN=mean, na.rm=TRUE)
lsg211mean <- mean(lsg211[0:nrow(lsg211),2])
lsg211beta <- bp2ls + bh1ls + bw1ls

lsg220 <- aggregate (cbind(rating) ~ scenario, 
                     data = subset(dat_ipls, 
                                   (physical == 'DNA') 
                                   & (history == 'Related') 
                                   & witness == 'No Witness'),
                     FUN=mean, na.rm=TRUE)
lsg220mean <- mean(lsg220[0:nrow(lsg220),2])
lsg220beta <- bp2ls + bh2ls

lsg221 <- aggregate (cbind(rating) ~ scenario, 
                     data = subset(dat_ipls, 
                                   (physical == 'DNA') 
                                   & (history == 'Related') 
                                   & witness == 'Yes Witness'),
                     FUN=mean, na.rm=TRUE)
lsg221mean <- mean(lsg221[0:nrow(lsg221),2])
lsg221beta <- bp2ls + bh2ls + bw1ls

lsgconf <- as.data.frame(rbind (lsg000mean, lsg001mean, lsg010mean, lsg011mean, lsg020mean, lsg021mean,
                                lsg100mean, lsg101mean, lsg110mean, lsg111mean, lsg120mean, lsg121mean,
                                lsg200mean, lsg201mean, lsg210mean, lsg211mean, lsg220mean, lsg221mean))

lsgbeta <- as.data.frame(rbind (lsg000beta, lsg001beta, lsg010beta, lsg011beta, lsg020beta, lsg021beta,
                                lsg100beta, lsg101beta, lsg110beta, lsg111beta, lsg120beta, lsg121beta,
                                lsg200beta, lsg201beta, lsg210beta, lsg211beta, lsg220beta, lsg221beta))

lsg <- cbind(lsgconf, lsgbeta)
colnames(lsg)<-c('ls_mean_rating','ls_comb_beta')

combdat = cbind(dmg,lsg)


#=== plots


ggplot(data=combdat) +
  geom_point(aes(x=mt_comb_beta, y=mt_mean_rating, color='red'),size=3)+
  geom_point(aes(x=ls_comb_beta, y=ls_mean_rating, color='blue'),size=3)+
  geom_smooth(aes(mt_comb_beta, mt_mean_rating), method='lm', formula=y~x)+
  geom_smooth(aes(ls_comb_beta, ls_mean_rating), method='lm', formula=y~x)+
  scale_color_discrete(labels=c("Law students", "mTurk")) +
  xlim(0,100)+
  ylim(0,100)+
  theme(
    panel.grid=element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color="black"),
    #    axis.text.x = element_text(hjust = 0, size=rel(2), color='black'),
    #    axis.text.y = element_text(hjust = 1, size=rel(2.5), color='black'),
    #    axis.title.y = element_text(size=rel(1.5)),
    #    plot.title=element_text(size=20,vjust=2),
    #    legend.text = element_text(size=rel(1.5)),
    #    legend.title = element_text(size=rel(1.5)),
    legend.position=c(1,1),
    legend.justification=c(1,1))

##### FIGURE 3A BASELINE CONFIDENCE IN GUILT VARIES BY SCENARIO

plt <- ggplot()+
  geom_boxplot(data=dfpopconf, aes(x=predictor, y=post.mean), outlier.colour = NA)+
  geom_point(data=dfpopconf, aes(x=predictor, y=post.mean),position=position_jitter(width=0.1), size=3, color="grey")+
  theme(
    panel.grid=element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color="black"),
    legend.position=c(1,1),
    legend.justification=c(1,1))





# # read data
# data_file_names <- c('prepped/data_mq_deid.csv', 'prepped/data_cu_deid.csv', 'prepped/data_sq_deid.csv', 'prepped/data_th_deid.csv', 'prepped/data_tq_deid.csv')
# dlist <- list()
# for (ind in 1:length(data_file_names)) {
#   dlist[[ind]] <- read.csv(data_file_names[[ind]])
# }
# df <- do.call('rbind.fill', dlist)
# 
# # add a group variable denoting that these data sets are general population
# df['group'] = 'genpop'
# 
# lawstudents_df <- read.csv('prepped/data_ipls.csv')
# lawstudents_df['group'] <- 'legal'
# lawstudents_df['hashedID'] <- lawstudents_df['uid']
# 
# # bind these groups together
# df <- rbind.fill(df, lawstudents_df)
# 
# # make an integer ID column from the hash
# df$ID <- as.integer(df$hashedID)
# 
# # make sure some variables are appropriately encoded
# predictors <- c('ID', 'scenario', 'physical', 'history', 'witness')
# #outcomes <- c('rating', 'rate_outrage', 'rate_punishment', 'rate_threat')
# outcomes <- c('rating', 'rate_punishment')
# df <- convert_to_factor(df, predictors)
# 
# # upper and lower rating ranges
# Rmin <- 0
# Rmax <- 100
# 
# # make upper and lower variables for each outcome
# cens_names <- c()
# for (oname in outcomes) {
#   upname <- paste(oname, 'max', sep='')
#   dnname <- paste(oname, '', sep='')
#   df[[upname]] <- ifelse(df[[oname]] == Rmax, Inf, df[[oname]])
#   df[[dnname]] <- ifelse(df[[oname]] == Rmin, -Inf, df[[oname]])
#   cens_names <- c(cens_names, dnname, upname)
# }
# 
# 
# ############# let's try some models
# 
# # all responses; correlated random effects of scenario; correlated residuals; censoring
# form_string <- paste('cbind(', paste(cens_names, collapse=', '), ')', '~  -1 + trait:group:(scenario + physical + history + witness)', collapse='')
# 
# fit <- MCMCglmm(fixed = as.formula(form_string), 
#                 random = ~ us(trait):ID, 
#                 rcov = ~ us(trait):units, 
#                 family = rep('cengaussian', length(outcomes)), 
#                 data = df)
# fit_ms_ls_conf_pun <- fit
# save(fit_ms_ls_conf_pun, file='mt_ls_conf_pun.obj')
# ########### fit processing ###########

########### START FROM HERE WITH OBJ FILE

#thisobj <- fit


#dfpopconfsortb["diff"] <- dfpopconfsortb$genpop-dfpopconfsortb$lspop
#
# melted <- melt(dfpopconfsortb, id.vars=c("scenario","order"))
#
#### gsub global dataframe search and replace string
# df2 <- as.data.frame(sapply(melted,gsub,pattern="scenario",replacement=""))
# 
# library(ggplot2)
# 
# plt <- ggplot(data=melted)
# plt <- plt + 
#   geom_point(aes(x=order, y=value, color=variable))+
#   theme(
#     panel.grid=element_blank(),
#     panel.background = element_blank(),
#     axis.line = element_line(color="black"),
#     axis.text.x = element_text(hjust = 0, size=rel(2), color='black'),
#     axis.text.y = element_text(hjust = 1, size=rel(2.5), color='black'),
#     axis.title.y = element_text(size=rel(1.5)),
#     plot.title=element_text(size=20,vjust=2),
#     legend.text = element_text(size=rel(1.5)),
#     legend.title = element_text(size=rel(1.5)),
#     legend.position=c(1,1),
#     legend.justification=c(1,1))
# 
# ggsave(plt, file="sc_mt_ls.pdf", width=12, height=7, units='in', useDingbats=FALSE)
#
### punishment
#
# dfpoppungen <- dfpoppun[dfpoppun$predictor=="groupgenpop",]
# dfpoppunleg <- dfpoppun[dfpoppun$predictor=="grouplegal",]
# 
# sortedpun <- dfpoppungen[order(dfpoppungen$post.mean),]
# sortedpun["order"]<-seq(length=nrow(sortedpun))
# 
# dfpoppunsort <- merge(sortedpun, dfpoppunleg, by="factor")
# 
# dfpoppunsort <- dfpoppunsort[order(dfpoppunsort$order),]
# 
# dfpoppunsort$outcome.x<-NULL
# dfpoppunsort$outcome.y<-NULL
# 
# dfpoppunsortb<-dfpoppunsort[c(1,3,6,8)]
# colnames(dfpoppunsortb) <- c("scenario","genpop","order","lspop")
# 
# dfpoppunsortb["diff"] <- dfpoppunsortb$genpop-dfpoppunsortb$lspop
# 
# 
# meltedpun <- melt(dfpoppunsortb, id.vars=c("scenario","order"))
# 
# library(ggplot2)
# 
# plt <- ggplot(data=meltedpun)
# plt<-plt + 
#   geom_point(aes(x=order, y=value, color=variable))+
#   theme(
#     panel.grid=element_blank(),
#     panel.background = element_blank(),
#     axis.line = element_line(color="black"),
#     axis.text.x = element_text(hjust = 0, size=rel(2), color='black'),
#     axis.text.y = element_text(hjust = 1, size=rel(2.5), color='black'),
#     axis.title.y = element_text(size=rel(1.5)),
#     plot.title=element_text(size=20,vjust=2),
#     legend.text = element_text(size=rel(1.5)),
#     legend.title = element_text(size=rel(1.5)),
#     legend.position=c(1,1),
#     legend.justification=c(1,1))


### corrplots ###
# 
# dfcorr <- dfpopsc[(1:4)]
# dfcorrw<-dcast(dfcorr, factor ~ outcome+predictor)
# colnames(dfcorrw)<-c("scenario","punish_gen","punish_ls","conf_gen","conf_ls")
# dfcorrw<-dfcorrw[(2:5)]
# #dfcorrw <- as.data.frame(sapply(dfcorrw,gsub,pattern="scenario",replacement=""))
# 
# 
# library(corrplot)
# 
# pdf("corrmatrix_scenario_gen_lawstudent.pdf", width=5, height=5)
# corrplot.mixed(cor(dfcorrw), lower='ellipse', upper='number')
# dev.off()


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
