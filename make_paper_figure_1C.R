# make figure showing predicted versus actual ratings for mTurk group
library(tidyr)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(rstan)

source('ggplot_setup.R')
load('data/stan_hier_postprocess.rdata')
effects <- effects %>% filter(group=='mturk')
dat <- dat %>% filter(group=='mturk')

####################

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
preds <- data.frame(code=ev_codes$code, pred=Xmat %*% betas$effect)

p <- ggplot(sc_means)
p <- p + geom_boxplot(aes(code, rating), outlier.size=0, outlier.stroke=0) +
     geom_jitter(aes(code, rating), width=0.2, alpha=0.5, color=color_genpop) +
     geom_point(data=preds, aes(x=code, y=pred), color="red", shape=5, size=5, stroke=2) +
     xlab('Evidence Combinations') + ylab('Mean case strength (points)') +
     ylim(0, 100) +
     th +
     theme(
       axis.text.x = element_blank()
     )

ggsave('figure_paper_1C.pdf', plot=p, width=8, height=5, units='in', useDingbats=FALSE)

####################

# plot punishment ratings for each scenario
load('data/stan_hier_postprocess_multi.rdata')

# dataframe linking scenarios to seriousness as rank ordered by PS
seriousness <- data.frame(seriousness=as.factor(c(1:33)), 
                          scenario=as.factor(c(27, 9, 30, 6, 12, 29, 13, 14, 1, 24, 2, 22, 25,
                                        3, 8, 4, 18, 33, 15, 7, 19, 28, 32, 5, 11, 26, 17,
                                        20, 31, 21, 10, 16, 23)))

df <- merge(dat, seriousness) %>% filter(rating_type=='rate_punishment')

# boxplot punishment rating by seriousness
plt_2 <- ggplot(df) +
  geom_boxplot(aes(seriousness, rating)) + 
  scale_x_discrete(name='Case (ordered by seriousness)',
                   breaks=c(1:33),
                   labels=seriousness$scenario)

plt_3 <- ggplot(df %>% group_by(scenario, seriousness) %>% summarise(punish=median(rating))) +
  geom_point(aes(seriousness, punish)) + 
  geom_smooth(aes(as.numeric(seriousness), punish)) +
  scale_x_discrete(name='Case (ordered by seriousness)',
                   breaks=c(1:33),
                   labels=seriousness$scenario) + 
  ylab("Median Punishment Rating")

# make a list of panels
plt_list <- list(plt_2, plt_3)

# convert to grobs
# grob_list <- lapply(plt_list, ggplotGrob)

# make sure axes align
max_heights <- do.call(unit.pmax, lapply(plt_list, function(x) {x$heights}))
grob_list <- lapply(plt_list, function(x) {x$heights <- max_heights; x})

# arrange with differing widths
# lay <- rbind(c(1, 2))
# plt_all <- do.call(arrangeGrob, c(grob_list, ncol=2, layout_matrix=list(lay),
#                                   widths=list(c(1.5, 1.5))))
plt_all <- arrangeGrob(plt_2, plt_3)

ggsave('figure_paper_1D.pdf', plot=plt_all, width=6, height=8, units='in', useDingbats=FALSE)
