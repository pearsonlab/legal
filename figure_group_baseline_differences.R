# plot baseline effects across groups
# was figure 3A in poster

################# load data and packages: all groups ####################
library(dplyr)
library(tidyr)
library(ggplot2)

load('data/fit_mt_ls_conf_pun.obj')
load('data/dat_rating_comb.rdata')
load('data/dat_rating_pun_comb.rdata')
load('data/dat_ipls.rdata')

fitobj <- fit_ms_ls_conf_pun
source('process_fits.R')

##### FIGURE 3A BASELINE CONFIDENCE IN GUILT VARIES BY SCENARIO
se <- fixed_effects %>% filter(outcome == 'rating',
                               grepl('scenario', predictor.2))
plt <- ggplot() +
  geom_boxplot(data=se, aes(x=predictor.1, y=post.mean, color=predictor.1), outlier.colour = NA) +
  geom_point(data=se, aes(x=predictor.1, y=post.mean), 
             position=position_jitter(width=0.1), size=3, color="grey") +
  theme(
    panel.grid=element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color="black"),
    legend.position=c(1,1),
    legend.justification=c(1,1))
