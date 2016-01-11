# make a correlation plot of baseline effects for different outcome types
# was part of Figure 4 in poster

################# load data and packages: all groups ####################
library(dplyr)
library(tidyr)
library(ggplot2)

# load fit object containing mturkers only
load('data/fit.rdata')

# change name of fit object and do some basic processing
fitobj <- fit
source('process_fits.R')

#=== correlation
library(corrplot)
se <- fixed_effects %>% filter(grepl('scenario', predictor.1))
df <- se %>% select(predictor.1, outcome, post.mean) %>% spread(outcome, post.mean)
names(df) <- c('Scenario', 'Confidence\n in guilt','Threat','Outrage','Deserved\n punishment')

#pdf("figure4_corrmatrix.pdf", width=5, height=5)
corrplot.mixed(cor(df %>% select(-Scenario)), lower='ellipse', upper='number')
#dev.off()