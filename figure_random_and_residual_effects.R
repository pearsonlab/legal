# correlation plots for random and residual effects
library(ggplot2)
library(gridExtra)
library(dplyr)

# load fit object containing mturkers only
load('data/fit.rdata')

# change name of fit object and do some basic processing
fitobj <- fit
source('process_fits.R')


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