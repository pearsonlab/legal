# this file loads and prepares data for fitting in Stan
# uses IL prosecutors
library(dplyr)
library(tidyr)

set.seed(11157)
args = commandArgs(trailingOnly=TRUE)
dset <- args[1]
foo <- switch(dset, 
              mturk={
                load('data/dat_rating_comb.rdata')
                # do some cleaing of datasets prior to merge
                group <- 'mturk'
                dat <- dat_rating_comb %>% rename(uid=hashedID) %>% mutate(group='mturk')
              },
              ipls={
                load('data/dat_ipls.rdata')
                group <- 'legal'
                dat <- dat_ipls %>% select(uid, scenario, physical, history, witness, rating) %>%
                  mutate(group='legal')
              },
              lsba={
                dat <- read.csv('data/data_prof_deid.csv') 
                group <- 'lsba'
                dat$scenario <- as.factor(dat$scenario)
                dat$physical <- factor(dat$physical, labels=c('No Physical', 'Non-DNA', 'DNA'))
                dat$history <- factor(dat$history, labels=c('No History', 'Unrelated', 'Related'))
                dat$witness <- factor(dat$witness, labels=c('No Witness', 'Yes Witness'))
                dat <- dat %>% filter(group == 'LSBA2016') 
                dat <- dat %>% select(uid, scenario, physical, history, witness, rating)
              },
              ilsa={
                dat <- read.csv('data/data_prof_deid.csv') 
                group <- 'ilsa'
                dat$scenario <- as.factor(dat$scenario)
                dat$physical <- factor(dat$physical, labels=c('No Physical', 'Non-DNA', 'DNA'))
                dat$history <- factor(dat$history, labels=c('No History', 'Unrelated', 'Related'))
                dat$witness <- factor(dat$witness, labels=c('No Witness', 'Yes Witness'))
                dat <- dat %>% filter(group == 'ILSA2016') 
                dat <- dat %>% select(uid, scenario, physical, history, witness, rating)
              }
)

# final cleanup
dat <- dat %>% na.omit() %>% mutate(uid=as.integer(droplevels(uid))) 
Nsub <- length(unique(dat$uid))

# subsample for quick prototyping
#dat <- dat %>% sample_frac(0.1)

# get upper and lower-bounded censored data
L <- min(dat$rating)
U <- max(dat$rating)

datL <- dat %>% filter(rating == L)
datU <- dat %>% filter(rating == U)
dat <- dat %>% filter(rating != U & rating != L)

# get design matrix (i.e., convert categoricals to binaries)
#form <- as.formula("~ -1 + scenario + scenario:(physical + history + witness)")
form <- as.formula("~ physical + history + witness")
X <- model.matrix(form, data=dat)
XL <- model.matrix(form, data=datL)
XU <- model.matrix(form, data=datU)

# make data frame of predictors corresponding to columns in X
prednames <- colnames(X)
prednames[1] <- 'baseline'
preds <- data.frame(evidence=prednames, group=group)

# break out ratings, subject mapping
R <- dat$rating
S <- dat$uid
SL <- datL$uid
SU <- datU$uid
C <- as.numeric(dat$scenario)
CL <- as.numeric(datL$scenario)
CU <- as.numeric(datU$scenario)

# useful dimensions
N <- dim(X)[1]
P <- dim(X)[2]
Nc <- length(unique(dat$scenario))
NL <- dim(XL)[1]
NU <- dim(XU)[1]

# write some stan
library(rstan)

# write compiled model and use multiple chains in parallel
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# run some stan
stan_dat <- list(L=L, U=U, Nsub=Nsub, Nc=Nc, N=N, NL=NL, NU=NU, P=P, R=R,
                 X=X, XL=XL, XU=XU, S=S, SL=SL, SU=SU)

fit <- stan(file = 'model_hier_scenario.stan', data = stan_dat,
            iter = 1000, chains = 4)

save.image(paste('data/stan_model_output_hier_', dset, '.rdata', sep=''))
