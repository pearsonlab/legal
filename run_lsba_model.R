# this file loads and prepares data for fitting in Stan
# uses law students and turkers
set.seed(11157)

dat_lsba <- read.csv('data/data_lsba_deid.csv')
dat_lsba$scenario <- as.factor(dat_lsba$scenario)
dat_lsba$physical <- factor(dat_lsba$physical, labels=c('No Physical', 'Non-DNA', 'DNA'))
dat_lsba$history <- factor(dat_lsba$history, labels=c('No History', 'Unrelated', 'Related'))
dat_lsba$witness <- factor(dat_lsba$witness, labels=c('No Witness', 'Yes Witness'))

library(dplyr)
library(tidyr)

# do some cleaing of datasets prior to merge
dat_ls <- dat_ipls %>% select(uid, scenario, physical, history, witness, rating) %>%
  mutate(group='legal')

dat_turk <- dat_rating_comb %>% rename(uid=hashedID) %>% mutate(group='mturk')

dat_lsba <- dat_lsba %>% select(uid, scenario, physical, history, witness, rating) %>%
  mutate(group='lsba')

# merge datasets
#dat <- rbind(dat_turk, dat_ls, dat_lsba) %>% na.omit() %>% mutate(uid=as.integer(droplevels(uid)))
dat <- rbind(dat_lsba) %>% na.omit() %>% mutate(uid=as.integer(droplevels(uid)))
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
form <- as.formula("~ -1 + scenario + scenario:(physical + history + witness)")
X <- model.matrix(form, data=dat)
XL <- model.matrix(form, data=datL)
XU <- model.matrix(form, data=datU)

# make data frame of predictors corresponding to columns in X
prednames <- colnames(X)
suppressWarnings(preds <- data.frame(varname=prednames) %>%
  separate(varname, c('scenario', 'group', 'evidence'), ':') %>%
  mutate(evidence = replace(evidence, is.na(evidence), 'baseline')) %>%
  mutate(evidence = as.factor(evidence)))

# break out ratings, subject mapping
R <- dat$rating
S <- dat$uid
SL <- datL$uid
SU <- datU$uid

# useful dimensions
N <- dim(X)[1]
P <- dim(X)[2]
NL <- dim(XL)[1]
NU <- dim(XU)[1]

# write some stan
library(rstan)

# write compiled model and use multiple chains in parallel
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# run some stan
stan_dat <- list(L=L, U=U, Nsub=Nsub, N=N, NL=NL, NU=NU, P=P, R=R,
                 X=X, XL=XL, XU=XU, S=S, SL=SL, SU=SU)

fit <- stan(file = 'model.stan', data = stan_dat,
            iter = 1000, chains = 4)

save.image('data/stan_model_output_lsba.rdata')
