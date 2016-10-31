# this file loads and prepares data for fitting in Stan
# uses IL prosecutors
library(dplyr)
library(tidyr)
set.seed(11157)

dat_ilsa <- read.csv('data/data_prof_deid.csv') %>% filter(group == 'ILSA2016')
dat_ilsa$scenario <- as.factor(dat_ilsa$scenario)
dat_ilsa$physical <- factor(dat_ilsa$physical, labels=c('No Physical', 'Non-DNA', 'DNA'))
dat_ilsa$history <- factor(dat_ilsa$history, labels=c('No History', 'Unrelated', 'Related'))
dat_ilsa$witness <- factor(dat_ilsa$witness, labels=c('No Witness', 'Yes Witness'))


# do some cleaing of datasets prior to merge
dat_ilsa <- dat_ilsa %>% select(uid, scenario, physical, history, witness, rating) %>%
  mutate(group='ilsa')

# merge datasets
dat <- dat_ilsa %>% na.omit() %>% mutate(uid=as.integer(droplevels(uid)))
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
preds <- suppressWarnings(data.frame(varname=prednames) %>%
  separate(varname, c('scenario', 'evidence'), ':') %>%
  mutate(evidence = replace(evidence, is.na(evidence), 'baseline')) %>%
  mutate(evidence = as.factor(evidence))) %>%
  mutate(group=unique(dat$group))

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

save.image('data/stan_model_output_ilsa.rdata')
