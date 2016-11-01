# this file loads and prepares data for fitting in Stan
# uses IL prosecutors
library(dplyr)
library(tidyr)
set.seed(11157)

# law students
load('data/dat_ipls.rdata')
dat_ls <- dat_ipls %>% select(uid, scenario, physical, history, witness, rating) %>%
  mutate(group='legal')

# lsba and ilsa
dat_prof <- read.csv('data/data_prof_deid.csv') 
dat_prof$scenario <- as.factor(dat_prof$scenario)
dat_prof$physical <- factor(dat_prof$physical, labels=c('No Physical', 'Non-DNA', 'DNA'))
dat_prof$history <- factor(dat_prof$history, labels=c('No History', 'Unrelated', 'Related'))
dat_prof$witness <- factor(dat_prof$witness, labels=c('No Witness', 'Yes Witness'))

# do some cleaing of datasets prior to merge
dat_prof <- dat_prof %>% select(uid, scenario, physical, history, witness, rating, group)


# merge datasets
dat <- rbind(dat_ls, dat_prof) %>% na.omit() %>% mutate(uid=as.integer(droplevels(uid))) %>%
  mutate(group = factor(group, levels=c('legal', 'LSBA2016', 'ILSA2016'), labels=c('students', 'lsba', 'ilsa'))) %>%
  mutate(groupnum=as.integer(group))
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
preds <- expand.grid(prednames, unique(dat$group))
names(preds) <- c('evidence', 'group')

# break out ratings, subject mapping
R <- dat$rating
S <- dat$uid
SL <- datL$uid
SU <- datU$uid
G <- (dat %>% group_by(uid, groupnum) %>% distinct() %>% ungroup() %>% arrange(uid))$groupnum
C <- as.numeric(dat$scenario)
CL <- as.numeric(datL$scenario)
CU <- as.numeric(datU$scenario)

# useful dimensions
N <- dim(X)[1]
P <- dim(X)[2]
Nc <- length(unique(dat$scenario))
Ng <- length(unique(dat$groupnum))
NL <- dim(XL)[1]
NU <- dim(XU)[1]

# write some stan
library(rstan)

# write compiled model and use multiple chains in parallel
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# run some stan
stan_dat <- list(L=L, U=U, Nsub=Nsub, Nc=Nc, Ng=Ng, N=N, NL=NL, NU=NU, P=P, R=R,
                 X=X, XL=XL, XU=XU, S=S, SL=SL, SU=SU, G=G)

fit <- stan(file = 'model_lawyer.stan', data = stan_dat,
            iter = 1000, chains = 4)

save.image('data/stan_model_output_lawyer.rdata')
