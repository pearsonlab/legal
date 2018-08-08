# this file loads and prepares data for fitting in Stan
library(tidyverse)

set.seed(11157)
stan_seed <- 11158 
nchains <- 4
iter <- 1000
thin <- 1

group <- 'mturk'
# load data and subset
dat <- read.csv('data/combined_data.csv')
dat <- dat %>% filter(group==!!group, rating_type=='rating') %>% 
  select(uid, scenario, physical, history, witness, rating, 
         nonwhite, hispanic, female) %>%
  mutate_at(c('uid', 'scenario', 'physical', 'history', 'witness', 
              'group', 'nonwhite', 'hispanic', 'female'), 'as.factor')
levels(dat$witness) <- c("No Witness", "Yes Witness")
levels(dat$physical) <- c("No Physical", "Non-DNA", "DNA")
levels(dat$history) <- c("No History", "Unrelated", "Related")
# final cleanup
dat <- dat %>% na.omit() %>% mutate(uid=as.integer(droplevels(uid)))
Nsub <- length(unique(dat$uid))

# subsample for quick prototyping
# dat <- dat %>% sample_frac(0.1)

# get upper and lower-bounded censored data
L <- min(dat$rating)
U <- max(dat$rating)

# get censoring data frame
R <- dat$rating
cens <- (R == U) - (R == L)

# get design matrix (i.e., convert categoricals to binaries)
#form <- as.formula("~ -1 + scenario + scenario:(physical + history + witness)")
form <- as.formula("~ physical + history + witness + female + nonwhite + hispanic")
X <- model.matrix(form, data=dat)

# make data frame of predictors corresponding to columns in X
prednames <- colnames(X)
prednames[1] <- 'baseline'
preds <- data.frame(evidence=prednames, group=group)

# break out ratings, subject mapping
S <- dat$uid
C <- as.numeric(dat$scenario)

# useful dimensions
N <- dim(X)[1]
P <- dim(X)[2]
Nc <- length(unique(dat$scenario))

# write some stan
library(rstan)

# write compiled model and use multiple chains in parallel
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# run some stan
stan_dat <- list(L=L, U=U, Nsub=Nsub, Nc=Nc, N=N, P=P, R=R, C=C,
                 X=X, S=S, cens=cens)

init <- function() {
  inits <- list()
  inits[['sigma']] <- 25 + rnorm(1)
  inits
}
fit <- stan(file = 'model_hier_scenario.stan', data = stan_dat,
            pars=c('mu', 'eta', 'gamma', 'tau', 'sigma'),
            iter = iter, chains = nchains, thin=thin, init=init, seed=stan_seed)

save.image(paste('data/stan_model_output_hier_t_', dset, 'with_demos.rdata', sep=''))
