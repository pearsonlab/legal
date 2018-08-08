# this file loads and prepares data for fitting in Stan
# uses IL prosecutors
library(tidyverse)

set.seed(11157)
stan_seed <- 11158 
nchains <- 4
iter <- 2000
thin <- 2

args = commandArgs(trailingOnly=TRUE)
group <- args[1]

# load data and subset
dat <- read.csv('data/combined_data.csv')
dat <- dat %>% filter(group==!!group, rating_type=='rating' | rating_type=='rate_punishment') %>% 
  select(uid, scenario, physical, history, witness, rating, rating_type) %>%
  mutate_at(c('uid', 'scenario', 'physical', 'history', 'witness', 'rating_type', 'group'), 'as.factor')
levels(dat$witness) <- c("No Witness", "Yes Witness")
levels(dat$physical) <- c("No Physical", "Non-DNA", "DNA")
levels(dat$history) <- c("No History", "Unrelated", "Related")

# clear out missing
dat <- dat %>% na.omit() %>% mutate(uid=as.integer(droplevels(uid)))

Nsub <- length(unique(dat$uid))

# subsample for quick prototyping
#dat <- dat %>% sample_frac(0.1)

# get upper and lower-bounded censored data
L <- min(dat$rating)
U <- max(dat$rating)

# get censoring data frame
R <- dat$rating
Ri <- as.integer(as.factor(dat$rating_type))
cens <- (R == U) - (R == L)


# get design matrix (i.e., convert categoricals to binaries)
#form <- as.formula("~ -1 + scenario + scenario:(physical + history + witness)")
form <- as.formula("~ physical + history + witness")
X <- model.matrix(form, data=dat)

# make data frame of predictors corresponding to columns in X
prednames <- colnames(X)
prednames[1] <- 'baseline'
preds <- data.frame(evidence=prednames, group=group)

# break out ratings, subject mapping
Nr <- length(levels(dat$rating_type))
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
stan_dat <- list(L=L, U=U, Nsub=Nsub, Nc=Nc, N=N, Nr=Nr, P=P, R=R, Ri=Ri,
                 C=C, X=X, S=S, cens=cens)

init <- function() {
  list(sigma = 25 + rnorm(Nr))
}
fit <- stan(file = 'model_hier_scenario_multivar.stan', data = stan_dat,
            iter = iter, chains = nchains, thin=thin, seed=stan_seed,
            pars=c('mu', 'eta', 'gamma', 'tau', 'sigma', 'Omega'),
            init=init)

save.image(paste('data/stan_model_output_hier_t_multi_', dset, '.rdata', sep=''))
