# this file loads and prepares data for fitting in Stan
# for this analysis, compare across all response types (outrage, punishment, etc.)
# uses only mTurk data

library(tidyverse)

set.seed(11157)
stan_seed <- 11158 
nchains <- 4
iter <- 1000
thin <- 1

group <- 'mturk'
datadir <- 'data/'
data_file_names <- paste(datadir, c('data_mq_nothreat_deid.csv',
                                    'data_cu_deid.csv',
                                    'data_sq_nothreat_deid.csv',
                                    'data_nb_deid.csv',
                                    'data_th_deid.csv'), sep="")
dlist <- list()
for (ind in 1:length(data_file_names)) {
  dlist[[ind]] <- read.csv(data_file_names[[ind]])
}
df <- do.call('bind_rows', dlist)

# do some cleaing of datasets prior to merge
dat <- df %>% dplyr::rename(uid=hashedID) %>%
              select(uid, scenario, physical, history, witness,
                     rating, rate_punishment, rate_threat, rate_outrage) %>%
              gather(key=rating_type, value=rating, c(rating, rate_punishment, rate_threat, rate_outrage)) %>%
              mutate_at(c('uid', 'scenario', 'physical', 'history', 'witness', 'rating_type'), 'as.factor') %>%
              mutate(group=group)
              
levels(dat$witness) <- c("No Witness", "Yes Witness")
levels(dat$physical) <- c("No Physical", "Non-DNA", "DNA")
levels(dat$history) <- c("No History", "Unrelated", "Related")

# final cleanup
dat <- dat %>% gather(key=rating_type, value=rating, c(rating, rate_punishment)) %>% 
               mutate(rating_type = as.factor(rating_type))
outcomes <- levels(as.factor(dat$rating_type))

# clear out missing
dat <- dat %>% na.omit() %>% mutate(uid=as.integer(droplevels(uid)))

Nsub <- length(unique(dat$uid))

# subsample for quick prototyping
# dat <- dat %>% sample_frac(0.1)

# get upper and lower-bounded censored data
L <- min(dat$rating)
U <- max(dat$rating)

# get censoring data frame
R <- dat$rating
Ri <- as.integer(dat$rating_type)
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
                 X=X, S=S, C=C, cens=cens)

# need to initialize sigma to be large so that all ratings have nonzero probability
init <- function() {
  list(sigma = 25 + rnorm(Nr))
}
fit <- stan(file = 'model_hier_scenario_multivar.stan', data = stan_dat,
            iter = iter, chains = nchains, thin=thin, seed=stan_seed,
            pars=c('mu', 'eta', 'gamma', 'tau', 'sigma', 'Omega'),
            init=init, verbose=TRUE)

save.image(paste('data/stan_model_output_hier_t_multi_all.rdata', sep=''))
