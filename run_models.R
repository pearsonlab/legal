# main model runner file
# usage:
# $ Rscript run_models.R <model> <group> <dist>
# <model> is the model to run: 
#    'sv':    hierarchical (confidence rating)
#    '2v':    two outcomes (confidence, punishment)
#    'mv':    multivariate outcomes for all responses (mturk only)
#    'demos': hierarchical with demographics (mturk only)
# <group> is the model to run (where applicable): 
#    'mturk': Amazon Mechanical Turk workers
#    'legal': law student group
#    'lsba':  Louisiana Bar
#    'ilsa':  Illinois Prosecutors
# <dist> is the distribution to use:
#    't':     models based on student t errors (default)
#    'norm':  models based on normal distribution

library(tidyverse)
datadir <- 'data/'

# set random seeds (for reproducibility)
set.seed(11157)
stan_seed <- 11158 

# get command line arguments
args <- commandArgs(trailingOnly=TRUE)
model <- args[1]
group <- args[2]
dist <- ifelse(is.na(args[3]), 't', args[3])

if (model == 'mv' | model == 'demos') {
  group <- 'mturk'
} else if (is.na(group)) {
  stop("Group must be specified for the model chosen.")
}

# load data and subset
dat <- read.csv('data/combined_data.csv') %>%
  filter(group==!!group, evidence_shown, rating_type != 'guilty') %>% 
  mutate_at(c('scenario', 'nonwhite', 'hispanic', 'female'),  'as.factor')

switch(model, 
       'sv'= {
         modelstr <- paste('models/sv_', dist, '.stan', sep='')
         dat <- dat %>% filter(rating_type=='rating') %>% 
           select(uid, scenario, physical, history, witness, rating, group)
       },
       
       '2v' = {
         modelstr <- paste('models/mv_', dist, '.stan', sep='')
         dat <- dat %>% 
           filter(rating_type=='rating' | rating_type=='rate_punishment') %>% 
           mutate(rating_type=droplevels(rating_type)) %>%
           select(uid, scenario, physical, history, witness, rating, rating_type, group) 
       },
       
       'mv' = {
         modelstr <- paste('models/mv_', dist, '.stan', sep='')
         dat <- dat %>% 
           select(uid, scenario, physical, history, witness, rating, rating_type, group) %>%
           filter(rating_type != 'guilty') %>%
           mutate(rating_type=droplevels(rating_type))
       },
       
       'demos' = {
         modelstr <- paste('models/sv_', dist, '.stan', sep='')
         dat <- dat %>% filter(rating_type=='rating') %>% 
           select(uid, scenario, physical, history, witness, rating, group,
                  nonwhite, hispanic, female)
       }
)

# reorder levels appropriately
dat$witness <- factor(dat$witness, levels=c("No Witness", "Yes Witness"))
dat$physical <- factor(dat$physical, levels=c("No Physical", "Non-DNA", "DNA"))
dat$history <- factor(dat$history, levels=c("No History", "Unrelated", "Related"))

# useful for postprocessing:
if ('rating_type' %in% names(dat)) {
  outcomes <- levels(as.factor(dat$rating_type))
}

# subsample for quick prototyping
# dat <- dat %>% sample_frac(0.1)

# final cleanup
dat <- dat %>% na.omit() %>% mutate(uid=as.integer(droplevels(uid)))

# number of subjects
Nsub <- length(unique(dat$uid))

# get upper and lower-bounded censored data
L <- min(dat$rating)
U <- max(dat$rating)

# get censoring data frame
R <- dat$rating
cens <- (R == U) - (R == L)
if ('rating_type' %in% names(dat)) {
  Ri <- as.integer(as.factor(dat$rating_type))
}

# get design matrix (i.e., convert categoricals to binaries)
if (model == 'demos'){
  form <- as.formula("~ physical + history + witness + female + nonwhite + hispanic")
} else {
  form <- as.formula("~ physical + history + witness")
}
X <- model.matrix(form, data=dat)

# make data frame of predictors corresponding to columns in X
prednames <- colnames(X)
prednames[1] <- 'baseline'
preds <- data.frame(evidence=prednames, group=group)

# break out ratings, subject mapping
if ('rating_type' %in% names(dat)) {
  Nr <- nlevels(dat$rating_type) 
} else {
  Nr <- 1
}
S <- dat$uid
C <- as.numeric(dat$scenario)

# useful dimensions
N <- dim(X)[1]
P <- dim(X)[2]
Nc <- length(levels(dat$scenario))

# write some stan
library(rstan)

# write compiled model and use multiple chains in parallel
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# run some stan
nchains <- 4
if ('rating_type' %in% names(dat)) {
  stan_dat <- list(L=L, U=U, Nsub=Nsub, Nc=Nc, N=N, Nr=Nr, P=P, R=R, Ri=Ri,
                   C=C, X=X, S=S, cens=cens)
  pars <- c('mu', 'eta', 'gamma', 'tau', 'sigma', 'Omega')
  iter <- 2000
  thin <- 2
} else {
  stan_dat <- list(L=L, U=U, Nsub=Nsub, Nc=Nc, N=N, P=P, R=R, C=C,
                   X=X, S=S, cens=cens)
  pars <- c('mu', 'eta', 'gamma', 'tau', 'sigma')
  iter <- 1000
  thin <- 1
}

if (dist == 't') {
  pars <- c(pars, 'nu_eps', 'nu_delta')
}


# need to initialize sigma to be large so that all ratings have nonzero probability
init <- function() {
  list(sigma = 25 + rnorm(Nr))
}

# fit model
fit <- stan(file = modelstr, data = stan_dat, pars=pars, iter = iter, 
            chains = nchains, thin=thin, init=init, seed=stan_seed)

# save data
save.image(paste(datadir, 
                 paste('stan', 'model', 'output', model, group, dist, sep='_'), 
                 '.rdata', sep='')
           )