# run Stan 

# start the timer
ptm <- proc.time()

library(rstan)

source("prep_data_for_stan.R")

# extract data from files
dat <- prep_data()

model <- 'model0'
modstr <- paste(model,'.stan', sep='')

# tester code:
# uncomment and run this code (much faster) to test the model
fit <- stan(file = modstr, data = dat, iter = 1000, chains = 3)
