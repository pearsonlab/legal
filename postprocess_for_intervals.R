# do MCMC postprocessing to get HPD intervals for differences in means
library(tidyverse)
library(rstan)

### First, do group comparisons for confidence 

datfiles <- c('data/stan_model_output_sv_mturk_t.rdata',
              'data/stan_model_output_sv_legal_t.rdata',
              'data/stan_model_output_sv_lsba_t.rdata',
              'data/stan_model_output_sv_ilsa_t.rdata')
datnames <- c("mTurk", "Law Students", "Louisiana Bar", "Illinois Prosecutors")

mu_list <- list()
for (dd in 1:length(datfiles)) {
  load(datfiles[dd])
  
  mu <- data.frame(extract(fit, pars='mu'))
  names(mu) <- prednames
  mu_list[[length(mu_list) + 1]] <- mu
}
  
hpd <- function(x, coverage=0.95) {
  # return an interval that covers a fraction coverage of the samples in x
  sorted <- sort(x)
  n <- length(sorted)
  tail <- (1 - coverage)/2
  lower <- floor(n * tail)
  upper <- ceiling(n * (1 - tail))
  return(list(sorted[lower], sorted[upper]))
}

make_ci_df <- function(mu_list, datnames) {
  ci_list <- list()
  for (ii in 1:(length(mu_list) - 1)) {
    for (jj in (ii + 1):length(mu_list)) {
      dmu <- (mu_list[[jj]] - mu_list[[ii]]) %>% 
        do(data.frame(ci=I(apply(., 2, hpd)))) %>% t()
      row.names(dmu) <- paste(datnames[jj], datnames[ii], sep = " - ")
      ci_list[[length(ci_list) + 1]] <- as.data.frame(dmu)
    }
  }
  ci_df = do.call(rbind, ci_list)
  names(ci_df) <- c("Crime effect", "Non-DNA physical evidence", "DNA physical evidence",
                    "Unrelated prior crime", "Related prior crime", "Witness present")
  ci_df <- ci_df[,c(3, 2, 6, 5, 4, 1)]
  return(ci_df)
}

group_ci_df <- make_ci_df(mu_list, datnames)

### Now, contrasts between rating types (mTurk)

datfiles <- c('data/stan_model_output_mv_mturk_t.rdata') 
load(datfiles)
mu <- data.frame(extract(fit, pars='mu'))
Noutcomes <- length(outcomes)
Nregressors <- length(prednames)

mu_list <- list()
for (dd in 1:Noutcomes) {
  mm <- mu[,(dd - 1) * Nregressors + 1:Nregressors]
  names(mm) <- prednames
  mu_list[[length(mu_list) + 1]] <- mm
}

ratings_ci_df <- make_ci_df(mu_list,  c("Outrage", "Punishment", "Likelihood", "Threat", "Confidence"))

save(group_ci_df, ratings_ci_df, file='data/stan_postprocess_ci.rdata')

detach("package:rstan", unload = TRUE)