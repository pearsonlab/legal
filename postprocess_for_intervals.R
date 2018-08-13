# do MCMC postprocessing to get HPD intervals for differences in means
library(tidyverse)
library(rstan)

### First, do group comparisons for confidence 

datfiles <- c('data/stan_model_output_hier_t_mturk.rdata',
              'data/stan_model_output_hier_t_ipls.rdata',
              'data/stan_model_output_hier_t_lsba.rdata',
              'data/stan_model_output_hier_t_ilsa.rdata')
datnames <- c("mTurk", "Law Students", "Louisiana Bar", "Illinois Prosecutors")

mu_list <- list()
for (dd in 1:length(datfiles)) {
  load(datfiles[dd])
  
  mu <- data.frame(extract(fit, pars='mu'))
  names(mu) <- prednames
  mu_list[[length(mu_list) + 1]] <- mu
}
  
# for (dd in 1:length(datfiles)) 
pval <- function(x, x0=0) {
  pv <- sum(x <= x0)/length(x)
  alpha <- 2 * min(pv, 1 - pv)  # because we want the 2-sided p-value, alpha is twice the threshold
  minalpha <- 2 / length(x)
  
  # make sure we only output a bound if the answer is 0
  signif(ifelse(alpha < minalpha, minalpha, alpha), 2)
}

make_pv_df <- function(mu_list, datnames) {
  pv_list <- list()
  for (ii in 1:(length(mu_list) - 1)) {
    for (jj in (ii + 1):length(mu_list)) {
      dmu <- (mu_list[[jj]] - mu_list[[ii]]) %>% 
        do(data.frame(pvalue=apply(., 2, pval))) %>% t()
      row.names(dmu) <- paste(datnames[jj], datnames[ii], sep = " - ")
      pv_list[[length(pv_list) + 1]] <- as.data.frame(dmu)
    }
  }
  pv_df = do.call(rbind, pv_list)
  names(pv_df) <- c("Crime effect", "Non-DNA physical evidence", "DNA physical evidence",
                    "Unrelated prior crime", "Related prior crime", "Witness present")
  pv_df <- pv_df[,c(3, 2, 6, 5, 4, 1)]
  return(pv_df)
}

group_pv_df <- make_pv_df(mu_list, datnames)

### Now, contrasts between rating types (mTurk)

datfiles <- c('data/stan_model_output_hier_t_multi_all.rdata') 
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

ratings_pv_df <- make_pv_df(mu_list,  c("Outrage", "Punishment", "Proximity", "Threat", "Confidence"))

save(group_pv_df, ratings_pv_df, file='data/stan_hier_postprocess_pvals.rdata')

detach("package:rstan", unload = TRUE)