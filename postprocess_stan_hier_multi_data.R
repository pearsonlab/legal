# take stan model output and perform expensive postprocessing
library(tidyverse)
library(rstan)

# get posterior means for effects
datfiles <- c('data/stan_model_output_hier_t_multi_mturk.rdata',
              'data/stan_model_output_hier_t_multi_ipls.rdata',
              'data/stan_model_output_hier_t_multi_lsba.rdata',
              'data/stan_model_output_hier_t_multi_ilsa.rdata')

renamer <- function(x) {
  y <- gsub("_", ".", x)
  gsub("\\[((\\s*(\\d+),)?\\s*(\\d+),)?\\s*(\\d+)\\s*\\]", "_\\5_\\4\\_\\3", y, perl=TRUE)
}
qprobs <- c(0.025, 0.5, 0.975)
eff_list <- list()
for (dd in 1:length(datfiles)) {
  load(datfiles[dd])
  
  # get matrix of summary statistics for each variable of interest
  ss <- data.frame(summary(fit, pars=c('mu', 'eta', 'gamma', 'tau', 'sigma', 'Omega'), probs=qprobs)$summary)
  
  # change rownames to make them easy to parse
  rownames(ss) <- sapply(rownames(ss), renamer)
  
  # make row names into a column
  ss$var <- rownames(ss)
  rownames(ss) <- NULL
  
  # clean up an edge case: L.eta[p, 1, 2] is a correlation coefficient
  non_corrs <- ss %>% filter(!grepl("L.eta", var))
  corrs <- ss %>% filter(grepl("Omega", var)) %>% filter(grepl("_1_2_\\d+", var)) %>%
                  mutate(var=sapply(var, function(x){gsub("Omega_1_2_(\\d+)", "rho__\\1_", x)}))
  ss <- rbind(non_corrs, corrs)
  
  # make var into separate columns for variable, evidence code, and scenario
  ss <- ss %>% separate(var, into=c("variable", "outcome", "evidence_num", "scenario"), sep="_")
  
  
  # make group a character vector so we can merge without worrying about factor levels
  preds$group <- as.character(preds$group)
  
  # create a trivial evidence code column
  preds$evidence_num <- rownames(preds)
  
  # bind variables columnwise
  df <- left_join(ss, preds, by="evidence_num")
  df$group <- df$group[1]  # make sure group is present in all rows and the same
  eff_list[[length(eff_list) + 1]] <- df
}
effects <- bind_rows(eff_list) %>% mutate(group=factor(group)) %>%
  mutate(scenario=factor(as.numeric(scenario))) %>%
  mutate(outcome = factor(outcome, levels=1:Nr, labels=outcomes))

save(dat, effects, form, file='data/stan_hier_postprocess_multi.rdata')

detach("package:rstan", unload = TRUE)
