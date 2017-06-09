# take stan model output and perform expensive postprocessing
library(tidyr)
library(dplyr)
library(rstan)

# get posterior means for effects
datfiles <- c('data/stan_model_output_hier_t_mturk.rdata',
              'data/stan_model_output_hier_t_ipls.rdata',
              'data/stan_model_output_hier_t_lsba.rdata',
              'data/stan_model_output_hier_t_ilsa.rdata')

renamer <- function(x) {
  gsub("\\[\\s*(\\d+)(,\\s*(\\d+))*\\s*\\]", "_\\3\\_\\1", x, perl=TRUE)
}
qprobs <- c(0.025, 0.5, 0.975)
eff_list <- list()
raw_list <- list()
for (dd in 1:length(datfiles)) {
  load(datfiles[dd])
  
  # get matrix of summary statistics for each variable of interest
  ss <- data.frame(summary(fit, pars=c('mu', 'eta', 'gamma', 'tau', 'sigma'), probs=qprobs)$summary)

  # change rownames to make them easy to parse
  rownames(ss) <- sapply(rownames(ss), renamer)

  # make row names into a column
  ss$var <- rownames(ss)
  rownames(ss) <- NULL

  # make var into separate columns for variable, evidence code, and scenario
  ss <- ss %>% separate(var, into=c("variable", "evidence_num", "scenario"))

  # make group a character vector so we can merge without worrying about factor levels
  preds$group <- as.character(preds$group)

  # create a trivial evidence code column
  preds$evidence_num <- rownames(preds)

  # bind variables columnwise
  df <- left_join(ss, preds, by="evidence_num")
  df$group <- df$group[1]  # make sure group is present in all rows and the same
  eff_list[[length(eff_list) + 1]] <- df
  
  # save raw data
  raw_list[[length(raw_list) + 1]] <- dat
}
dat <- bind_rows(raw_list)
effects <- bind_rows(eff_list) %>% mutate(group=factor(group)) %>%
  mutate(scenario=factor(as.numeric(scenario)))

save(dat, effects, form, file='data/stan_hier_postprocess.rdata')

detach("package:rstan", unload = TRUE)