# take stan model output and perform expensive postprocessing
# usage:
# $ Rscript postprocess_model_data.R <files>
# <files> is a list of files to postprocess
# entries should be of the form: stan_model_output_<model>_<group>_<distribution>.rdata

library(tidyverse)
library(rstan)
datadir <- 'data/'

# get command line arguments
datfiles <- commandArgs(trailingOnly=TRUE)

# strip path and extension
pieces <- str_split(datfiles[1], "/")[[1]] 
file_part <- pieces[length(pieces)]
ext_stripped <- str_split(file_part, "\\.")[[1]][1]
name_parts <- str_split(ext_stripped, "_")[[1]]
model <- name_parts[4]
group <- name_parts[5]
dist <- name_parts[6]

renamer1 <- function(x) {
  gsub("\\[\\s*(\\d+)(,\\s*(\\d+))*\\s*\\]", "_\\3\\_\\1", x, perl=TRUE)
}

renamer2 <- function(x) {
  y <- gsub("_", ".", x)
  gsub("\\[((\\s*(\\d+),)?\\s*(\\d+),)?\\s*(\\d+)\\s*\\]", "_\\5_\\4\\_\\3", y, perl=TRUE)
}

lowertri <- function(vname) {
  # determines whether entry is in in lower triangular portion of Omega
  matches <- str_match(vname, "Omega_(\\d+)_(\\d+)_(\\d+)")
  as.numeric(matches[,2]) > as.numeric(matches[,3])  
}

pars <- c('mu', 'eta', 'gamma', 'tau', 'sigma')
handle_omega <- FALSE
multi_output <- FALSE

switch(model,
       'sv' = {
         renamer <- renamer1
       },
       
       '2v' = {
         renamer <- renamer2
         pars <- c(pars, 'Omega')
         handle_omega <- TRUE
         multi_output <- TRUE
       },
       
       'mv' = {
         renamer <- renamer2
         pars <- c(pars, 'Omega')
         handle_omega <- TRUE
         multi_output <- TRUE
       },
       
       'demos' = {
         renamer <- renamer1
       }
)

if (dist == 't') {
  pars <- c(pars, 'nu_eps', 'nu_delta')
}

qprobs <- c(0.025, 0.5, 0.975)
eff_list <- list()
raw_list <- list()

for (dd in 1:length(datfiles)) {
  load(datfiles[dd])
  
  # get matrix of summary statistics for each variable of interest
  ss <- data.frame(summary(fit, pars=pars, probs=qprobs)$summary)

  # change rownames to make them easy to parse
  rownames(ss) <- sapply(rownames(ss), renamer)

  # make row names into a column
  ss$var <- rownames(ss)
  rownames(ss) <- NULL
  
  # clean up an edge case: Omega[p, 1, 2] is a correlation coefficient
  if (handle_omega) {
    non_corrs <- ss %>% filter(!grepl("Omega", var))
    corrs <- ss %>% filter(grepl("Omega", var)) %>% 
      filter(lowertri(var)) %>%
      mutate(var=sapply(var, function(x){gsub("Omega_(\\d+)_(\\d+)_(\\d+)", 
                                              "Omega\\.\\1\\.\\2__\\3_", x)}))
    ss <- rbind(non_corrs, corrs)
  }

  # make var into separate columns 
  if (multi_output) {
    ss <- ss %>% separate(var, into=c("variable", "outcome", "evidence_num", "scenario"), sep="_")
  } else {
    ss <- ss %>% separate(var, into=c("variable", "evidence_num", "scenario"))
  }
  
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

if (multi_output) {
  effects <- effects %>% mutate(outcome=factor(outcome, levels=1:Nr, labels=outcomes))
}

# save data
outfile <- paste(datadir, paste('stan', 'postprocess', model, dist, sep="_"), '.rdata', sep="")
save(dat, effects, form, file=outfile)

detach("package:rstan", unload = TRUE)
