# combine all datasets into a single relatively clean one
library(tidyverse)

datadir <- 'data/'

### Mturk
group <- 'mturk'
data_file_names <- paste(datadir, c('data_mq_nothreat_deid.csv',
                                    'data_cu_deid.csv',
                                    'data_sq_nothreat_deid.csv',
                                    'data_nb_deid.csv',
                                    'data_th_deid.csv'), sep="")
dlist <- list()
for (ind in 1:length(data_file_names)) {
  dlist[[ind]] <- read.csv(data_file_names[[ind]])
  
  # recode threat question in threat dataset as distinct
  if (ind == length(data_file_names)) {
    dlist[[ind]] <- dlist[[ind]] %>% mutate(rate_threat_2=rate_threat) %>% select(-rate_threat)
  }
}
df <- do.call('bind_rows', dlist)

# recode some demographics
df <- df %>% mutate(nonwhite=race != 5, 
                    hispanic=ethnicity == 1, 
                    female=gender == 2) %>%
  mutate_at(c('nonwhite', 'hispanic', 'female'), 'as.factor')

# do some cleaing of datasets prior to merge
dat <- df %>% dplyr::rename(uid=hashedID) %>%
              select(uid, scenario, physical, history, witness,
                     rating, rate_punishment, rate_threat, rate_threat_2, rate_outrage,
                     nonwhite, hispanic, female) %>%
              gather(key=rating_type, value=rating, c(rating, rate_punishment, rate_threat, rate_threat_2, rate_outrage)) %>%
              mutate_at(c('uid', 'scenario', 'physical', 'history', 'witness', 'rating_type'), 'as.factor') %>%
              mutate(group=group)
              
levels(dat$witness) <- c("No Witness", "Yes Witness")
levels(dat$physical) <- c("No Physical", "Non-DNA", "DNA")
levels(dat$history) <- c("No History", "Unrelated", "Related")


### IPLS

load('data/dat_ipls.rdata')
group <- 'legal'
df <- dat_ipls %>% select(uid, scenario, physical, history, witness, rating, rate_punishment) %>%
  gather(key=rating_type, value=rating, c(rating, rate_punishment)) %>%
  mutate(group=group)

dat <- bind_rows(dat, df)

### LSBA
df <- read.csv('data/data_prof_deid.csv')
group <- 'lsba'
df$scenario <- as.factor(df$scenario)
df$physical <- factor(df$physical, labels=c('No Physical', 'Non-DNA', 'DNA'))
df$history <- factor(df$history, labels=c('No History', 'Unrelated', 'Related'))
df$witness <- factor(df$witness, labels=c('No Witness', 'Yes Witness'))
df <- df %>% filter(group == 'LSBA2016')
df <- df %>% select(uid, scenario, physical, history, witness, rating, rate_punishment) %>% 
  gather(key=rating_type, value=rating, c(rating, rate_punishment)) %>%
  mutate(group=group)

dat <- bind_rows(dat, df)

### ILSA
df <- read.csv('data/data_prof_deid.csv')
group <- 'ilsa'
df$scenario <- as.factor(df$scenario)
df$physical <- factor(df$physical, labels=c('No Physical', 'Non-DNA', 'DNA'))
df$history <- factor(df$history, labels=c('No History', 'Unrelated', 'Related'))
df$witness <- factor(df$witness, labels=c('No Witness', 'Yes Witness'))
df <- df %>% filter(group == 'ILSA2016')
df <- df %>% select(uid, scenario, physical, history, witness, rating, rate_punishment) %>%
  gather(key=rating_type, value=rating, c(rating, rate_punishment)) %>%
  mutate(group='ilsa')

dat <- bind_rows(dat, df)

### write out
write.csv(dat, "data/combined_data.csv", row.names = FALSE)