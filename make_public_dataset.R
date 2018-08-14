# combine all datasets into a single relatively clean one
library(tidyverse)

datadir <- 'data/'

### Mturk
group <- 'mturk'
data_file_names <- paste(datadir, c('data_mq_nothreat_deid.csv',
                                    'data_sq_nothreat_deid.csv',
                                    'data_nb_deid.csv',
                                    'data_cu_deid.csv',
                                    'data_tq_deid.csv',
                                    'data_th_deid.csv'), sep="")
dlist <- list()
unique_ids <- c()
for (ind in 1:length(data_file_names)) {
  dlist[[ind]] <- read.csv(data_file_names[[ind]]) %>% 
    mutate(hashedID = as.character(hashedID)) %>%
    filter(!(hashedID %in% unique_ids))  # remove people who'd taken a previous version
  
  unique_ids <- c(unique_ids, unique(dlist[[ind]]$hashedID))
  
  # recode threat question in threat dataset as distinct
  if (ind == length(data_file_names)) {
    dlist[[ind]] <- dlist[[ind]] %>%  mutate(rate_threat_2=rate_threat)  %>%  select(-rate_threat) 
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
                     nonwhite, hispanic, female, question,
                     age, gender, race, ethnicity, education, political_party) %>%
              gather(key=rating_type, value=rating, c(rating, rate_punishment, rate_threat, rate_threat_2, rate_outrage)) %>%
              mutate_at(c('uid', 'scenario', 'physical', 'history', 'witness', 'rating_type'), 'as.factor') %>%
              mutate(group=group)
              
levels(dat$witness) <- c("No Witness", "Yes Witness")
levels(dat$physical) <- c("No Physical", "Non-DNA", "DNA")
levels(dat$history) <- c("No History", "Unrelated", "Related")

dat$gender = factor(dat$gender, labels=c("Male", "Female", "Other"))
dat$race = factor(dat$race, labels=c("American Indian or Alaska Native", 
                                     "Asian", 
                                     "Black or African American",
                                     "Native Hawaiian or other Pacific Islander",
                                     "White",
                                     "More than one race",
                                     "Unknown or do not want to disclose"))
dat$ethnicity = factor(dat$ethnicity, labels=c("Hispanic or Latino", 
                                               "Not Hispanic or Latino", 
                                               "Unknown or do not want to disclose"))
dat$education = factor(dat$education,
                       levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                       labels=c("Less than high school", 
                                "Completed high school",
                                "GED", 
                                "Some college",
                                "Associate's Degree", 
                                "Bachelor's Degree", 
                                "Master's Degree", 
                                "Ph.D.", 
                                "Law degree",
                                "Other professional degree"))
dat$political_party = factor(dat$political_party, 
                             labels=c("Independent or no party Affiliation", 
                                      "Republican", 
                                      "Democrat", 
                                      "Other", 
                                      "I am not a registered voter"))

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
  mutate(group=group)

dat <- bind_rows(dat, df)

### write out
write.csv(dat, "data/combined_data.csv", row.names = FALSE)