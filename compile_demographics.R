# get some basic info on participants (numbers and ages, for now)
library(tidyverse)

mturk_sets <- c('data/data_mq_nothreat_deid.csv',
                'data/data_sq_nothreat_deid.csv',
                'data/data_nb_deid.csv',
                'data/data_cu_deid.csv',
                'data/data_tq_deid.csv',
                'data/data_th_deid.csv')

dlist <- list()
for (dname in mturk_sets) {
  dat <- read.csv(dname)
  
  dlist[[length(dlist) + 1]] <- dat %>% distinct(hashedID, age)
}

mturk_demos <- bind_rows(dlist)

mean_turk_age <- mean(mturk_demos$age, na.rm=TRUE)

ipls_set <- 'data/data_ipls.csv'
dat <- read.csv(ipls_set)
mean_ipls_age <- mean((dat %>% distinct(uid, age))$age, na.rm=TRUE)

prof_set <- 'data/data_prof_deid.csv'
dat <- read.csv(prof_set)
# we didn't collect ages for these groups