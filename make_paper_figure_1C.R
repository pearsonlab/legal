# make figure showing predicted versus actual ratings for mTurk group
library(tidyr)
library(dplyr)
library(ggplot2)
library(rstan)
color_genpop='#1b9e77'
color_lawstudents='#d95f02'
color_lsba ='#7570b3'
color_ilsa ='#9e331b'
# more colors
# #023bd9
# #b39470

# get posterior means for effects
datfiles <- c('data/stan_model_output_hier_t_mturk.rdata')

renamer <- function(x) {
  gsub("\\[\\s*(\\d+)(,\\s*(\\d+))*\\s*\\]", "_\\3\\_\\1", x, perl=TRUE)
}
qprobs <- c(0.025, 0.5, 0.975)
eff_list <- list()
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
}
effects <- bind_rows(eff_list) %>% mutate(group=factor(group)) %>%
  mutate(scenario=factor(as.numeric(scenario)))

####################

# calculate mean rating for session and evidence combinations
sc_means <- dat %>% group_by(scenario, physical, history, witness) %>% 
                    summarise(rating=mean(rating)) %>%
                    arrange(scenario, physical, witness, history) %>%
                    ungroup()

# get unique variable code for each evidence combination
ev_codes <- sc_means %>% select(physical, witness, history) %>% 
                         distinct(physical, witness, history) %>%
                         mutate(code=as.factor(row_number()))

# add code column to scenario means
sc_means <- sc_means %>% merge(ev_codes) %>% arrange(code, scenario)

# calculate model prediction
# get mean values of coefficients across scenarios
betas <- effects %>% filter(variable == 'gamma') %>% 
         select(scenario, evidence, mean) %>%
         group_by(evidence) %>%
         summarise(effect=mean(mean)) %>%
         arrange(evidence)

# generate model matrix for predictions         
Xmat <- model.matrix(form, ev_codes)
colnames(Xmat)[1] <- 'baseline'
Xmat <- Xmat[, sort(colnames(Xmat), index.return=TRUE)$ix]
preds <- data.frame(code=ev_codes$code, pred=Xmat %*% betas$effect)

p <- ggplot(sc_means)
p <- p + geom_boxplot(aes(code, rating), outlier.size=0, outlier.stroke=0) +
     geom_jitter(aes(code, rating), width=0.2, alpha=0.5, color=color_genpop) +
     geom_point(data=preds, aes(x=code, y=pred), color="red", shape=5, size=5, stroke=2) +
     xlab('Evidence') + ylab('Mean strength of case (points)') +
     ylim(0, 100) +
     theme(
       panel.grid=element_blank(),
       panel.background = element_blank(),
       axis.line = element_line(color="black"),
       axis.text.x = element_blank(),
       axis.title.x = element_text(size=rel(1.5)),
       axis.ticks.x = element_blank(),
       axis.text.y = element_text(hjust = 1, size=rel(2.5), color='black'),
       axis.title.y = element_text(size=rel(1.5)),
       plot.title=element_text(size=20,vjust=2),
       legend.text = element_text(size=rel(1.5)),
       legend.title = element_text(size=rel(1.5)),
       legend.position='none')

ggsave('data_vs_predictions.pdf', plot=p, width=8, height=5, units='in', useDingbats=FALSE)
