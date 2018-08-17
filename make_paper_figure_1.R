# make figure showing predicted versus actual ratings for mTurk group
library(tidyverse)
library(grid)
library(gridExtra)
library(rstan)
library(magick)

source('ggplot_setup.R')
load('data/stan_postprocess_sv_t.rdata')
effects <- effects %>% filter(group=='mturk')
dat <- dat %>% filter(group=='mturk')

####################
# task mockup from svg
task_mock <- image_read('figs/task_mockup.svg')
plt_1 <- ggplot() + th + 
  labs(title="A", size=rel(3)) +
  annotation_custom(rasterGrob(task_mock))

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
code_means <- sc_means %>% group_by(physical, witness, history) %>% 
  summarise(rating=mean(rating)) %>%
  ungroup() %>%
  mutate_at(c("history"), funs(substr(as.character(.), 1, 1))) %>%
  mutate_at(c("witness"), funs(sapply(strsplit(as.character(.), " "), `[`, 1)))

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

plt_2a <- ggplot(sc_means) +
     geom_boxplot(aes(code, rating), outlier.size=0, outlier.stroke=0) +
     geom_jitter(aes(code, rating), width=0.2, alpha=0.5, color=color_genpop) +
     geom_point(data=preds, aes(x=code, y=pred), color="red", shape=5, size=5, stroke=2) +
     ylab('Mean case strength (points)') +
     coord_cartesian(ylim = c(-0, 100), expand = TRUE) +
     labs(title="B", size=rel(3)) +
     annotate(geom = "text", x = seq_len(18), y = -8, 
              label = code_means$history, size = 4) +
     annotate(geom = "text", x = -0, 
              y = -8, label = "Prior conviction", size = 4, hjust=1) +
     annotate(geom = "text", x = seq(2, 18, 3), 
              y = -15, label = code_means$witness[seq(2, 18, 3)], size = 4) +
     annotate(geom = "text", x = -0, 
              y = -15, label = "Witness", size = 4, hjust=1) +
     annotate(geom = "text", x = seq(3.5, 18, 6), 
              y = -22, label = code_means$physical[seq(3, 18, 6)], size = 4) +
     annotate(geom = "text", x = -0, 
              y = -22, label = "Physical evidence", size = 4, hjust=1) +
     th +
     theme(
       plot.margin=unit(c(25.5, 25.5, 50.5, 25.5), "points"),
       axis.text.x = element_blank(),
       axis.title.x = element_blank()
     )

plt_2 <- ggplot_gtable(ggplot_build(plt_2a))
plt_2$layout$clip[plt_2$layout$name == "panel"] <- "off"
grid::grid.draw(plt_2)
####################

# plot punishment ratings for each scenario
load('data/stan_postprocess_2v_t.rdata')

# dataframe linking scenarios to seriousness as rank ordered by PS
sc_ranked <- as.factor(c(27, 6, 12, 29, 13, 14, 1, 24, 2, 22, 25,
                                        3, 8, 9, 4, 18, 33, 15, 7, 19, 28, 32, 5, 11, 26, 17,
                                        20, 30, 31, 21, 10, 16, 23))
crime_type <- rep('state', 33)
crime_type[c(1, 14, 28)] <- 'federal'
crime_type <- as.factor(crime_type)

seriousness <- data.frame(seriousness=as.factor(c(1:33)), 
                          scenario=sc_ranked,
                          crime_type=crime_type)

df <- merge(dat, seriousness) %>% filter(rating_type=='rate_punishment') %>% 
  group_by(scenario, seriousness, crime_type) %>% 
  summarise(punish=median(rating), lower=quantile(rating, 0.25), 
            upper=quantile(rating, 0.75))
  

# boxplot punishment rating by seriousness
plt_3 <- ggplot(df) +
  geom_pointrange(aes(seriousness, punish, ymin=lower, ymax=upper), color=color_conf) + 
  geom_smooth(aes(as.numeric(seriousness), punish), color=color_conf, span=0.85, fullrange=TRUE) +
  scale_x_discrete(name='Scenario (rank-ordered by severity)',
                   breaks=c(1:33),
                   labels=seriousness$scenario) + 
  coord_cartesian(ylim=c(0, 100)) +
  ylab("Punishment Rating (points)") +
  labs(title="D", size=rel(3)) +
  th +
  theme(
    axis.text.x = element_text(size=rel(0.75))
    )

################
# plot relationship between case strength and guilt

dat <- read.csv('data/combined_data.csv') %>% 
  filter(!is.na(guilty), !is.na(rating), rating_type=='rating')

fit <- glm(guilty ~ rating, family = binomial(), data=dat)
preds <- predict.glm(fit, newdata=data.frame(rating=1:100), type="response")

pred_dat <- data.frame(prob=preds, rating=1:100)
plt_4 <- ggplot(pred_dat) + geom_line(aes(x=rating, y=prob)) + 
  stat_summary_bin(data=dat, aes(x=rating, y=guilty), fun.data = "mean_cl_boot") +
  xlab("Case Strength (points)") +
  ylab("Probability of Voting Guilty") +
  labs(title="C", size=rel(3)) +
  th

#Scenario make a list of panels
plt_list <- list(plt_1, plt_2, plt_3, plt_4)

# make sure axes align
max_heights <- do.call(unit.pmax, lapply(plt_list, function(x) {x$heights}))
grob_list <- lapply(plt_list, function(x) {x$heights <- max_heights; x})

# arrange with differing widths
lay <- rbind(c(1, 2), c(4, 3))
plt_all <- do.call(arrangeGrob, c(plt_list, ncol=2, layout_matrix=list(lay),
                                  widths=list(c(1, 1))))

ggsave('figs/figure_paper_1.pdf', plot=plt_all, width=12, height=10, units='in', useDingbats=FALSE)
