# fourth figure from the paper
library(tidyverse)
library(grid)
library(gridExtra)
library(gtable)
library(gridBase)
# library(broom)

source('ggplot_setup.R')
load('data/stan_hier_postprocess_multi.rdata')

############### Panel XX: Punishment and case strength effect correlations ##################################
plt_1 <- ggplot(data=(effects %>% filter(variable=='rho')))
plt_1 <- plt_1 + geom_pointrange(aes(x=evidence, y=X50., ymin=X2.5., ymax=X97.5., color=group), 
                         position=position_dodge(width = 0.5)) + 
  xlab('Evidence') + ylab('Correlation') +
  group_color_scale +
  evidence_plus_baseline_x_axis +
  geom_vline(xintercept=1.5, colour='grey') +
  geom_vline(xintercept=2.5, colour='grey') +
  geom_vline(xintercept=3.5, colour='grey') +
  geom_vline(xintercept=4.5, colour='grey') +
  geom_vline(xintercept=5.5, colour='grey') +
  th + 
  theme(
    axis.text.x = element_text(hjust = 0.5, size=rel(1), color='black')
  )

############### Combine into a single figure ##################################
# make a list of panels
plt_list <- list(plt_1)

# convert to grobs
grob_list <- lapply(plt_list, ggplotGrob)

# make sure axes align
max_heights <- do.call(unit.pmax, lapply(grob_list, function(x) {x$heights}))
grob_list <- lapply(grob_list, function(x) {x$heights <- max_heights; x})

# arrange with differing widths
lay <- rbind(c(1,2,3),
             c(4,5,5))
plt_all <- do.call(arrangeGrob, c(grob_list, ncol=3, layout_matrix=list(lay),
                                  widths=list(c(1.1, 0.5, 1.25))))

# save to disk
ggsave('figure_paper_4.pdf', plot=plt_all, width=11, height=8.5, units='in', useDingbats=FALSE)
