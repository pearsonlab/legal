# set up common ggplot themes, scales, etc.

color_genpop='#0656A3'
color_lawstudents='#d95f02'
color_lsba ='#b39470'
color_ilsa ='#9e331b'

color_outrage='#733238'
color_punish='#A69A60'
color_threat='#D9BF3D'
color_threat_2='#7570b3'
color_conf='#0656A3'

# more colors
# #023bd9
# #7570b3

th <-  theme(
    plot.margin=unit(c(25.5, 25.5, 25.5, 25.5), "points"),
    panel.grid=element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color="black"),
    axis.text.x = element_text(hjust = 0.5, size=rel(2), color='black'),
    axis.title.x = element_text(size=rel(1.5)),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(hjust = 1, size=rel(2), color='black'),
    axis.title.y = element_text(size=rel(1.5)),
    plot.title=element_text(size=20, vjust=2, hjust=0.5),
    legend.text = element_text(size=rel(1)),
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.background = element_blank(),
    legend.position='none')

group_color_scale <- scale_color_manual(values=c('mturk'=color_genpop,
                                                 'legal'=color_lawstudents,
                                                 'lsba'=color_lsba,
                                                 'ilsa'=color_ilsa),
                                  name='Group',
                                  breaks=c('legal', 'lsba', 'ilsa', 'mturk'),
                                  labels=c('Law Students', 'Louisiana Bar', 'Illinois Prosecutors', 'mTurk'))

group_fill_scale <- scale_fill_manual(values=c('mturk'=color_genpop,
                                               'legal'=color_lawstudents,
                                               'lsba'=color_lsba,
                                               'ilsa'=color_ilsa))

group_x_axis <- scale_x_discrete(breaks=c('legal', 'lsba', 'ilsa', 'mturk'),
                      labels=c('Law Students', 'Louisiana Bar', 'Illinois Prosecutors', 'mTurk'))


outcome_color_scale <- scale_color_manual(values=c('rating'=color_conf, 
                                                   'rate_punishment'=color_punish, 
                                                   'rate_outrage'=color_outrage, 
                                                   'rate_threat'=color_threat,
                                                   'rate_threat_2'=color_threat_2),
                                          labels=c("Case Strength", 
                                                   "Punishment", 
                                                   "Outrage", 
                                                   "Threat", 
                                                   "Threat2"))

outcome_fill_scale <- scale_fill_manual(values=c('rating'=color_conf, 
                                                 'rate_punishment'=color_punish, 
                                                 'rate_outrage'=color_outrage, 
                                                 'rate_threat'=color_threat),
                                        labels=c("Case Strength", "Punishment", "Outrage", "Threat"))

outcome_x_axis <- scale_x_discrete(breaks=c("rating","rate_punishment", "rate_outrage", "rate_threat"), 
                   labels=c("Case Strength", "Punishment", "Outrage", "Threat"))


evidence_x_axis <- scale_x_discrete(breaks=c("physicalDNA", 
                                             "physicalNon-DNA", 
                                             "witnessYes Witness", 
                                             "historyRelated", 
                                             "historyUnrelated"), 
                                     labels=c("DNA \nphysical \nevidence", 
                                              "Non-DNA \nphysical \nevidence",  
                                              "Witness \npresent", 
                                              "Related \nprior crime", 
                                              "Unrelated \nprior crime"))

evidence_plus_baseline_x_axis <- scale_x_discrete(breaks=c("baseline", 
                                                           "physicalDNA", 
                                                           "physicalNon-DNA", 
                                                           "witnessYes Witness", 
                                                           "historyRelated", 
                                                           "historyUnrelated"), 
                                                   labels=c("Baseline",
                                                            "DNA \nphysical \nevidence", 
                                                            "Non-DNA \nphysical \nevidence",  
                                                            "Witness \npresent", 
                                                            "Related \nprior crime", 
                                                            "Unrelated \nprior crime"))

variance_x_axis <- scale_x_discrete(breaks=c('eta', 'tau', 'sigma'),
                                    limits=c('eta', 'tau', 'sigma'),
                                    labels=c('Within Group\nAcross Scenarios', 
                                             'Within Scenario\nAcross Subjects', 
                                             'Within Subject\nAcross Trials'))