Models, analysis, and reproducible results for the paper ["Modeling the effects of crime type and evidence on judgments about guilt"](https://link.tbd).

# What you need (dependencies):
We used R via [RStudio](https://www.rstudio.com/). We also make heavy use of the [Stan](http://mc-stan.org/) probabilistic programming language and the [tidyverse](https://tidyverse.tidyverse.org/index.html).

# About the data
The data are recorded in a single file, `combined_data.csv` in the `data` folder. The file is a single table, one line per rating given, with the following columns:
- `uid`: Unique id for each participant.
- `scenario`: Integer indicating which crime was presented on a given trial: (1 - 33).
- `physical`: Which physical evidence was presented? (`No Physical`, `Non-DNA`, `DNA`)
- `history`: What criminal history information was presented? (`No History`, `Unrelated`, `Related`)
- `witness`: What eyewitness information was presented? (`No Witness`, `Yes Witness`)
- `nonwhite`: Did the participant identify as non-white? (`FALSE`, `TRUE`)
- `hispanic`: Did the participant identify as hispanic? (`FALSE`, `TRUE`)
- `female`: Did the participant identify as female? (`FALSE`, `TRUE`)
- `rating_type`: Which type of rating does the datum represent:
  - `rating`: Most common. "How strong is the case that the accused committed this crime?"
  - `rate_punishment`: Next most common when participants gave two or more ratings. "How severe should the punishment be for someone who commits a crime like this one?"
  - `rate_outrage`: "When you read about crimes like this one, how upset to you feel?"
  - `rate_threat`: "How likely is this crime to occur in your community?"
  - `rate_threat2`: "When you read about a crime like this, how concerned do you feel for your own safety, or the safety of your community?"
- `rating`: Numerical rating for the relevant question (1 - 100).
- `group`: Which experimental group the participant belonged to:
  - `mturk`: Amazon Mechanical Turk sample.
  - `legal`: Law students
  - `ilsa`: Illinois Prosecutors
  - `lsba`: Louisiana Bar

All together, the data comprise more than 135,000 ratings from 836 unique individuals.

# <makefile stuff>...

# Quick notes

Currently, Bayesian pipeline is:

- `runall.sh`: Run model on case strength outcome for each group.
  - calls `run_hier_model.R`
  - uses `model_hier_scenario.stan`
  - postprocesses with `postprocess_stan_hier_data.R`
- `runall_mv.sh`: Run model incorporating both case strength and punishment outcome for each group.
  - calls `run_hier_model_multivariate.R`
  - uses `model_hier_scenario_multivar.stan`
  - postprocesses with `postprocess_stan_hier_multi_data.R`
- `runall_mv_all.sh`: Run model incorporating all four outcomes (mTurk only)
  - calls `run_hier_model_multivariate_all.R`
  - uses `model_hier_scenario_multivar.stan`
  - postprocesses with `postprocess_stan_hier_multi_all_data.R`