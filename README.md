Models, analysis, and reproducible results for the paper ["Modeling the effects of crime type and evidence on judgments about guilt"](https://link.tbd).

# What you need (dependencies):
We used R via [RStudio](https://www.rstudio.com/). We also make heavy use of the [Stan](http://mc-stan.org/) probabilistic programming language and the [tidyverse](https://tidyverse.tidyverse.org/index.html).

# About the data

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