FIGDIR = figs
DATADIR = data
GROUPS = mturk legal lsba ilsa
OUT_STEM = $(DATADIR)/stan_model_output_hier_t_
POST_STEM = $(DATADIR)/stan_hier_postprocess
HIER_OUTS = $(foreach grp, $(GROUPS), $(OUT_STEM)$(grp).rdata)
HIER_MV_OUTS = $(foreach grp, $(GROUPS), $(OUT_STEM)multi_$(grp).rdata)

all: figs supplement

figs: models

supplement: models

models: hier_models hier_models_mv hier_models_mv_all hier_models_with_demos

# Model classes
hier_models: $(POST_STEM).rdata
	Rscript postprocess_stan_hier_data.R

hier_models_mv: $(POST_STEM)_multi.rdata
	Rscript postprocess_stan_hier_multi_data.R

hier_models_mv_all: $(POST_STEM)_multi_all.rdata
	Rscript postprocess_stan_hier_multi_all_data.R

hier_models_with_demos: $(OUT_STEM)mturk_with_demos.rdata

# Postprocessed model outputs
$(POST_STEM).rdata: $(HIER_OUTS)

$(POST_STEM)_multi.rdata: $(HIER_MV_OUTS)

$(POST_STEM)_multi_all.rdata: $(OUT_STEM)multi_all.rdata

# Model outputs (in order of most to least specific)
$(OUT_STEM)mturk_with_demos.rdata:
	Rscript run_hier_model_with_demos.R

$(OUT_STEM)multi_all.rdata: 
	Rscript run_hier_model_multivariate_all.R

$(OUT_STEM)multi_%.rdata:
	Rscript run_hier_model_multivariate.R $*
	
$(OUT_STEM)%.rdata:
	Rscript run_hier_model.R $*

	
foo:
	$(info $(HIER_OUTS))