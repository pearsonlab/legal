FIGDIR = figs
DATADIR = data
GROUPS = mturk legal lsba ilsa
FIGFILES = $(foreach fig, 1 2 3 4, $(FIGDIR)/figure_paper_$(fig).pdf)
OUT_STEM = $(DATADIR)/stan_model_output_hier_t_
POST_STEM = $(DATADIR)/stan_hier_postprocess
HIER_OUTS = $(foreach grp, $(GROUPS), $(OUT_STEM)$(grp).rdata)
HIER_MV_OUTS = $(foreach grp, $(GROUPS), $(OUT_STEM)multi_$(grp).rdata)

all: $(FIGFILES) docs/supplement.pdf

figs: $(FIGFILES)

supplement: docs/supplement.pdf

$(FIGDIR)/figure_paper_%.pdf: models
	Rscript make_paper_figure_$*.R

docs/supplement.pdf:
	Rscript -e "library(rmarkdown); render('docs/supplement.Rmd', 'pdf_document')"

# Model classes
models: $(POST_STEM).rdata $(POST_STEM)_multi.rdata $(POST_STEM)_multi_all.rdata \
	$(OUT_STEM)mturk_with_demos.rdata

# Postprocessed model outputs
$(POST_STEM).rdata: $(HIER_OUTS)
	Rscript postprocess_stan_hier_data.R

$(POST_STEM)_multi.rdata: $(HIER_MV_OUTS)
	Rscript postprocess_stan_hier_multi_data.R

$(POST_STEM)_multi_all.rdata: $(OUT_STEM)multi_all.rdata
	Rscript postprocess_stan_hier_multi_all_data.R

# Model outputs (in order of most to least specific)
$(OUT_STEM)mturk_with_demos.rdata:
	time Rscript run_hier_model_with_demos.R

$(OUT_STEM)multi_all.rdata: 
	time Rscript run_hier_model_multivariate_all.R

$(OUT_STEM)multi_%.rdata:
	time Rscript run_hier_model_multivariate.R $*
	
$(OUT_STEM)%.rdata:
	time Rscript run_hier_model.R $*
