FIGDIR = figs
DATADIR = data
DATAFILE = $(DATADIR)/combined_data.csv
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

docs/supplement.pdf: $(DATAFILE)
	Rscript -e "library(rmarkdown); render('docs/supplement.Rmd', 'pdf_document')"

# Model classes
models: $(POST_STEM).rdata $(POST_STEM)_multi.rdata $(POST_STEM)_multi_all.rdata \
	$(POST_STEM)_with_demos.rdata

# Postprocessed model outputs
$(POST_STEM).rdata: $(HIER_OUTS)
	Rscript postprocess_model_data.R hier

$(POST_STEM)_multi.rdata: $(HIER_MV_OUTS)
	Rscript postprocess_model_data.R mv

$(POST_STEM)_multi_all.rdata: $(OUT_STEM)multi_all.rdata
	Rscript postprocess_model_data.R mv_all

$(POST_STEM)_with_demos.rdata: $(OUT_STEM)multi_with_demos.rdata
	Rscript postprocess_model_data.R demos
	
# Model outputs (in order of most to least specific)
$(OUT_STEM)mturk_with_demos.rdata: $(DATAFILE)
	Rscript run_models.R demos

$(OUT_STEM)multi_all.rdata: $(DATAFILE)
	Rscript run_models.R mv_all 

$(OUT_STEM)multi_%.rdata: $(DATAFILE)
	Rscript run_models.R mv $*
	
$(OUT_STEM)%.rdata: $(DATAFILE)
	Rscript run_models.R hier $*
