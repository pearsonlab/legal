FIGDIR = figs
DATADIR = data
DATAFILE = $(DATADIR)/combined_data.csv
GROUPS = mturk legal lsba ilsa
FIGFILES = $(foreach fig, 1 2 3 4, $(FIGDIR)/figure_paper_$(fig).pdf)
OUT_STEM = $(DATADIR)/stan_model_output
POST_STEM = $(DATADIR)/stan_postprocess
SV_OUTS = $(foreach grp, $(GROUPS), $(OUT_STEM)_sv_$(grp)_t.rdata)
2V_OUTS = $(foreach grp, $(GROUPS), $(OUT_STEM)_2v_$(grp)_t.rdata)

all: $(FIGFILES) docs/supplement.pdf

figs: $(FIGFILES)

supplement: docs/supplement.pdf

$(FIGDIR)/figure_paper_%.pdf: models
	Rscript make_paper_figure_$*.R

docs/supplement.pdf: sv_models 2v_models mv_models demos_models
	Rscript -e "library(rmarkdown); render('docs/supplement.Rmd', 'pdf_document')"

# Model classes
models: sv_models 2v_models mv_models demos_models $(DATADIR)/stan_postprocess_ci.rdata

sv_models: $(POST_STEM)_sv_t.rdata 

2v_models: $(POST_STEM)_2v_t.rdata 

mv_models: $(POST_STEM)_mv_t.rdata 

demos_models: $(POST_STEM)_demos_t.rdata

# Postprocessed model outputs
$(POST_STEM)_sv_t.rdata: $(SV_OUTS)
	Rscript postprocess_model_data.R $^

$(POST_STEM)_2v_t.rdata: $(2V_OUTS)
	Rscript postprocess_model_data.R $^

$(POST_STEM)_mv_t.rdata: $(OUT_STEM)_mv_mturk_t.rdata
	Rscript postprocess_model_data.R $^

$(POST_STEM)_demos_t.rdata: $(OUT_STEM)_demos_mturk_t.rdata
	Rscript postprocess_model_data.R $^

$(DATADIR)/stan_postprocess_ci.rdata: $(SV_OUTS) $(OUT_STEM)_mv_mturk_t.rdata
	Rscript postprocess_for_intervals.R
	
# Model outputs 
$(OUT_STEM)_%.rdata: $(DATAFILE)
	Rscript run_models.R $(subst _, ,$*)
