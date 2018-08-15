FIGDIR = figs
DATADIR = data
DATAFILE = $(DATADIR)/combined_data.csv
GROUPS = mturk legal lsba ilsa
FIGFILES = $(foreach fig, 1 2 3 4, $(FIGDIR)/figure_paper_$(fig).pdf)
OUT_STEM = $(DATADIR)/stan_model_output
POST_STEM = $(DATADIR)/stan_postprocess
SV_OUTS = $(foreach grp, $(GROUPS), $(OUT_STEM)_sv_$(grp)_t.rdata)
2V_OUTS = $(foreach grp, $(GROUPS), $(OUT_STEM)_2v_$(grp)_t.rdata)
MODELS = $(POST_STEM)_sv_t.rdata $(POST_STEM)_2v_t.rdata $(POST_STEM)_mv_t.rdata\
$(POST_STEM)_demos_t.rdata $(DATADIR)/stan_postprocess_ci.rdata

all: $(FIGFILES) docs/supplement.pdf

figs: $(FIGFILES)

supplement: docs/supplement.pdf

$(FIGDIR)/figure_paper_%.pdf: $(MODELS)
	Rscript make_paper_figure_$*.R

docs/supplement.pdf: $(MODELS)
	Rscript -e "library(rmarkdown); render('docs/supplement.Rmd', 'pdf_document')"

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

