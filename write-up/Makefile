.SUFFIXES: .pdf .tex .bib $(SUFFIXES)

FLAGS := -quiet

BUILD := build

# Proposal

PROP := proposal.bib
PROP_DIR := proposal
PROP := $(addprefix $(PROP_DIR)/,$(PROP))

# Dissertation

DIS :=  myclass.cls \
        semantics_def.tex \
        titlepage.tex \
        declaration.tex \
        abstract.tex \
        introduction.tex \
        background.tex \
        implementation.tex \
        evaluation.tex \
        related_work.tex \
        conclusion.tex \
        ott_spec.tex \
        timings.tex \
        primitives.tex \
	proposal.bib \
        eval_data.tex \
        timings_all.tex \
        timings.txt \
        trace.txt

DIS_DIR := dissertation
DIS := $(addprefix $(DIS_DIR)/,$(DIS))

# Semantics

SEM := semantics_def.tex \
       interpretation.tex \
       lemmas.tex \
       soundness.tex

SEM_DIR := semantics
SEM := $(addprefix $(SEM_DIR)/,$(SEM))

# Paper

EXAMPLES := factorial.lt \
            sum_array.lt \
            weighted_avg_infer.lt \
            square.lt \
            lin_reg.lt \
            l1_norm_min.lt \
            kalman.lt
EXAMPLES :=  $(addprefix ../examples/,$(EXAMPLES))

PAPER := intro.tex \
         lang_and_examples.tex \
         formal_system.tex \
         implementation.tex \
         discussion.tex \
         appendix.tex \
         lin_reg_timings.tex \
         l1_norm_min_timings.tex \
         kalman_timings.tex \
         ourbib.bib \
         lipics-v2019.cls \
         fig1.eps \
         plainurl.bst

PAPER_DIR := paper
PAPER := $(addprefix $(PAPER_DIR)/,$(PAPER))

# Isolated paper for submission

ISOLATED := lipics-v2019.cls \
            ottlayout.sty \
            pf2.sty \
            orcid.pdf \
            cc-by.pdf \
            lipics-logo-bw.pdf \
            impl_build.png \
            ourbib.bib \
            plainurl.bst

ISOLATED_DIR := isolated

ISOLATED := $(addprefix $(PAPER_DIR)/,$(ISOLATED)) $(EXAMPLES)

BUILD_DIS := $(BUILD)/$(DIS_DIR)

PDFS := $(SEM_DIR).pdf \
        $(PROP_DIR).pdf  \
        $(DIS_DIR).pdf \
        $(PAPER_DIR).pdf

.PHONY: all clean
all: $(PDFS) $(ISOLATED_DIR).tar.gz

.SECONDEXPANSION:
$(PDFS):%.pdf: $$*/$$*.tex | $(BUILD)
	cd $(dir $<) && latexmk -pdf -dvi- -ps- $(notdir $<) -shell-escape -output-directory=../$(BUILD) $(FLAGS)
	mv $(BUILD)/$@ $@

$(PAPER_DIR)/$(PAPER_DIR).tex: $(PAPER) $(SEM) $(EXAMPLES)
	touch $@

$(DIS_DIR)/$(DIS_DIR).tex: $(DIS) | $(BUILD_DIS)
	touch $@

$(DIS_DIR)/semantics_def.tex: $(DIS_DIR)/semantics.ott
	ott -i $< -tex_show_meta false -tex_wrap false -o $@

$(SEM_DIR)/$(SEM_DIR).tex: $(SEM)
	touch $@

$(PROP_DIR)/$(PROP_DIR).tex: $(PROP)
	touch $@

$(SEM_DIR)/semantics_def.tex: $(SEM_DIR)/semantics.ott
	ott -i $< -tex_show_meta false -tex_wrap false -o $@

.SECONDEXPANSION:
$(ISOLATED_DIR).tar.gz:%.tar.gz: $$*/$$*.tex $(ISOLATED) | $(ISOLATED_DIR)
	cp $(ISOLATED) $(dir $<)
	tar czf $@ $(dir $<)

$(ISOLATED_DIR)/$(ISOLATED_DIR).tex: $(PAPER_DIR)/$(PAPER_DIR).tex $(ISOLATED) | $(ISOLATED_DIR)
	cd $(dir $<) && cat $(notdir $<) | sed 's~^\\clearpage\\appendix\\input{appendix}$$~%&~' | latexpand - > ../$@
	sed -i 's~\[outputdir=\.\./$(BUILD)\]~~' $@
	sed -i 's~\.\./\.\./examples/~~' $@
	sed -i 's/Figure~\\ref{fig:\(cblas\|ocaml\)_kalman}\|Appendix~\\ref{\w\+\:\(\w\|_\)\+}/the Appendix/g' $@
	sed -i 's/^permissions (\\ref{fracPermSub})/permissions; all of these (and more) are stated formally and proved in the Appendix/g' $@
	sed -i 's/ (\\ref{\(frame\|subsetKJ\|restriction\|fracPermSub\)})//g' $@
	sed -i 's/\\ref{fracPermSub}/one of the lemmas/g' $@

$(BUILD_DIS): $(BUILD)
$(BUILD) $(BUILD_DIS) $(ISOLATED_DIR): %:
	mkdir -p $@

clean:
	rm -rf $(BUILD) $(ISOLATED_DIR)
