.SUFFIXES: .pdf .tex .bib $(SUFFIXES)

FLAGS := -quiet

TARGETS :=  semantics.pdf \
	    semantics2.pdf \
	    proposal.pdf  \
	    dissertation.pdf

BUILD := build

# Add Bibligraphy at some point
SRC :=	myclass.cls \
	titlepage.tex \
	declaration.tex \
	abstract.tex \
	introduction.tex \
	background.tex \
	semantics_def.tex \
	implementation.tex \
	evaluation.tex \
	related_work.tex \
	conclusion.tex \
	ott_spec.tex \
	timings.tex \
	primitives.tex \
	eval_data.tex \
	timings_all.tex

all: $(TARGETS)

$(TARGETS):%.pdf: %.tex | $(BUILD) $(BUILD_AUX)
	latexmk -pdf -dvi- -ps- $< -shell-escape -output-directory=$(BUILD) $(FLAGS)
	mv $(BUILD)/$@ $@

dissertation.tex: $(SRC)
	touch $@

semantics.tex: semantics_def.tex
	touch $@

semantics_def.tex: semantics.ott
	ott -i $< -tex_show_meta false -tex_wrap false -o $@

semantics2.tex: semantics_def2.tex
	touch $@

semantics_def2.tex: semantics2.ott
	ott -i $< -tex_show_meta false -tex_wrap false -o $@

$(BUILD_AUX): $(BUILD)
$(BUILD):
	mkdir -p $(BUILD)

.PHONY: clean
clean:
	rm -rf $(BUILD)
