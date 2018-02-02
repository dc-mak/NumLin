.SUFFIXES: .pdf .tex .bib $(SUFFIXES)

FLAGS := -quiet

TARGETS :=  semantics.pdf \
	    proposal.pdf

BUILD := build

all : $(TARGETS)

%.pdf: %.tex | $(BUILD) $(BUILD_AUX)
	latexmk -pdf -dvi- -ps- $< -shell-escape -output-directory=$(BUILD) $(FLAGS)
	mv $(BUILD)/$@ $@

semantics.tex: semantics.ott
	ott -i $< -o $@

$(BUILD_AUX): $(BUILD)
$(BUILD):
	mkdir -p $(BUILD)

.PHONY: clean
clean:
	rm -rf $(BUILD)
