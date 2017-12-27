FLAGS := -quiet

TARGETS := proposal.pdf

BUILD := build

all : $(TARGETS)

%.pdf: %.tex | $(BUILD)
	latexmk -pdf -dvi- -ps- $< -shell-escape -output-directory=$(BUILD) $(FLAGS)
	mv $(BUILD)/$@ $@

$(BUILD):
	mkdir -p $(BUILD)

clean: rm -rf $(BUILD)
