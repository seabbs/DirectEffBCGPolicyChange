default: all
RDIR = .
VIG_DRAFTS_DIR = $(RDIR)/drafts
RESOURCES = $(RDIR)/resources/*

#update paper
PAPER = $(VIG_DRAFTS_DIR)/paper/paper.html

all: $(PAPER)

$(PAPER): paper.Rmd $(RESOURCES)
			Rscript -e 'rmarkdown::render("paper.Rmd", c("html_document", "word_document"), output_dir = "drafts/paper", output_options = list())'
