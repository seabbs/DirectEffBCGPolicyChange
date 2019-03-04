
default: all

DOCS = $(wildcard docs/*)

all: build_data build_package build_vignettes README.md build_site

#Update data
.PHONY: build_data
build_data:
		cd data-raw && make


#build update documents and build package
.PHONY: build_package
build_package:
     Rscript -e 'devtools::document()'
		 Rscript -e 'devtools::install()'

#update readme
README.md: README.Rmd
		Rscript -e 'rmarkdown::render("README.Rmd")'
		rm README.html


#Update vignettes
.PHONY: build_vignettes
build_vignettes:
		cd vignettes && make

#build pkgdown site
build_site: 
		Rscript -e 'pkgdown::build_site()'