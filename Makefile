RESUME = ./resume
SRC = ./src
POSITIONS = ./positions_data
LOGO = ./logo

## help		: help window
help:
	@ grep '^##' Makefile | sed -e 's/##//g'
	@ echo ''

## html_resume  	: generates the html resume
html_resume: $(RESUME)/resume.html 
$(POSITIONS)/experience.csv: $(SRC)/positions_data/experience.R
	@ echo 'Changes in experience source file'
	@ sleep 1.25
	@ echo 'Writing new experience.csv in /positions_data...'
	@ Rscript $(SRC)/positions_data/experience.R
	@ echo 'Done.'
	@ echo ''

$(LOGO)/ch.png: $(SRC)/logo/logo.R
	@ echo 'Changes in logo source file'
	@ sleep 1.25
	@ echo 'Writing new logo image in /logo...'
	@ Rscript $(SRC)/logo/logo.R
	@ echo 'Done.'
	@ echo ''

$(RESUME)/resume.html: $(SRC)/resume/* \
					   $(POSITIONS)/experience.csv \
					   $(LOGO)/ch.png
	@ echo 'Changes in source files for resume.html'
	@ sleep 1.25
	@ echo 'Rendering new html...'
	@ Rscript -e "rmarkdown::render(input = here::here('src', 'resume', 'resume.Rmd'), output_dir = here::here('resume'), quiet = TRUE)"
	@ echo 'Done.'
	@ echo 'New resume.html available in /resume'
	@ echo ''
