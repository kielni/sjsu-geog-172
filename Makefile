lint:
	Rscript -e 'styler::style_dir()'
	Rscript -e "lintr::lint_dir()"

.PHONY: lab1
lab1: lint
	Rscript labl/Lab1_Kimberly_Nicholls.R

.PHONY: lab3a
lab3a: lint
	quarto render lab3/lab3a.qmd --to html
	Rscript -e 'rmarkdown::rpubsUpload("Lab 3A", "lab3/lab3a.html")'

.PHONY: lab3c
lab3c: lint
	Rscript lab3/lab3c.R

.PHONY: final
final: lint
	Rscript final/final.R