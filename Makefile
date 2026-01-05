lint:
	Rscript -e "lintr::lint_dir()"
	Rscript -e 'styler::style_dir()'

.PHONY: lab1
lab1: lint
	Rscript Labl1/Lab1_Kimberly_Nicholls.R