lint:
	Rscript -e "lintr::lint_dir()"
	Rscript -e 'styler::style_dir()'