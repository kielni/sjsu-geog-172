# Setup

## texts

  - Wickham, Hadley & Garret Grolemund. [R for Data Science](http://r4ds.had.co.nz)
  - Lovelace, Robin, Jakub Nowosad & Jannes Muenchow. [Geocomputation with R](https://geocompr.robinlovelace.net)
  - Walker, Kyle.[Analyzing US Census Data: Methods, Maps, and Models in R](https://walker-data.com/census-r/)
  - Wilke, Claus O. [Fundamentals of Data Visualization](https://clauswilke.com/dataviz)

## R

https://rstudio.github.io/renv/articles/renv.html

```
renv::update()
renv::snapshot()
```

## VSCode

[R Extension for Visual Studio Code](https://github.com/REditorSupport/vscode-R/wiki/Installation:-macOS)

  - + [VSCode-R-Debugger extension](https://marketplace.visualstudio.com/items?itemName=RDebugger.r-debugger) `RDebugger.r-debugger`

Set up with `renv`
```
install.packages("renv")
renv::init()
install.packages("languageserver")
install.packages("rmarkdown")
install.packages("remotes")
remotes::install_github("ManuelHentschel/vscDebugger")
remotes::install_github("nx10/httpgd")
install.packages("lintr")
install.packages("styler")
renv::snapshot(type = "all", prompt = FALSE)
```

for course

```
install.packages(c("tidyverse", "tidycensus", "sf", "tmap"))
```

lint

```
lintr::lint_dir()
styler::style_dir()
```

dev process
  - usethis for scaffolding
  - devtools for build/test workflows
  - roxygen2 for documentation
