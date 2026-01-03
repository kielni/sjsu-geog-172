# setup
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

# course
install.packages(c("tidyverse", "tidycensus", "sf", "tmap"))
