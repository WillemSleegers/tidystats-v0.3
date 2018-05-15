# README --------------------------------------------------------------------------------------

library(knitr)
knit("README.Rmd")

# CRAN submission -----------------------------------------------------------------------------

# Check package
check()

# run R CMD check on CRAN’s servers
build_win()

# Build tar
build()