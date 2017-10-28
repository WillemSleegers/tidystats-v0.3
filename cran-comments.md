## Test environments
* local OS X install, R 3.4.2
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
  Maintainer: ‘Willem Sleegers <w.sleegers@me.com>’
  
  This is my first R package submission.

## CRAN review notes

* Thanks, perhaps only:
  Tidy Statistics Output File (that this file is produced, should be clear)

  	*I changed the title to tidystats: Create a Tidy Statistics Output File. I believe this makes it more clear that the package is about the user producing such a file, rather than it being done automatically.*

* Please elaborate which statistics are supported in the actual package version.

  	*This is added to the Description in DESCRIPTION*

* Please write package names and software names in single quotes (e.g. 'tidyverse').

  	*I have changed this in the DESCRIPTION file*

* Please add more small executable examples in your Rd-files.

  	*Many more examples have been added. Each function has an example, with the more important functions (e.g., generic functions) having multiple examples*

* Your function write_stats() is writing in the user's home directory: write_stats(results, "results.csv")

  	*My apologies, this has been changed. tempdir() is used now.*