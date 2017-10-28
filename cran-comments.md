## Test environments
* local OS X install, R 3.4.2
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
  Maintainer: ‘Willem Sleegers <w.sleegers@me.com>’
  
  *This is my first R package submission.*

## CRAN review notes

* Thanks, we see:

  The Title field starts with the package name.
  The Title field should be in title case, current version then in title case:
  'tidystats: Create a Tidy Statistics Output File'
  'Create a Tidy Statistics Output File'

  Please also omit the package name from the title.

  *The package name has been removed, which also makes it title case.*

* Description: Set of functions to produce a data file containing the output

  "Set of functions" is really redundanr.

  *This part has been removed from the Description.*


* Supported statistical functions are: 't.test()', 'cor.test()', 'lm()', 'aov()', 'anova()'. 

  Please omit the single quotes for function names when you write parentheses anyway.

  *The single quotes have been removed.*