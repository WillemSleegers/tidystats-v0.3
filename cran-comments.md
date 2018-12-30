## Test environments
* local OS X install, R 3.5.2
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs or NOTES. 

## Additional notes
I have added the feature that users can set a default tidystats list via `options()`. The argument is called 'tidystats_list'. Assuming 'results' is a tidystats list, one can set the default list with `options(tidystats_list = results)`. However, I'm not sure whether my current implementation and examples are consistent with CRAN policy. If not, I gladly hear about best practices for this kind of feature.
