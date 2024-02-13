# updateme

{updateme} modifies `library()` to tell you if your package is up to date when
it gets loaded: 




### Before announce
- handle tidyverse loading message
- reasonable unit testing

### Before CRAN
- bioconductor support

### One day
- add timeouts, and option to control
- check for internet connection more intelligently. In some cases, could still
  use the `available.packages()` cache, even if internet is down. Currently,
  this doesn't happen. 
