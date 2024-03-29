# updateme

<!-- badges: start -->
[![R-CMD-check](https://github.com/wurli/updateme/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/wurli/updateme/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Installation

You can install {updateme} from CRAN like so:
``` r
install.packages("updateme")
```

## What's in the box?

{updateme} modifies `library()` to tell you if your packages are up to date when
you load them:

<!-- GitHub link used b/c pkgdown doesn't like file path -->
![](https://raw.githubusercontent.com/wurli/updateme/main/readme-files/updateme-cran-pkgs.gif)

{updateme} works with packages installed from CRAN (and CRAN-like repos),
Bioconductor, GitHub and GitLab. Packages installed from Bioconductor are
intelligently checked against the version of Bioconductor you're using, and 
these get a special message if they're more recent than they probably should be:

![](https://raw.githubusercontent.com/wurli/updateme/main/readme-files/updateme-bioc-pkgs.gif)

The tidyverse is a special case, which gets its own modified startup message:

![](https://raw.githubusercontent.com/wurli/updateme/main/readme-files/updateme-tidyverse-msg.gif)

{updateme} is configurable using
[`updateme_sources_set()`](https://wurli.github.io/updateme/reference/updateme_sources_set.html). If, for example, you're using the development version of {lubridate} but you'd 
like to see which version is currently on CRAN, you can get this behaviour like 
so:

![](https://raw.githubusercontent.com/wurli/updateme/main/readme-files/updateme-sources-set.gif)

By default, {updateme} will check for new versions of a given package from 
wherever your version seems to have been installed from. 

## Use {updateme} by default

If you find you'd like to have {updateme} available all the time, consider 
loading it at startup by adding this snippet to your `.Rprofile`:

``` r
if (interactive()) {
  suppressMessages(require(updateme))
}
```

If necessary, you can turn {updateme} off using 
[`updateme_off()`](https://wurli.github.io/updateme/reference/updateme_on.html),
and back on with 
[`updateme_on()`](https://wurli.github.io/updateme/reference/updateme_on.html).

### Caching

Some caching of downloads is used to make most calls to `library()` pretty
quick. The cache resets every hour, but you can configure this using 
`options()`:

``` r
# Set the cache expiry time to 30 minutes
options(updateme.cache_expiry = 30)
```
