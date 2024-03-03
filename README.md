# updateme

<!-- badges: start -->
[![R-CMD-check](https://github.com/wurli/updateme/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/wurli/updateme/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

{updateme} modifies `library()` to tell you if your packages are up to date when
you load them:

<!-- GitHub link used b/c pkgdown doesn't like file path -->
![](https://raw.githubusercontent.com/wurli/updateme/main/updateme-demo.gif)

## Installation

{updateme} is not yet on CRAN, but you can install it from GitHub using {pak}:
``` r
# install.packages("pak")
pak::pak("wurli/updateme")
```

## Usage

To use {updateme}, simply call `library(updateme)` before loading other 
packages. You may find you'd like to have {updateme} available all the time;
in this case, consider adding this snippet to your `.Rprofile`:

``` r
# If {updateme} isn't installed...
if (!requireNamespace("updateme", quietly = TRUE)) {

  # If {pak} isn't installed...
  if (!requireNamepsace("pak", quietly = TRUE)) {
  
    # Install {pak} from CRAN
    install.packages("pak")
  }
  
  # Use {pak} to install {updateme} from GitHub
  pak::pak("wurli/updateme")
}

library(updateme)
```

## Configuration

### Package sources

By default, new versions of packages will be looked up from the location
where they seem to have been installed from. If you installed from CRAN
(e.g. using `install.packages()`), {updateme} will check CRAN for a newer
version, and similarly with packages installed from Bioconductor, GitHub, and 
GitLab.

Occasionally you may want more control over where {updateme} looks for new
package versions. You can configure this behaviour, including turning {updateme}
off for particular packages, using `updateme_sources_set()`. If you want to 
temporarily disable {updateme} entirely you can do so using `updateme_off()`.

### Caching

Some caching of downloads is used to make most calls to `library()` pretty
quick. The cache resets every hour, but you can configure this like so:

``` r
# Set the cache expiry time to 30 minutes
options(updateme.cache_expiry = 30)
```
