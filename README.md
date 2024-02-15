# updateme

<!-- badges: start -->
[![R-CMD-check](https://github.com/wurli/updateme/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/wurli/updateme/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

{updateme} modifies `library()` to tell you if your package is up to date when
it gets loaded:

<!-- GitHub link used b/c pkgdown doesn't like file path -->
![](https://raw.githubusercontent.com/wurli/updateme/main/updateme-demo.gif)

## Installation

Installation can be done from GitHub using {pak}:
``` r
# install.packages("pak")
pak::pak("wurli/updateme")
```

## Usage

To use {updateme}, simply call `library(updateme)` before loading other 
packages. To use {updateme} everywhere, you can do this in your 
`.Rprofile` startup script.

## Configuration

### Package sources

By default, new versions of packages will be looked up from the location
where they seem to have been installed from. If you installed from CRAN
(e.g. using `install.packages()`), {updateme} will check CRAN for a newer
version, and similarly with packages installed from GitHub.

You can configure where the latest versions of packages are looked up from
using `updateme_sources_set()`:

``` r
# If you want to check non-standard repos for new versions of packages,
# you'll first have to set the repos global option. Note that each
# option must be named for compatibility with {updateme}
options(repos = c(

  # Your default repos, e.g. c(CRAN = "https://cloud.r-project.org")
  getOption("repos"),

  # The tidyverse r-universe, including dev versions of tidyverse packages
  tidyverse = "https://tidyverse.r-universe.dev",

  # The r-lib r-universe, including dev versions of infrastructure packages
  # like {cli}, {rlang}, etc
  rlib = "https://r-lib.r-universe.dev"
))

# 1. New versions will first be looked up in the tidyverse r-universe
# 2. If not found, they will be looked up from your usual CRAN mirror
# 3. {bslib} will always be first looked up from GitHub
# 4. {cli} will always be first looked up from the r-lib r-universe
updateme_sources_set(
  "tidyverse",
  "CRAN",
  bslib = "https://github.com/rstudio/bslib", # Name is optional here
  cli = "rlib"
)
```

Packages installed from Bioconductor, or from other remotes such as GitLab,
Bitbucket, etc, are not yet supported.

### Caching

Some caching of downloads is used to make most calls to `library()` pretty
quick. The cache resets every hour, but you can configure this like so:

``` r
# Set the cache expiry time to 30 minutes
options(updateme.cache_expiry = 30)
```
