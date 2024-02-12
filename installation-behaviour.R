library(dplyr)
library(purrr)
`%||%` <- function(x, y) if (is.null(x)) y else x

installation_methods <- tibble::tribble(
  ~ Source, ~Function, ~Package, ~ Call,
        "CRAN",        "install.packages",    "lubridate", 'install.packages("lubridate")',
        "CRAN",                "pak::pak",    "lubridate", 'pak::pak("lubridate")',
        "CRAN",    "BiocManager::install",    "lubridate", 'BiocManager::install("lubridate")',
  "r-universe",        "install.packages",    "lubridate", "install.packages('lubridate', repos = c('https://tidyverse.r-universe.dev', 'https://cloud.r-project.org'))",
  "r-universe",                "pak::pak",    "lubridate", "withr::with_options(list(repos = c('https://tidyverse.r-universe.dev', 'https://cloud.r-project.org')), pak::pak('lubridate'))",
  "r-universe",    "BiocManager::install",    "lubridate", "BiocManager::install('lubridate', site_repository = 'https://tidyverse.r-universe.dev')",
      "GitHub",                "pak::pak",    "lubridate", 'pak::pak("tidyverse/lubridate")',
      "Github", "remotes::install_github",    "lubridate", 'remotes::install_github("tidyverse/lubridate")',
     #  "GitLab", Come back to this
         "Git",                "pak::pak",    "lubridate", "pak::pak('git::https://github.com/tidyverse/lubridate')" ,
         "Git",    "remotes::install_git",    "lubridate", "remotes::install_git('https://github.com/tidyverse/lubridate')",
        "Bioc",        "install.packages", "BiocGenerics", "install.packages('BiocGenerics', repos = BiocManager::repositories())",
        "Bioc",    "BiocManager::install", "BiocGenerics", 'BiocManager::install("BiocGenerics")',
        "Bioc",                "pak::pak", "BiocGenerics", "pak::pak('bioc::BiocGenerics')",
       "Local",       "devtools::install",     "updateme", "devtools::install()",
       "Local",                "pak::pak",     "updateme", "pak::pak('.')"
)


get_installation_info <- function(package, fun, expr) {
  cli::cli_h1("Installing {.pkg {package}} using {.fun {fun}}")

  tryCatch(
    remove.packages(package),
    error = function(e) cli <- <- <- <- |> |> |>
