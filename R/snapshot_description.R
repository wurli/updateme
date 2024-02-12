# IDEA:
#   - For more sophisticated resolution of where packages have been downloaded
#     from, could just copy the method used by renv. This is definitely non-
#     trivial, though. Even just assuming everything has come from CRAN would
#     probably solve 95% of uses. Including BioConductor and GitHub probably
#     solves 99%. The other 1% can be graceful failures for now.
#
#   - Doing something more complicated might be worth it in the future though,
#     since this probably _would_ lead to an improvement.
#
# Based on renv_snapshot_description()
if (FALSE) {

get_installation_info <- function(package) {
  package = "tibble"

  # 2. Read the description
  dcf <- read_description(file.path(.libPaths(), package, "DESCRIPTION"))

  # 3. Get the fields

  source <- renv_snapshot_description_source(dcf)
  dcf[names(source)] <- source

  git <- grep("^git", names(dcf), value = TRUE)
  remotes <- grep("^Remote", names(dcf), value = TRUE)

  cranlike <- is.null(dcf[["RemoteType"]]) ||
    identical(dcf[["RemoteType"]], "standard")

  out_fields <- c(
    "Package", "Version", "Source", "Repository", "OS_type",
    if (!cranlike) c(remotes, git)
  )

  dcf[intersect(out_fields, names(dcf))]

}

read_description <- function(path) {
  dcf <- read.dcf(path)
  setNames(as.list(dcf), colnames(dcf))
}





renv_snapshot_description_source_custom <- function(dcf) {

  # check for 'standard' remote type
  type  <- dcf[["RemoteType"]]
  if (!identical(type, "standard"))
    return(NULL)

  # check for a declared repository URL
  remoterepos <- dcf[["RemoteRepos"]]
  if (is.null(remoterepos))
    return(NULL)

  # if this package appears to have been installed from a
  # repository which we have knowledge of, skip
  repos <- as.list(getOption("repos"))
  repository <- dcf[["Repository"]]
  if (!is.null(repository) && repository %in% names(repos))
    return(NULL)

  # check whether this repository is already in use;
  # if so, we can skip declaring it
  name <- dcf[["RemoteReposName"]]
  isset <- if (is.null(name))
    remoterepos %in% repos
  else
    name %in% names(repos)

  if (isset)
    return(NULL)

  list(Source = "Repository", Repository = remoterepos)

}

renv_snapshot_description_source <- function(dcf) {

  # check for packages installed from a repository not currently
  # encoded as part of the user's repository option, and include if required
  source <- renv_snapshot_description_source_custom(dcf)
  if (!is.null(source))
    return(source)

  # check for a declared remote type
  # treat 'standard' remotes as packages installed from a repository
  # https://github.com/rstudio/renv/issues/998
  type <- dcf[["RemoteType"]]
  if (identical(type, "standard")) {

    # if this is a 'standard' Bioconductor remote, then encode it as such
    if (!is.null(dcf[["biocViews"]]))
      return(list(Source = "Bioconductor"))

    # otherwise, check for custom repository information
    repository <- dcf[["RemoteReposName"]] %||% dcf[["Repository"]]
    if (!is.null(repository))
      return(list(Source = "Repository", Repository = repository))

  } else if (!is.null(type)) {
    return(list(Source = alias(type)))
  }

  # packages from Bioconductor are normally tagged with a 'biocViews' entry;
  # use that to infer a Bioconductor source
  if (!is.null(dcf[["biocViews"]]))
    return(list(Source = "Bioconductor"))

  # check for a declared repository
  repository <- dcf[["Repository"]]
  if (!is.null(repository))
    return(list(Source = "Repository", Repository = repository))

  # check for a valid package name
  package <- dcf[["Package"]]
  if (is.null(package))
    return(list(Source = "unknown"))

  # if this is running as part of the synchronization check, skip CRAN queries
  # https://github.com/rstudio/renv/issues/812
  if (the$project_synchronized_check_running)
    return(list(Source = "unknown"))

  # NOTE: this is sort of a hack that allows renv to declare packages which
  # appear to be installed from sources, but are actually available on the
  # active R package repositories, as though they were retrieved from that
  # repository. however, this is often what users intend, especially if
  # they haven't configured their repository to tag the packages it makes
  # available with the 'Repository:' field in the DESCRIPTION file.
  #
  # still, this has the awkward side-effect of a package's source potentially
  # depending on what repositories happen to be active at the time of snapshot,
  # so it'd be nice to tighten up the logic here if possible
  #
  # NOTE: local sources are also searched here as part of finding the 'latest'
  # available package, so we need to handle local packages discovered here
  tryCatch(
    renv_snapshot_description_source_hack(package, dcf),
    error = function(e) list(Source = "unknown")
  )

}

renv_snapshot_description_source_hack <- function(package, dcf) {

  # check cellar
  for (type in renv_package_pkgtypes()) {
    cellar <- renv_available_packages_cellar(type)
    if (package %in% cellar$Package)
      return(list(Source = "Cellar"))
  }

  # check available packages
  latest <- catch(renv_available_packages_latest(package))
  if (is.null(latest) || inherits(latest, "error"))
    return(list(Source = "unknown"))

  # check version; use unknown if it's too new
  if (renv_version_gt(dcf[["Version"]], latest[["Version"]]))
    return(list(Source = "unknown"))

  # ok, this package appears to be from a package repository
  list(Source = "Repository", Repository = latest[["Repository"]])

}

}
