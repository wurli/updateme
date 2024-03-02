# GitHub -----------------------------------------------------------------------
# paste(
#   "https://docs.github.com/en/enterprise-cloud@latest/admin/",
#   "identity-and-access-management/iam-configuration-reference/",
#   "username-considerations-for-external-authentication#about-username-normalization
# )
# https://stackoverflow.com/a/59082561/5633063
# Usernames only alphanumeric and dashes
# Repo names only alphanumeric, dashes, dots, and underscores
.github_username_pattern <- "[a-zA-Z0-9-]+"
.github_repo_pattern     <- "[a-zA-Z0-9_.-]+"

is_github_url <- function(x) {
  grepl(x = x, paste0(
    "^\\s*https://github\\.com/",
    .github_username_pattern, "/", .github_repo_pattern,
    "\\s*$"
  ))
}

github_url_check <- function(x) {
  check_repo_url(x, is_github_url, "GitHub", "https://github.com/username/repo")
}

github_username_from_url <- function(x) {
  github_url_check(x)
  pat <- paste0(
    "\\s*https://github\\.com/",
    "(", .github_username_pattern, ")", # Capture group
    "/", .github_repo_pattern,
    "\\s*$"
  )
  sub(pat, "\\1", x)
}

github_repo_from_url <- function(x) {
  github_url_check(x)
  pat <- paste0(
    "\\s*https://github\\.com/",
    .github_username_pattern, "/",
    "(", .github_repo_pattern, ")", # Capture group
    "\\s*$"
  )
  sub(pat, "\\1", x)
}

github_url_make <- function(username, repo) {
  paste0("https://github.com/", username, "/", repo)
}

# Gitlab -----------------------------------------------------------------------
# https://docs.gitlab.com/ee/user/reserved_names.html
# "
#   Project names can contain only letters, digits, emoji,
#     underscores, dots, pluses, dashes, or spaces.
#   Group names can contain only letters, digits, emoji,
#     underscores, dots, parentheses (), dashes, or spaces.
# "
# So, basically anything. I don't want to write a regex to capture every
# emoji ever, so just allow anything besides ], [, /, \, {, }
.gitlab_username_pattern <- r"([^][\/{}()]+)"
.gitlab_repo_pattern     <- r"([^][\/{}]+)"

# print(is_gitlab_url("https://gitlab.com/some (crazy.org name\U270C/ some crazy Pr0j.name \U270C"))
is_gitlab_url <- function(x) {
  grepl(x = x, paste0(
    "^\\s*https://gitlab\\.com/",
    .gitlab_username_pattern, "/", .gitlab_repo_pattern,
    "\\s*$"
  ))
}


gitlab_url_check <- function(x) {
  check_repo_url(x, is_gitlab_url, "GitLab", "https://gitlab.com/username/repo")
}

gitlab_username_from_url <- function(x) {
  gitlab_url_check(x)
  pat <- paste0(
    "\\s*https://gitlab\\.com/",
    "(", .gitlab_username_pattern, ")", # Capture group
    "/", .gitlab_repo_pattern,
    "\\s*$"
  )
  sub(pat, "\\1", x)
}

gitlab_repo_from_url <- function(x) {
  gitlab_url_check(x)
  pat <- paste0(
    "\\s*https://gitlab\\.com/",
    .gitlab_username_pattern, "/",
    "(", .gitlab_repo_pattern, ")", # Capture group
    "\\s*$"
  )
  sub(pat, "\\1", x)
}


gitlab_url_make <- function(username, repo) {
  paste0("https://gitlab.com/", username, "/", repo)
}

# Utils ------------------------------------------------------------------------
check_repo_url <- function(x, checker, name, correct) {
  if (!checker(x))
    cli::cli_abort(c(
      "Incorrectly formed {name} URL {.url {x}}",
      i = "URLs should have format {.val {correct}}"
    ))
  invisible(NULL)
}
