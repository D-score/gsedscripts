#' Require a package
#'
#' Check whether a package is installed. If not, the
#' procedure offers to install the latest version from CRAN or GitHub.
#' Then it check for a minimum version.
#' @param pkg      A string with the required package
#' @param version  A string with the minimum version, e.g. "0.1.0"
#' @param repo     A string with the GitHub repo name. For the default
#' `repo == "CRAN"` installation proceeds from CRAN.
#' @return
#' A logical indicating success (TRUE) or failure (FALSE) for version check.
#' @export
require_package <- function(pkg, version, repo = "CRAN") {
  if (requireNamespace(pkg, quietly = TRUE) &&  packageVersion(pkg) >= version) {
    return(TRUE)
  }
  if (interactive()) {
    if (repo == "CRAN") {
      answer <- askYesNo(paste("Package", pkg, "needed. Install from CRAN?"))
      if (answer) {
        install.packages(pkg, repos = "https://cloud.r-project.org/", quiet = TRUE)
      }
    } else {
      answer <- askYesNo(paste("Package", pkg, "needed. Install from GitHub?"))
      if (answer) remotes::install_github(repo)
    }
  }
  if (requireNamespace(pkg, quietly = TRUE) &&  packageVersion(pkg) >= version) {
    return(TRUE)
  }
  return(FALSE)
}
