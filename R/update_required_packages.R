#' Update the required packages to their development versions
#'
#' This function checks if the required packages are installed and up-to-date.
#' If not, it asks the user if they want to install the development versions
#' from GitHub.
#' @param allow_update Logical. If `TRUE`, the function will install the
#' development versions from GitHub. The default in `FALSE`
#' @return Logical. `TRUE` if all required packages are installed and up-to-date.
#' @export
update_required_packages <- function(allow_update = FALSE) {

  current <- c("dscore" = "1.9.8",
               "dmetric" = "0.68.1",
               "gseddata" = "1.9.0",
               "gsedread" = "0.8.0",
               "gsedscripts" = "0.16.0")
  OK <- TRUE

  if (!requireNamespace("dscore", quietly = TRUE) ||
      packageVersion("dscore") < current[["dscore"]]) {
    if (allow_update) {
      remotes::install_github("d-score/dscore")
    } else {
      warning("Package dscore is not installed or not up-to-date. Please install version ", current[["dscore"]])
      OK <- FALSE
    }
  }

  if (!requireNamespace("dmetric", quietly = TRUE) ||
      packageVersion("dmetric") < current[["dmetric"]]) {
    if (allow_update) {
      remotes::install_github("d-score/dmetric")
    } else {
      warning("Package dmetric is not installed or not up-to-date. Please install version ", current[["dmetric"]])
      OK <- FALSE
    }
  }

  if (!requireNamespace("gseddata", quietly = TRUE) ||
      packageVersion("gseddata") < current[["gseddata"]]) {
    if (allow_update) {
      remotes::install_github("d-score/gseddata")
    } else {
      warning("Package gseddata is not installed or not up-to-date. Please install version ", current[["gseddata"]])
      OK <- FALSE
    }
  }

  if (!requireNamespace("gsedread", quietly = TRUE) ||
      packageVersion("gsedread") < current[["gsedread"]]) {
    if (allow_update) {
      remotes::install_github("d-score/gsedread")
    } else {
      warning("Package gsedread is not installed or not up-to-date. Please install version ", current[["gsedread"]])
      OK <- FALSE
    }
  }

  if (!requireNamespace("gsedscripts", quietly = TRUE) ||
      packageVersion("gsedscripts") < current[["gsedscripts"]]) {
    if (allow_update) {
      remotes::install_github("d-score/gsedscripts")
    } else {
      warning("Package gsedscripts is not installed or not up-to-date. Please install version ", current[["gsedscripts"]])
      OK <- FALSE
    }
  }

  if (!OK) warning("Use 'update_required_packages(allow_update = TRUE)' to update.")
  return(OK)
}
