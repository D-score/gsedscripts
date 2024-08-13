#' Update the required packages to their development versions
#'
#' This function checks if the required packages are installed and up-to-date.
#' If not, it asks the user if they want to install the development versions
#' from GitHub.
#' @export
update_required_packages <- function() {

  current <- c("dscore" = "1.9.8",
               "dmetric" = "0.68.1",
               "gseddata" = "1.9.0",
               "gsedread" = "0.8.0",
               "gsedscripts" = "0.14.0")

  if (!requireNamespace("dscore", quietly = TRUE) &&
      packageVersion("dscore") < current[["dscore"]] &&
      interactive()) {
    answer <- askYesNo(paste("Package dscore needed. Install from GitHub?"))
    if (answer) remotes::install_github("d-score/dscore")
  }

  if (!requireNamespace("dmetric", quietly = TRUE) &&
      packageVersion("dmetric") < current[["dmetric"]] &&
      interactive()) {
    answer <- askYesNo(paste("Package dmetric needed. Install from GitHub?"))
    if (answer) remotes::install_github("d-score/dmetric")
  }

  if (!requireNamespace("gseddata", quietly = TRUE) &&
      packageVersion("gseddata") < current[["gseddata"]] &&
      interactive()) {
    answer <- askYesNo(paste("Package gseddata needed. Install from GitHub?"))
    if (answer) remotes::install_github("d-score/gseddata")
  }

  if (!requireNamespace("gsedread", quietly = TRUE) &&
      packageVersion("gsedread") < current[["gsedread"]] &&
      interactive()) {
    answer <- askYesNo(paste("Package gsedread needed. Install from GitHub?"))
    if (answer) remotes::install_github("d-score/gsedread")
  }

  if (!requireNamespace("gsedscripts", quietly = TRUE) &&
      packageVersion("gsedscripts") < current[["gsedscripts"]] &&
      interactive()) {
    answer <- askYesNo(paste("Package gsedscripts needed. Install from GitHub?"))
    if (answer) remotes::install_github("d-score/gsedscripts")
  }
}
