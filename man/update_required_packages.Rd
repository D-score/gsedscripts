% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/update_required_packages.R
\name{update_required_packages}
\alias{update_required_packages}
\title{Update the required packages to their development versions}
\usage{
update_required_packages(allow_update = FALSE, include_gseddata = FALSE)
}
\arguments{
\item{allow_update}{Logical. If `TRUE`, the function will install the
development versions from GitHub. The default in `FALSE`}

\item{include_gseddata}{Logical. If `TRUE`, the function will also check for
the `gseddata` package. The default is `FALSE`.}
}
\value{
Logical. `TRUE` if all required packages are installed and up-to-date.
}
\description{
This function checks if the required packages are installed and up-to-date.
If not, it asks the user if they want to install the development versions
from GitHub.
}
