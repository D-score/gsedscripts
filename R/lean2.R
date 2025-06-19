#' Convert a data frame with one row per visit into a long list
#'
#' The `as.lean2()` function converts a data set with one row
#' per visit into a list with the following elements:
#'
#' @details
#' 1. `visits`: a data frame with administrative and covariate variables. The
#' minimum columns are specified by `visit_id` (e.g., `"subjid"`, `"agedays"`).
#' Each visit is defined by a unique combination of these columns.
#' 2. `responses`: a data frame with items in long format. The minimal
#' columns are specified by `visit_id` plus columns named `"item"` and
#' `"response"` that stores each item-response score.
#' 3. `auxiliary`: a data frame with auxiliary variables in long format,
#' 4. `visit_id`: a character vector with the names of the grouping variable(s)
#' whose unique combination define a visit.
#'
#' The object has class `lean2`. This function is a modern version of the
#' `gseddata::as.lean()` function.
#'
#' @param x A `data.frame` with one visit per row with
#' administrative, covariates, items and auxiliary variables.
#' @param remove_duplicates A logical indicating whether the function
#' may proceed silently by removing rows with duplicate keys.
#' @param strict A logical specifying whether the resulting item
#' names must conform to one of the built-in names in the `dscore`
#' package. The default is \code{strict = TRUE}.
#' @param visit_id A character vector with the names of the grouping variable(s)
#' that define a visit. The default is \code{c("subjid", "agedays")}.
#'
#' @author Stef van Buuren
#'
#' @note
#' Function \code{as.lean()} and \code{as.data.frame.lean()} perform
#' inverse operations. However, the results will only be identical
#' if there are no duplicate visits, if all responses are integer, if
#' all auxiliary variables are numeric, and if no item in \code{x} has
#' entirely missing data.
#'
#' @seealso \code{\link{as.data.frame.lean2}}
#' @examples
#' data <- data.frame(subjid = c(1L, 1L, 2L, 3L, 3L),
#'                    agedays = c(100L, 200L, 149L, 120L, 200L),
#'                    sexn = c(2L, 2L, 1L, 1L, 1L),
#'                    by3cgd036 = c(0L, 1L, NA, NA, 0L),
#'                    by3cgd037 = c(1L, NA, NA, NA, 1L),
#'                    by3cgd038 = c(NA, 1L, 0L, NA, 0L),
#'                    outcome1 = c(NA, NA, 121, 113, 113),
#'                    outcome2 = c(2, 3, NA, 8, 6))
#' lean <- as.lean2(data)
#' lean
#' unlean <- as.data.frame(lean)
#' unlean
#' identical(data, unlean)
#' @export
as.lean2 <- function(x, visit_id = c("subjid", "agedays"),
                     remove_duplicates = FALSE, strict = TRUE) {
  if (!is.data.frame(x)) stop("`x` must be a data.frame")
  if (!all(visit_id %in% names(x))) stop("Some `visit_id` variable not in data")

  # Remove duplicates if requested
  n_orig <- nrow(x)
  x <- dplyr::distinct(x, dplyr::across(all_of(visit_id)), .keep_all = TRUE)
  if (!remove_duplicates && nrow(x) != n_orig) {
    stop(n_orig - nrow(x), " duplicate ", paste(visit_id, collapse = "/"),
         " combinations detected")
  }

  # Identify variable types
  namelist <- split_names(colnames(x), strict = strict)

  # Items to long format
  items <- x |>
    dplyr::select(all_of(c(visit_id, namelist$itm))) |>
    tidyr::pivot_longer(
      cols = all_of(namelist$itm),
      names_to = "item",
      values_to = "response",
      values_drop_na = TRUE) |>
    dplyr::mutate(
      response = as.integer(.data$response),
      item = as.character(.data$item))

  # Auxiliary variables to long format
  auxiliary <- x |>
    dplyr::select(all_of(c(visit_id, namelist$itm))) |>
    tidyr::pivot_longer(
      cols = all_of(namelist$itm),
      names_to = "variable",
      values_to = "value",
      values_drop_na = TRUE) |>
    dplyr::mutate(
      value = as.integer(.data$value),
      variable = as.character(.data$variable))

  visits <- x |>
    dplyr::select(all_of(c(visit_id, namelist$adm, namelist$cov)))

  rownames(visits) <- NULL
  rownames(items) <- NULL
  rownames(auxiliary) <- NULL

  lean2 <- list(
    visits = visits,
    items = items,
    auxiliary = auxiliary,
    visit_id = visit_id
    )
  class(lean2) <- c("lean2", "list")
  return(lean2)
}

#' Read lean2 object from a database
#'
#' The `read_lean2()` function construct an object of class `lean2` from
#' a database.
#'
#' @param dbfile File name of the database
#' @rdname as.lean2
#' @export
read_lean2 <- function(dbfile) {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = dbfile)

  safe_read <- function(con, table_name) {
    if (table_name %in% DBI::dbListTables(con)) {
      tbl <- DBI::dbReadTable(con, table_name)
      if (!nrow(tbl)) data.frame() else tbl
    } else {
      data.frame()
    }
  }

  responses <- safe_read(con, "responses")
  visits <- safe_read(con, "visits")
  auxiliary <- safe_read(con, "auxiliary")

  DBI::dbDisconnect(con, shutdown = TRUE)

  list(responses = responses, visits = visits, auxiliary = auxiliary)
}

split_names <- function(x, strict = TRUE) {
  # standard variable names
  adm <- c("studyid", "ctrycd", "cohort", "cohortn",
           "subjid", "subjido", "agedays")
  cov <- c("sex", "sexn", "gagebrth", "mage",
           "birthwt", "birthlen", "meducyrs", "edumocat",
           "residence", "htcm", "wtkg", "haz", "waz",
           "whz", "baz", "arm")

  itm <- dscore::get_itemnames(x, strict = strict)
  aux <- setdiff(x, c(adm, cov, itm))

  # remove any variables unknown in x
  adm <- x[x %in% adm]
  cov <- x[x %in% cov]
  itm <- x[x %in% itm]
  aux <- x[x %in% aux]

  return(list(adm = adm, cov = cov, itm = itm, aux = aux))
}

#' Check for class `lean2`
#'
#' @param x Any R object
#' @seealso \code{\link{as.lean2}}
#' @export
is.lean2 <- function(x) inherits(x, "lean2")

#' @export
print.lean2 <- function(x, ...) {
  if (!is.lean2(x)) stop("`x` not of class `lean2`")

  items <- unique(x[["items"]]$item)
  item_info <- dscore::decompose_itemnames(items)
  visits <- nrow(x[["visits"]])

  cat("lean2 data\n")
  cat("visits:", visits,
      "\n")
  cat("scores:", nrow(x[["items"]]),
      "  items:", length(items),
      "  instruments:", length(unique(item_info$instrument)),
      "  domains:", length(unique(item_info$domain)),
      "\n")
}

#' Convert lean2 object into a data.frame
#'
#' @param x An object of class \code{lean2}
#' @param \dots Passed down to \code{as.data.frame()}
#' @details
#' Function \code{as.lean2()} and \code{as.data.frame.lean2()} perform
#' inverse operations. However, the results will only be identical
#' if there are no duplicates, if all items are integer, and if
#' all auxiliary variables are numeric.
#' @seealso \code{\link{as.lean2}}, \code{\link{as.data.frame}}
#' @export
as.data.frame.lean2 <- function(x, ...) {
  if (!is.lean2(x)) stop("`x` not of class `lean2`")

  add <- x[["items"]] |>
    tidyr::pivot_wider(
      names_from = "item",
      values_from = "response"
    )

  df <- dplyr::left_join(
    x = x[["visits"]],
    y = add,
    by = x[["visit_id"]]
  )

  add <- x[["auxiliary"]] |>
    tidyr::pivot_wider(
      names_from = "variable",
      values_from = "value"
    )

  df <- dplyr::left_join(
    x = x[["visits"]],
    y = add,
    by = x[["visit_id"]]
  )

  return(as.data.frame(df, ...))
}

