#' Merge measurement made on different days to one child level record
#'
#' For a given child, this script joins measurements made within a
#' specified window.
#' @param long        Long form data, typically created by scripts/edit_data.R
#' @param instruments Character vector of instruments names. Currently supported
#'                    instruments are `"gpa"`, `"gto"` and `"by3"`.
#' @param days        Non-negative integer, window width in number of days
#' @param drop_na_age Logical. Should measurements with missing ages be dropped?
#' @param include_return  Logical. Should the return visits be included in the join?
#' @param quiet       Logical. Print messages to terminal?
#' @return A data frame with one record per child. The first five columns
#' are administrative variables, followed by the intervals in days per
#' instrument. The remaining columns are items from the instruments.
#' @examples
#' \dontrun{
#' joined <- make_wide(long, instruments = c("gpa", "gto"))
#' }
#' @export
make_wide <- function(long,
                      instruments = c("gpa", "gto", "by3"),
                      days = 10L,
                      drop_na_age = TRUE,
                      include_return = FALSE,
                      quiet = FALSE) {
  if (!requireNamespace("fuzzyjoin", quietly = TRUE)) {
    stop("fuzzyjoin is NOT installed.")
  }

  stopifnot(days >= 0L)
  stopifnot(length(instruments) >= 2L)
  if (any(!instruments %in% c("gpa", "gto", "by3"))) {
    stop("Sorry, make_wide() currently only supports 'gpa', 'gto' and 'by3'")
  }

  # variable names
  adm <- c("cohort", "cohortn", "subjid", "subjido", "agedays")
  items_sf <- c(get_itemnames(instrument = "gpa", order = "indm"))
  items_lf <- c(get_itemnames(instrument = "gto"))
  items_bsid <- c(get_itemnames(instrument = "by3"))
  items <- c(items_sf, items_lf, items_bsid)

  # create joinid, recode items,
  long <- long |>
    mutate(
      joinid = .data$subjid * 100,
      across(any_of(items), ~ case_when(
        .x == "1" ~ 1L,
        .x == "0" ~ 0L))) |>
    select(all_of(adm), any_of(items), "ins", "joinid")

  # remove rows with missing agedays
  if (any(is.na(long$agedays)) && drop_na_age) {
    n <- nrow(long)
    long <- long |>
      tidyr::drop_na("agedays")
    if (!quiet) {
      cat("Measurements removed with missing ages: ", n - nrow(long), "\n")
    }
  }

  # split file into three parts: LF, SF and BSID
  sf <- long |>
    filter(.data$ins == "sf") |>
    group_by(.data$subjid) |>
    mutate(row_number = row_number()) |>
    select(all_of(adm), any_of(items_sf), "joinid", "row_number")
  lf <- long |>
    filter(.data$ins == "lf") |>
    group_by(.data$subjid) |>
    mutate(row_number = row_number()) |>
    select(all_of(adm), any_of(items_lf), "joinid", "row_number")
  bsid <- long |>
    filter(.data$ins == "bsid") |>
    group_by(.data$subjid) |>
    mutate(row_number = row_number()) |>
    select(all_of(adm), any_of(items_bsid), "joinid", "row_number")

  sf_first <- sf |>
    filter(row_number == 1L)
  lf_first <- lf |>
    filter(row_number == 1L)
  bsid_first <- bsid |>
    filter(row_number == 1L)
  sf_second <- sf |>
    filter(row_number >= 2L)
  lf_second <- lf |>
    filter(row_number >= 2L)
  bsid_second <- bsid |>
    filter(row_number >= 2L)

  #  fuzzy join within days - FIRST
  phase1 <- sf_first |>
    fuzzyjoin::difference_left_join(
      lf_first,
      by = c("joinid", "agedays"),
      max_dist = days,
      distance_col = "dist") |>
    ungroup() |>
    rename(cohort = "cohort.x", cohortn = "cohortn.x",
           subjid = "subjid.x", subjido = "subjido.x",
           agedays = "agedays.x", joinid = "joinid.x",
           day_gto = "agedays.dist") |>
    mutate(day_gpa = 0) |>
    select(all_of(adm), any_of(items), "joinid",
           "day_gpa", "day_gto")

  #  fuzzy join the LF/SF and BSID
  if ("by3" %in% instruments) {
    phase1 <- phase1 |>
      fuzzyjoin::difference_left_join(
        bsid_first,
        by = c("joinid", "agedays"),
        max_dist = days, distance_col = "dist") |>
      ungroup() |>
      rename(cohort = "cohort.x", cohortn = "cohortn.x",
             subjid = "subjid.x", subjido = "subjido.x",
             agedays = "agedays.x", joinid = "joinid.x",
             day_by3 = "agedays.dist") |>
      select(all_of(adm), any_of(items), "joinid",
             "day_gpa", "day_gto", "day_by3")
  }

  #  fuzzy join within days - SECOND
  if (include_return) {
    #  fuzzy join within days - FIRST
    sec <- sf_second |>
      fuzzyjoin::difference_left_join(
        lf_second,
        by = c("joinid", "agedays"),
        max_dist = days,
        distance_col = "dist") |>
      ungroup() |>
      rename(cohort = "cohort.x", cohortn = "cohortn.x",
             subjid = "subjid.x", subjido = "subjido.x",
             agedays = "agedays.x", joinid = "joinid.x",
             day_gto = "agedays.dist") |>
      mutate(day_gpa = 0) |>
      select(all_of(adm), any_of(items), "joinid",
             "day_gpa", "day_gto")

    #  fuzzy join the LF/SF and BSID
    if ("by3" %in% instruments) {
      sec <- sec |>
        fuzzyjoin::difference_left_join(
          bsid_second,
          by = c("joinid", "agedays"),
          max_dist = days, distance_col = "dist") |>
        ungroup() |>
        rename(cohort = "cohort.x", cohortn = "cohortn.x",
               subjid = "subjid.x", subjido = "subjido.x",
               agedays = "agedays.x", joinid = "joinid.x",
               day_by3 = "agedays.dist") |>
        select(all_of(adm), any_of(items), "joinid",
               "day_gpa", "day_gto", "day_by3")
    }
  } else {
    sec <- NULL
  }

  phase1 <- bind_rows(phase1, sec) |>
    arrange(.data$subjid, .data$agedays)

  phase1 <- phase1 |>
    select(all_of(adm), any_of(c("day_gpa", "day_gto", "day_by3")), any_of(items))

  return(phase1)
}

