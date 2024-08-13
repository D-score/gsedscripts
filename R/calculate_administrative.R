#' Calculate administrative variables
#'
#' @param data Long form data, typically created by scripts/edit_data.R
#' @return Five administrtative variables
#' @export
calculate_administrative <- function(data) {
  required <- c("gsed_id", "age")
  if (any(!required %in% colnames(data))) {
    stop("Expected gsed_id and age not found.")
  }

  adm <- c("cohort", "cohortn", "subjid", "subjido", "agedays")
  data <- data |>
    select("gsed_id", "age") |>
    mutate(
      subjido = .data$gsed_id,
      agedays = .data$age,
      cohort = strtrim(.data$subjido, 7L),
      cohort = case_match(
        .data$cohort,
        "11-GSED" ~ "GSED-BGD",
        "17-GSED" ~ "GSED-PAK",
        "20-GSED" ~ "GSED-TZA",
        .default = NA_character_),
      cohortn = as.integer(strtrim(.data$subjido, 2L)) + 100L,
      subjid = .data$cohortn * 100000L +
        as.integer(substr(.data$subjido, 9L, 12L))) |>
    select(all_of(adm))

  return(data)
}
