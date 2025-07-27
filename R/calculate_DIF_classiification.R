#' Calculate Differential Item Functioning (DIF) classification table
#'
#' Classifies each items according to the Jodoin/Gierl criteria for DIF.
#' Currently implemented:
#'
#'   - phase (1 vs 2)
#'   - each country vs others
#'
#' @param responses A data frame containing the responses to the items.
#' @param model Optional. A `dmodel` object used to calculate the `dscore`.
#' If not specified, the function will calculate the D-score using the
#' default key.
#' @return A data frame with items and their classification for DIF.
#' @export
calculate_DIF_classification <- function(responses, model = NULL) {


  id_cols <- c("cohort", "country", "subjid", "pair")
  item_var <- "item"
  response_var <- "response"

  items_sf <- get_itemnames(instrument = "sf_", order = "imnd")
  items_lf <- get_itemnames(instrument = c("lfa", "lfb", "lfc"))
  items <- c(items_sf, items_lf)
  items <- intersect(items, unique(responses[[item_var]]))

  wide <- responses |>
    select(all_of(c(id_cols, "agedays", "ins", item_var, response_var))) |>
    pivot_wider(
      names_from = !!item_var,
      values_from = !!response_var,
      id_cols = all_of(id_cols)
    ) |>
    arrange(!!!syms(id_cols))
  agedays_info <- responses |>
    distinct(!!!syms(c("subjid", "pair", "ins", "agedays"))) |>
    pivot_wider(names_from = "ins",
                values_from = "agedays",
                names_prefix = "agedays_") |>
    mutate(agedays = .data$agedays_sf)
  wide <- wide |>
    left_join(agedays_info, by = c("subjid", "pair")) |>
    mutate(phase = if_else(.data$country %in% c("BGD", "PAK", "TZA"), 1L, 2L)) |>
    select(all_of(id_cols), starts_with("agedays"),
           "phase", any_of(items))

  # DIF analysis: phase (1 vs 2)
  itemdata <- wide |>
    select(any_of(items)) |>
    as.matrix()
  phase <- wide$phase

  # Calculate D-score, using model itembank when provided
  itembank <- NULL
  key <- NULL
  if (!is.null(model)) {
    itembank <- model$itembank
    key <- model$name
  }
  d <- dscore(wide, xname = "agedays", xunit = "days",
              itembank = itembank, key = key)$d

  # remove mode s SF items from itemdata, since these are only measured in one cohort
  # items_to_remove <- get_itemnames(ins = "sf_", mode = "s")
  # itemdata <- itemdata[, !colnames(itemdata) %in% items_to_remove]

  # DIF analysis: phase
  LR <- genDichoDif(Data = itemdata,
                    group = phase,
                    focal.names = 1,
                    match = d,
                    method = c("genLogistic"))

  # DIF analysis: each country vs others
  country <- wide$country
  # countries <- setdiff(unique(country), "NLD")
  countries <- unique(country)
  LR_list <- vector("list", length(countries))
  names(LR_list) <- countries
  for (ct in countries) {
    # cat("ct: ", ct, "\n")
    LR_list[[ct]] <- genDichoDif(Data = itemdata,
                                 group = country,
                                 focal.names = ct,
                                 match = d,
                                 method = c("genLogistic"))
  }

  # Classify according to Jodoin/Gierl criteria
  DIF_table <- data.frame(
    item = colnames(itemdata),
    phase = cut(LR$deltaR2, breaks = c(-Inf, 0.035, 0.07, Inf),
                labels = c("no", "moderate", "large"), right = FALSE),
    BGD  = cut(LR_list[["BGD"]]$deltaR2, breaks = c(-Inf, 0.035, 0.07, Inf),
               labels = c("no", "moderate", "large"), right = FALSE),
    BRA  = cut(LR_list[["BRA"]]$deltaR2, breaks = c(-Inf, 0.035, 0.07, Inf),
               labels = c("no", "moderate", "large"), right = FALSE),
    CHN  = cut(LR_list[["CHN"]]$deltaR2, breaks = c(-Inf, 0.035, 0.07, Inf),
               labels = c("no", "moderate", "large"), right = FALSE),
    CIV  = cut(LR_list[["CIV"]]$deltaR2, breaks = c(-Inf, 0.035, 0.07, Inf),
               labels = c("no", "moderate", "large"), right = FALSE),
    NLD  = cut(LR_list[["NLD"]]$deltaR2, breaks = c(-Inf, 0.035, 0.07, Inf),
               labels = c("no", "moderate", "large"), right = FALSE),
    PAK  = cut(LR_list[["PAK"]]$deltaR2, breaks = c(-Inf, 0.035, 0.07, Inf),
               labels = c("no", "moderate", "large"), right = FALSE),
    TZA  = cut(LR_list[["TZA"]]$deltaR2, breaks = c(-Inf, 0.035, 0.07, Inf),
               labels = c("no", "moderate", "large"), right = FALSE)
  )

  return(DIF_table)
}
