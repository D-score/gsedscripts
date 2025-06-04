#' Calculate DIF statistic per item for multiple groups
#'
#' This function calculates DIF statistics for multiple groups using the
#' `difR` package. The function uses the `genDichoDif()` function for
#' generalized logistic regression and the generalized Mantel-Haenszel test.
#' The function returns a table with DIF statistics per item.
#'
#' @param data Data frame with dichotomous item responses and age
#' @param items Character vector with item names
#' @param group Factor or character vector with group names. Must have same
#' length as the number of rows in the data frame.
#' @param score Data frame with item parameters. Optional.
#' @param focal.names Optional. Character vector with group names.
#' If omitted, the function will use the unique values of the group variable.
#' @param thr Optional. Threshold for DIF classification using the MH-statistics.
#' If omitted, the function will use the threshold from the `difGMH()` function.
#' @param p.adjust.method The adjustment method. Defaults to `"holm"`
#' @param method Character vector with methods to use for DIF analysis. Either
#' `"GMH"` or `"genLogistic"`.
#' @details
#' The function uses the `"holm"` method for p-value adjustment.
#'
#' @return data frame with DIF statistics per item
#'
#' @examples
#' library(dscore)
#' library(childdevdata)
#' data <- childdevdata::gcdg_nld_smocc
#' items <- intersect(dscore::get_itemnames(ins = "ddi", order = "indm"), colnames(data))
#'
#' # Method 1: Use a numeric group variable
#' group <- ifelse(data$sex == "Male", 1, 0)
#' focal.names <- 1
#'
#' # Method 2: Use a character group variable
#' group <- data$sex
#' focal.names <- "Male"
#'
#' # dt <- calculate_DIF_table(data = data, items = items, group = group,
#' #                          focal.names = focal.names)
#'
#' @export
calculate_DIF_table <- function(data, items, group, score = "score",
                                focal.names = NULL, thr = NULL,
                                p.adjust.method = "holm",
                                method = c("GMH", "genLogistic")) {

  stopifnot(is.data.frame(data))
  stopifnot(length(items) > 0L)
  stopifnot(length(score) == 1L || length(score) == nrow(data))
  stopifnot(length(group) == nrow(data))

  # remove missing group values
  idx <- !is.na(group)
  data <- data[idx, ]
  group <- as.character(group[idx])

  if (is.null(focal.names)) {
    focal.names <- unique(group)
  }

  # check length of focal.names
  stopifnot(length(focal.names) == 1L ||
              length(focal.names) == length(unique(group)))

  # MH-test for uniform DIF
  if ("GMH" %in% method) {
    MH <- genDichoDif(Data = as.matrix(data[, items]),
                      group = group,
                      focal.names = focal.names,
                      p.adjust.method = p.adjust.method,
                      method = c("GMH"))
  } else {
    MH <- NULL
  }

  if ("genLogistic" %in% method) {
    # LR-test: unif + nunif
    LR <- genDichoDif(Data = as.matrix(data[, items]),
                      group = group,
                      focal.names = focal.names,
                      p.adjust.method = p.adjust.method,
                      method = c("genLogistic"))
  } else {
    LR <- NULL
  }

  # Derive DIF classification from MH using threshold thr
  if (is.null(thr)) {
    thr <- MH$thr
  }
  MH_DIF <- cut(MH$GMH, breaks = c(-Inf, thr, Inf), labels = c("", "DIF"))
  tmp <- data.frame(item = items,
                    MH_stat = MH$GMH,
                    MH_DIF = MH_DIF)

  # Derive Zumbo and Jodoin classifications from LR
  LR_zumbo <- cut(LR$deltaR2,
                  breaks = c(-Inf, 0.13, 0.26, Inf),
                  labels = c(LETTERS[1:3]))
  LR_jodoin <- cut(LR$deltaR2,
                   breaks = c(-Inf, 0.035, 0.07, Inf),
                   labels = c(LETTERS[1:3]))

  # create item table sorted by DIF
  dif_table <- data.frame(num = 1:length(items),
                          item = items,
                          LR_stat = LR$genLogistik,
                          LR_deltaR2 = LR$deltaR2,
                          LR_zumbo = LR_zumbo,
                          LR_jodoin = LR_jodoin)
  dif_table <- left_join(dif_table, tmp, by = c("item" = "item"))
  dif_table <- dif_table[, c("num", "item",
                             "MH_stat", "MH_DIF",
                             "LR_stat", "LR_deltaR2", "LR_zumbo", "LR_jodoin")]

  return(dif_table)

}
