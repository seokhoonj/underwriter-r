#' Rank the diseases behind simplified-issue declines
#'
#' Which representative disease decided the decline, from the `kcd_main` carried
#' through from the answer that won the fold. One decline is one disease here --
#' unlike the standard [list_decline_disease()], where several diseases can drive
#' the same declined cell and the counts deliberately overlap.
#'
#' @param combined The per-`(id, coverage)` table from [combine_si_decision()].
#' @param coverage Optional coverage(s) to restrict to; `NULL` for all.
#' @param n_top How many diseases to return.
#' @return A `data.table` of `kcd_main`, `n`, `share`, ordered by descending count.
#' @seealso [list_decline_disease()], [list_si_rule_impact()].
#' @export
list_si_decline_disease <- function(combined, coverage = NULL, n_top = 20L) {
  reason <- kcd_main <- n <- share <- NULL  # NSE
  dt <- as.data.table(combined)
  if (!is.null(coverage)) dt <- dt[dt$coverage %chin% coverage]
  declined <- dt[!is.na(reason) & !is.na(kcd_main)]
  if (!nrow(declined))
    return(data.table(kcd_main = character(), n = integer(), share = numeric()))

  out <- declined[, .(n = .N), by = kcd_main]
  out[, share := round(100 * n / sum(n), 1)]
  setorder(out, -n)
  head(out, n_top)[]
}
