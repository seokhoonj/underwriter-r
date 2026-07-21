#' Auto-decision rate implied by a simplified-issue tabulation
#'
#' The share of cells the engine settled without an underwriter, read off the
#' `auto` flag rather than by naming a code -- rename the referral code in the
#' workbook and this still reports the same thing.
#'
#' @param tab A tabulation from [tabulate_si_decision()].
#' @param by Optional grouping columns (e.g. `"coverage"`); `NULL` for the total.
#' @return A `data.table` of `auto_rate` (percent), grouped as asked.
#' @seealso [tabulate_si_decision()].
#' @export
auto_rate <- function(tab, by = NULL) {
  auto <- n <- NULL  # NSE
  dt <- as.data.table(tab)
  if (is.null(by)) return(dt[, .(auto_rate = 100 * sum(n[auto == "1"]) / sum(n))])
  dt[, .(auto_rate = 100 * sum(n[auto == "1"]) / sum(n)), by = by]
}
