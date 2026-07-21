# The simplified-issue counterpart of tabulate_decision() (auto_rate() lives in
# its own file, as auto_rate.R).
# The standard combined table is wide (one column per coverage) and its codes
# compose, so tabulate_decision() splits composite codes before counting. The SI
# table is already long -- one row per (id, coverage) -- and its three codes never
# compose, so this is a group-by. The output shape is kept parallel anyway
# (coverage x decision x n x prop x auto) so a reader who knows one can read both.

#' Tabulate the simplified-issue decision distribution per coverage
#'
#' The simplified-issue counterpart of [tabulate_decision()].
#'
#' @param combined The per-`(id, coverage)` table from [combine_si_decision()].
#' @param rulebook A rulebook from [load_si_rulebook()]; supplies the `decision`
#'   sheet's names and `auto` flag, so what counts as automated is a workbook cell
#'   rather than a literal here.
#' @return A `data.table` with `coverage`, `decision`, `name`, `auto`, `n`,
#'   `prop`, ordered by coverage and descending count.
#' @seealso [tabulate_decision()], [auto_rate()], [combine_si_decision()].
#' @export
tabulate_si_decision <- function(combined, rulebook) {
  coverage <- dec <- decision <- n <- auto <- name <- prop <- NULL  # NSE
  dt <- as.data.table(combined)
  if (!nrow(dt)) stop("`combined` has no rows to tabulate.")

  out <- dt[, .(n = .N), by = .(coverage, decision = dec)]
  out[, name := setNames(rulebook$decision$name, rulebook$decision$code)[decision]]
  out[, auto := factor(rulebook$auto[decision], levels = c(0L, 1L))]
  out[, prop := n / sum(n), by = coverage]
  setcolorder(out, c("coverage", "decision", "name", "auto", "n", "prop"))
  setorder(out, coverage, -n)
  out[]
}
