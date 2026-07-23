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
#' @param combined The per-`(id, coverage)` table from [combine_si_decision()],
#'   which carries the `decision` sheet as its `decision_table` attribute -- so the
#'   decision names and `auto` flag come from the workbook, not a literal here, and
#'   no rulebook is passed.
#' @return A `data.table` with `coverage`, `decision`, `name`, `auto`, `n`,
#'   `prop`, ordered by coverage and descending count.
#' @seealso [tabulate_decision()], [auto_rate()], [combine_si_decision()].
#' @export
tabulate_si_decision <- function(combined) {
  coverage <- dec <- decision <- n <- auto <- name <- prop <- NULL  # NSE
  dt <- as.data.table(combined)
  if (!nrow(dt)) stop("`combined` has no rows to tabulate.")
  decision_table <- attr(combined, "decision_table")
  if (is.null(decision_table))
    stop("`combined` has no `decision_table` attribute; produce it with combine_si_decision().")
  name_of <- setNames(decision_table$name, decision_table$code)
  auto_of <- setNames(as.integer(decision_table$auto), decision_table$code)

  out <- dt[, .(n = .N), by = .(coverage, decision = dec)]
  out[, name := name_of[decision]]
  out[, auto := factor(auto_of[decision], levels = c(0L, 1L))]
  out[, prop := n / sum(n), by = coverage]
  setcolorder(out, c("coverage", "decision", "name", "auto", "n", "prop"))
  setorder(out, coverage, -n)
  out[]
}
