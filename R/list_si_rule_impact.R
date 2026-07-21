#' List what drives simplified-issue declines, by question and reason
#'
#' The simplified-issue counterpart of [list_rule_impact()]. The standard path
#' measures a rule's marginal lift by relaxing it and re-deciding; SI needs no
#' counterfactual, because each decline already carries the question and condition
#' that caused it, so the impact is a direct attribution rather than an estimate.
#'
#' Counted on the combined table, not on every answer: one insured x coverage is
#' one decision, and the reason reported is the one that actually decided it.
#' Counting raw answers would let a cell declined by two questions count twice.
#' The engine speaks reason keys; the wording (`reason_ko`) is joined from the
#' workbook, so the package source stays ASCII and a rewording is a cell edit.
#'
#' @param combined The per-`(id, coverage)` table from [combine_si_decision()].
#' @param rulebook A rulebook from [load_si_rulebook()].
#' @param coverage Optional coverage(s) to restrict to; `NULL` for all.
#' @return A `data.table` of `question`, `reason`, `reason_ko`, `n`, `share`
#'   (percent of declines), ordered by descending count.
#' @seealso [list_rule_impact()], [list_si_decline_disease()].
#' @export
list_si_rule_impact <- function(combined, rulebook, coverage = NULL) {
  reason <- question <- n <- reason_ko <- share <- NULL  # NSE
  dt <- as.data.table(combined)
  if (!is.null(coverage)) dt <- dt[dt$coverage %chin% coverage]
  declined <- dt[!is.na(reason)]
  if (!nrow(declined))
    return(data.table(question = character(), reason = character(),
                      reason_ko = character(), n = integer(), share = numeric()))

  out <- declined[, .(n = .N), by = .(question, reason)]
  out[, reason_ko := setNames(rulebook$reason$reason_ko, rulebook$reason$reason)[reason]]
  out[, share := round(100 * n / sum(n), 1)]
  setcolorder(out, c("question", "reason", "reason_ko", "n", "share"))
  setorder(out, -n)
  out[]
}
