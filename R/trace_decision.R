#' Trace one insured's final decision back to its per-disease inputs
#'
#' For a single `id`, shows how each coverage's final decision was built from the
#' per-disease rule decisions in `applied`, and verifies it: the decision is
#' recomputed for that id with [combine_decision()] and checked against the
#' stored `combined`. Use it to audit a single case -- what fed in, how it merged,
#' and whether the stored result is reproducible.
#'
#' @param applied Per-disease decisions from [match_rule()] (`$applied`).
#' @param combined A wide combined-decision table from [combine_decision()],
#'   carrying its config-table attributes.
#' @param id The single insured id to trace (must be present in `combined`). An id
#'   with no diagnosis in `applied` -- an automatic pass recorded by
#'   [combine_decision()]'s `pass_ids` -- is traced as a pass on every coverage.
#' @return A `data.table`, one row per coverage, with columns `coverage`,
#'   `diseases` (the contributing `kcd_main:code` inputs, `" | "`-separated),
#'   `computed` (the decision recomputed for this id), `stored` (the value in
#'   `combined`), and `ok` (`computed == stored`). A coverage present on only one
#'   side surfaces as a row with `ok = FALSE`.
#' @seealso [combine_decision()], [tabulate_decision()].
#' @export
trace_decision <- function(applied, combined, id) {
  key             <- id
  decision_cols   <- attr(applied, "decision_cols")
  decision_table  <- attr(combined, "decision_table")
  exclusion_table <- attr(combined, "exclusion_table")
  reduction_table <- attr(combined, "reduction_table")
  loading_table   <- attr(combined, "loading_table")
  if (is.null(decision_table))
    stop("`combined` has no config attributes; produce `combined` with combine_decision().")

  role          <- decision_table$role
  standard      <- decision_table$code[!is.na(role) & role == "standard"][1L]
  manual_review <- decision_table$code[!is.na(role) & role == "manual_review"][1L]

  combined_dt <- as.data.table(combined)[id == key]
  if (!nrow(combined_dt)) stop(sprintf("id %s not found in `combined`.", format(key)))
  stored <- melt(combined_dt, id.vars = "id", variable.name = "coverage",
                 value.name = "stored", variable.factor = FALSE)[, .(coverage, stored)]

  applied_one <- as.data.table(applied)[id == key]
  if (!nrow(applied_one)) {
    # no diagnosis in `applied`: combine_decision(pass_ids=) recorded this id as
    # an automatic pass, so every coverage should read standard.
    out <- stored[, .(coverage, diseases = "(no diagnosis: auto pass)",
                      computed = standard, stored, ok = stored == standard)]
    setcolorder(out, c("coverage", "diseases", "computed", "stored", "ok"))
    return(out[order(coverage)])
  }

  # per-disease tokens that fed each coverage; mirror combine's fill so an
  # unmatched disease shows as manual review rather than a blank cell
  filled <- copy(applied_one)
  filled[matched == 0L, (decision_cols) := manual_review]
  inputs <- melt(filled, id.vars = "kcd_main", measure.vars = decision_cols,
                 variable.name = "coverage", value.name = "code", variable.factor = FALSE)
  inputs <- inputs[!is.na(code) & nzchar(code)]
  per_cov <- inputs[, .(diseases = paste(sprintf("%s:%s", kcd_main, code), collapse = " | ")),
                    by = coverage]

  # recompute this id's decision and compare to the stored one
  recomputed <- combine_decision(applied_one, decision_table, exclusion_table, reduction_table, loading_table,
                                 decision_cols = decision_cols)
  computed <- melt(recomputed, id.vars = "id", variable.name = "coverage", value.name = "computed",
                   variable.factor = FALSE)[, .(coverage, computed)]

  # full join so a coverage present on only one side surfaces as a mismatch
  out <- merge(computed, stored, by = "coverage", all = TRUE)
  out <- merge(out, per_cov, by = "coverage", all.x = TRUE)
  out[, ok := !is.na(computed) & !is.na(stored) & computed == stored]
  setcolorder(out, c("coverage", "diseases", "computed", "stored", "ok"))
  setorder(out, coverage)
  out[]
}
