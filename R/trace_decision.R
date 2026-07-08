#' Trace one insured's final decision back to its per-disease inputs
#'
#' For a single `id`, shows how each coverage's final decision was built from the
#' per-disease rule decisions in `applied`, and verifies it: the decision is
#' recomputed for that id with [combine_decision()] and checked against the
#' stored `final`. Use it to audit a single case -- what fed in, how it merged,
#' and whether the stored result is reproducible.
#'
#' @param applied Per-disease decisions from [match_rule()] (`$applied`).
#' @param final A wide final-decision table from [combine_decision()], carrying
#'   its config-table attributes.
#' @param id The single insured id to trace.
#' @return A `data.table`, one row per coverage that received any per-disease
#'   decision, with columns `coverage`, `diseases` (the contributing
#'   `kcd_main:code` inputs, `" | "`-separated), `computed` (the decision
#'   recomputed for this id), `stored` (the value in `final`), and `ok`
#'   (`computed == stored`).
#' @seealso [combine_decision()], [tabulate_decision()].
#' @export
trace_decision <- function(applied, final, id) {
  key             <- id
  decision_cols   <- attr(applied, "decision_cols")
  decision_table  <- attr(final, "decision_table")
  exclusion_table <- attr(final, "exclusion_table")
  reduction_table <- attr(final, "reduction_table")
  loading_table   <- attr(final, "loading_table")
  if (is.null(decision_table))
    stop("`final` has no config attributes; produce `final` with combine_decision().")

  one <- as.data.table(applied)[id == key]
  if (!nrow(one)) stop(sprintf("id %s not found in `applied`.", format(key)))

  # per-disease decision tokens that fed each coverage
  inputs <- melt(one, id.vars = "kcd_main", measure.vars = decision_cols,
                 variable.name = "coverage", value.name = "code", variable.factor = FALSE)
  inputs <- inputs[!is.na(code) & nzchar(code)]
  per_cov <- inputs[, .(diseases = paste(sprintf("%s:%s", kcd_main, code), collapse = " | ")),
                    by = coverage]

  # recompute this id's final and compare to the stored one
  re <- combine_decision(one, decision_table, exclusion_table, reduction_table, loading_table,
                         decision_cols = decision_cols)
  computed <- melt(re, id.vars = "id", variable.name = "coverage", value.name = "computed",
                   variable.factor = FALSE)[, .(coverage, computed)]
  stored <- melt(as.data.table(final)[id == key], id.vars = "id", variable.name = "coverage",
                 value.name = "stored", variable.factor = FALSE)[, .(coverage, stored)]

  out <- merge(per_cov, computed, by = "coverage", all.x = TRUE)
  out <- merge(out, stored, by = "coverage", all.x = TRUE)
  out[, ok := computed == stored]
  setcolorder(out, c("coverage", "diseases", "computed", "stored", "ok"))
  setorder(out, coverage)
  out[]
}
