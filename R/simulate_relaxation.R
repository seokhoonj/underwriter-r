#' Simulate relaxing a disease and measure the per-coverage lift
#'
#' What-if experiment: if the given representative disease(s) (`kcd_main`) were
#' relaxed to standard (auto-approved, every restriction dropped), how much would
#' each coverage's auto-decided share rise? The target disease's per-coverage
#' decisions are set to the standard code, the decisions are re-combined with
#' [combine_decision()], and the auto share is re-tabulated and compared to the
#' baseline.
#'
#' Because a coverage routes to manual review when *any* of an insured's diseases
#' does, the lift is computed by re-running the full combine -- an insured still
#' held for review by another disease does not flip.
#'
#' @param applied Per-disease decisions from [match_rule()] (`$applied`).
#' @param final The baseline wide decision table from [combine_decision()],
#'   carrying its config-table attributes.
#' @param kcd_main One or more patterns matched against `kcd_main`, relaxed
#'   together. A single string is a regex: `"M51"` relaxes M51 and any sub-code
#'   (`M510`, `M512`, ...), and `"^M51$"` relaxes exactly `M51`. A character
#'   vector is OR'd, so the representative codes from [screen_relaxation()] can be
#'   passed straight in -- `c("M543", "M542", "M13")` relaxes all three at once
#'   (same as `"M543|M542|M13"`). Codes carry no regex metacharacters, so a plain
#'   code is a safe substring pattern.
#' @param mode How much to relax a matched target disease: `"review_only"`
#'   (default) turns only its manual-review decisions into standard, keeping any
#'   exclusions, loadings, reductions, and declines; `"full"` turns every decision
#'   it makes into standard. Unmatched targets, which drive manual review
#'   everywhere, relax to standard either way. `"review_only"` can only raise the
#'   auto share and is the realistic lever. `"full"` can *lower* it: waiving a
#'   decline that outranked another disease's manual review surfaces that review,
#'   moving the insured from auto-decline to review (a negative `lift`).
#' @return A `data.table`, one row per coverage sorted by `lift` descending, with
#'   `auto_base` (baseline auto share), `auto_relaxed` (after relaxing), `lift`
#'   (the increase), and `n_flipped` (insured moved from manual review to auto).
#' @note `mode = "full"` can *lower* the auto share on some coverages (negative
#'   `lift`), which is easy to miss. Relaxing a disease that was auto-declining an
#'   insured waives that decline; if another disease held the same coverage for
#'   manual review that the decline had outranked, the insured now routes to
#'   review instead of auto-decline. `mode = "review_only"` keeps declines and so
#'   only ever raises the auto share.
#' @seealso [screen_relaxation()] to rank every disease, [tabulate_decision()],
#'   [combine_decision()].
#' @export
simulate_relaxation <- function(applied, final, kcd_main, mode = c("review_only", "full")) {
  mode <- match.arg(mode)
  if (length(kcd_main) < 1L || anyNA(kcd_main) || any(!nzchar(kcd_main)))
    stop("`kcd_main` must be one or more non-empty patterns.")
  decision_table  <- attr(final, "decision_table")
  exclusion_table <- attr(final, "exclusion_table")
  reduction_table <- attr(final, "reduction_table")
  loading_table   <- attr(final, "loading_table")
  decision_cols   <- attr(applied, "decision_cols")
  if (is.null(decision_table) || is.null(decision_cols))
    stop("`final` must come from combine_decision() and `applied` from match_rule().")

  role          <- decision_table$role
  standard      <- decision_table$code[!is.na(role) & role == "standard"][1L]
  manual_review <- decision_table$code[!is.na(role) & role == "manual_review"][1L]
  target        <- paste(kcd_main, collapse = "|")   # a vector of codes -> OR'd

  base <- tabulate_decision(final)[, .(auto_base = sum(prop[auto == "1"]),
                                       n_total   = sum(n)), by = coverage]

  # relax the target diseases to standard. An unmatched disease drives manual
  # review on every coverage (combine fills it), so relax it everywhere; a matched
  # disease decides only some coverages, so relax just those -- leaving the ones it
  # is silent on untouched, or an insured would be pulled into a new coverage.
  relaxed <- copy(as.data.table(applied))
  tgt <- grepl(target, relaxed$kcd_main)
  relaxed[tgt & matched == 0L, (decision_cols) := standard]
  if (mode == "review_only") {
    for (col in decision_cols)   # only the manual-review cells, keep restrictions
      relaxed[tgt & matched == 1L & get(col) == manual_review, (col) := standard]
  } else {
    for (col in decision_cols)   # every decision the disease makes
      relaxed[tgt & matched == 1L & !is.na(get(col)) & nzchar(get(col)), (col) := standard]
  }
  relaxed[tgt, matched := 1L]

  # ids in the baseline but not in `applied` are no-disease auto-passes (see
  # combine_decision()'s pass_ids); carry them so the relaxed final is scored over
  # the same population, or every coverage's baseline would look inflated.
  pass_ids <- setdiff(final$id, unique(as.data.table(applied)$id))

  new_final <- combine_decision(relaxed, decision_table, exclusion_table,
                                reduction_table, loading_table, decision_cols = decision_cols,
                                pass_ids = pass_ids)
  relaxed_share <- tabulate_decision(new_final)[, .(auto_relaxed = sum(prop[auto == "1"])),
                                                by = coverage]

  out <- merge(base, relaxed_share, by = "coverage")
  out[, lift := auto_relaxed - auto_base]
  out[, n_flipped := round(lift * n_total)]
  out[, n_total := NULL]
  setcolorder(out, c("coverage", "auto_base", "auto_relaxed", "lift", "n_flipped"))
  setorder(out, -lift)
  setattr(out, "class", c("relaxation_simulation", "data.table", "data.frame"))
  out
}
