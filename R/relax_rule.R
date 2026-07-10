#' Relax a rule (or set of rules) and measure the per-coverage lift
#'
#' What-if experiment: if the rule(s) for the given representative disease(s)
#' (`kcd_main`) were relaxed -- their manual-review decisions turned into the
#' standard (auto) code, keeping every exclusion / loading / reduction / decline
#' -- how much would each coverage's auto-decided share rise? The target's
#' manual-review cells are set to standard, the decisions are re-combined with
#' [combine_decision()], and the auto share is re-tabulated against the baseline.
#'
#' Because a coverage routes to manual review when *any* of an insured's diseases
#' does, the lift is computed by re-running the full combine -- an insured still
#' held for review by another disease does not flip. Only manual-review decisions
#' are relaxed (never declines or restrictions), so the auto share can only rise;
#' fully waiving a decline could unmask a manual review it had outranked and
#' *lower* the auto share, which is why that is deliberately not done.
#'
#' @param applied Per-disease decisions from [match_rule()] (`$applied`).
#' @param combined The baseline wide decision table from [combine_decision()],
#'   carrying its config-table attributes.
#' @param kcd_main One or more patterns matched against `kcd_main`, relaxed
#'   together. A single string is a regex: `"M51"` relaxes M51 and any sub-code
#'   (`M510`, `M512`, ...), and `"^M51$"` relaxes exactly `M51`. A character
#'   vector is OR'd, so the representative codes from [list_rule_impact()] can be
#'   passed straight in -- `c("M543", "M542", "M13")` relaxes all three at once
#'   (same as `"M543|M542|M13"`). Codes carry no regex metacharacters, so a plain
#'   code is a safe substring pattern.
#' @param coverage Optional coverage name(s) to restrict the result to (e.g.
#'   `"adb"` or `c("hos", "sur")`); default `NULL` keeps every coverage.
#' @return A `relaxed_rule` object (a `data.table`), one row per coverage sorted
#'   by `lift` descending, with `auto_base` (baseline auto share), `auto_relaxed`
#'   (after relaxing), `lift` (the increase), and `n_flipped` (insured moved from
#'   manual review to auto). Relaxing several codes at once gives the *joint*
#'   effect (synergy included); compare `sum(n_flipped)` to the sum of the codes'
#'   individual marginals from [list_rule_impact()] to read the synergy.
#' @seealso [list_rule_impact()] for every rule's marginal impact,
#'   [tabulate_decision()], [combine_decision()].
#' @export
relax_rule <- function(applied, combined, kcd_main, coverage = NULL) {
  if (length(kcd_main) < 1L || anyNA(kcd_main) || any(!nzchar(kcd_main)))
    stop("`kcd_main` must be one or more non-empty patterns.")
  decision_table  <- attr(combined, "decision_table")
  exclusion_table <- attr(combined, "exclusion_table")
  reduction_table <- attr(combined, "reduction_table")
  loading_table   <- attr(combined, "loading_table")
  decision_cols   <- attr(applied, "decision_cols")
  if (is.null(decision_table) || is.null(decision_cols))
    stop("`combined` must come from combine_decision() and `applied` from match_rule().")

  role          <- decision_table$role
  standard      <- decision_table$code[!is.na(role) & role == "standard"][1L]
  manual_review <- decision_table$code[!is.na(role) & role == "manual_review"][1L]
  target        <- paste(kcd_main, collapse = "|")   # a vector of codes -> OR'd

  base <- tabulate_decision(combined)[, .(auto_base = sum(prop[auto == "1"]),
                                          n_total   = sum(n)), by = coverage]

  # relax the target diseases to standard. An unmatched disease drives manual
  # review on every coverage (combine fills it), so relax it everywhere; a matched
  # disease decides only some coverages, so relax just those -- leaving the ones it
  # is silent on untouched, or an insured would be pulled into a new coverage.
  relaxed <- copy(as.data.table(applied))
  tgt <- grepl(target, relaxed$kcd_main)
  relaxed[tgt & matched == 0L, (decision_cols) := standard]
  for (col in decision_cols)   # relax only the manual-review cells, keep restrictions
    relaxed[tgt & matched == 1L & get(col) == manual_review, (col) := standard]
  relaxed[tgt, matched := 1L]

  # ids in the baseline but not in `applied` are no-disease auto-passes (see
  # combine_decision()'s pass_ids); carry them so the relaxed result is scored over
  # the same population, or every coverage's baseline would look inflated.
  pass_ids <- setdiff(combined$id, unique(as.data.table(applied)$id))

  new_combined <- combine_decision(relaxed, decision_table, exclusion_table,
                                   reduction_table, loading_table, decision_cols = decision_cols,
                                   pass_ids = pass_ids)
  relaxed_share <- tabulate_decision(new_combined)[, .(auto_relaxed = sum(prop[auto == "1"])),
                                                   by = coverage]

  out <- merge(base, relaxed_share, by = "coverage")
  out[, lift := auto_relaxed - auto_base]
  out[, n_flipped := round(lift * n_total)]
  out[, n_total := NULL]
  setcolorder(out, c("coverage", "auto_base", "auto_relaxed", "lift", "n_flipped"))
  setorder(out, -lift)
  if (!is.null(coverage)) { pick <- coverage; out <- out[coverage %in% pick] }
  setattr(out, "class", c("relaxed_rule", "data.table", "data.frame"))
  out[]
}
