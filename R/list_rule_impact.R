#' List each rule's marginal (independent) auto-rate impact by coverage
#'
#' For every representative disease (`kcd_main`) whose rule refers a coverage to
#' the underwriter, the *marginal* lift of relaxing that rule on its own: the
#' insured and the cells that would flip to auto, per coverage. Each rule is
#' scored **independently** (no synergy across rules) and ranked within each
#' coverage, so the result is a per-coverage list of marginal rule impacts -- the
#' shortlist of what to relax on each coverage. For the *joint* effect of relaxing
#' several rules together (synergy included) use [relax_rule()]; to split that
#' into marginals vs synergy use [decompose_rule_impact()].
#'
#' A referred cell counts for a rule only when that rule is its *sole* source of
#' the referral (another disease still holding it keeps it referred), so
#' the marginal is exact for relaxing one rule at a time without re-running
#' [combine_decision()] per candidate. Cells already auto-decided -- including
#' ones a decline outranks -- are not counted, so the auto share can only rise.
#'
#' @param applied Per-disease decisions from [match_rule()] (`$applied`).
#' @param combined The wide decision table from [combine_decision()], carrying its
#'   config-table attributes.
#' @param coverage Optional coverage name(s) to restrict to (e.g. `"adb"` or
#'   `c("hos", "sur")`); default `NULL` lists every coverage.
#' @return A `rule_impact_list` (a `data.table`) with `coverage`, `kcd_main`,
#'   `n_id` (insured no longer referred), `n_flipped` (cells flipped), and
#'   `auto_lift` (`n_flipped` over that coverage's cells), sorted within each
#'   coverage by descending `n_flipped`.
#' @seealso [relax_rule()] for one rule's per-coverage detail,
#'   [decompose_rule_impact()] for a rule set's marginal/joint/synergy split.
#' @export
list_rule_impact <- function(applied, combined, coverage = NULL) {
  decision_cols  <- attr(applied, "decision_cols")
  decision_table <- attr(combined, "decision_table")
  if (is.null(decision_cols) || is.null(decision_table))
    stop("`combined` must come from combine_decision() and `applied` from match_rule().")
  priority    <- setNames(as.integer(decision_table$priority), decision_table$code)
  underwriter <- .decision_letters(decision_table, priority)$underwriter

  # cells referred to the underwriter in the combined -- the only ones that flip
  combined_long <- melt(as.data.table(combined), id.vars = "id", variable.name = "coverage",
                        value.name = "decision", variable.factor = FALSE)
  combined_long <- combined_long[!is.na(decision) & nzchar(decision)]
  if (!is.null(coverage)) { cov <- coverage; combined_long <- combined_long[coverage %in% cov] }
  referred_cells <- combined_long[decision == underwriter, .(id, coverage)]

  # the diseases that refer each coverage: matched cells holding the underwriter
  # code, plus unmatched diseases (referred on every coverage)
  applied_dt <- as.data.table(applied)
  matched_referred <- melt(applied_dt[matched == 1L], id.vars = c("id", "kcd_main"),
                           measure.vars = decision_cols, variable.name = "coverage",
                           value.name = "code", variable.factor = FALSE)[
                           code == underwriter, .(id, kcd_main, coverage)]
  unmatched_referred <- applied_dt[matched == 0L, .(id, kcd_main)][, .(coverage = decision_cols),
                                                                   by = .(id, kcd_main)]
  referred_src <- rbindlist(list(matched_referred, unmatched_referred))
  referred_src <- referred_src[referred_cells, on = .(id, coverage), nomatch = NULL]   # real cells only

  referred_src[, n_causes := uniqueN(kcd_main), by = .(id, coverage)]
  sole <- referred_src[n_causes == 1L]                            # sole cause -> flips if relaxed
  out <- sole[, .(n_id = uniqueN(id), n_flipped = .N), by = .(kcd_main, coverage)]
  out <- merge(out, combined_long[, .(n_cov = .N), by = coverage], by = "coverage")
  out[, auto_lift := n_flipped / n_cov][, n_cov := NULL]
  setcolorder(out, c("coverage", "kcd_main", "n_id", "n_flipped", "auto_lift"))
  setorder(out, coverage, -n_flipped)
  setattr(out, "class", c("rule_impact_list", "data.table", "data.frame"))
  out[]
}
