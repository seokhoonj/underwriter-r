#' Rank diseases by the auto-rate lift of relaxing each
#'
#' Which representative disease, if relaxed, moves the most insured off manual
#' review? For every `kcd_main` that drives a coverage to manual review, counts
#' the insured and the `(insured x coverage)` cells that would flip to auto if it
#' were relaxed on its own, and ranks them.
#'
#' A manual-review cell flips only when the relaxed disease is its *sole*
#' manual-review source (another disease still holding it keeps it on review), so
#' the lift is exact for relaxing one disease at a time without re-running
#' [combine_decision()] per candidate. Cells already auto-decided -- including
#' ones a decline outranks -- are not counted.
#'
#' @param applied Per-disease decisions from [match_rule()] (`$applied`).
#' @param final The wide decision table from [combine_decision()], carrying its
#'   config-table attributes.
#' @return A `data.table`, one row per disease sorted by `n_flipped` descending,
#'   with `kcd_main`, `n_id` (insured moved off review), `n_flipped`
#'   (`insured x coverage` cells flipped), and `auto_lift` (`n_flipped` over all
#'   decision cells).
#' @seealso [relax_disease()] for one disease's per-coverage detail.
#' @export
rank_disease <- function(applied, final) {
  decision_cols  <- attr(applied, "decision_cols")
  decision_table <- attr(final, "decision_table")
  if (is.null(decision_cols) || is.null(decision_table))
    stop("`final` must come from combine_decision() and `applied` from match_rule().")
  role          <- decision_table$role
  manual_review <- decision_table$code[!is.na(role) & role == "manual_review"][1L]

  # cells that are on manual review in the final -- the only ones that can flip
  final_long <- melt(as.data.table(final), id.vars = "id", variable.name = "coverage",
                     value.name = "decision", variable.factor = FALSE)
  final_long <- final_long[!is.na(decision) & nzchar(decision)]
  u_cells     <- final_long[decision == manual_review, .(id, coverage)]
  total_cells <- nrow(final_long)

  # the diseases that route each coverage to manual review: matched cells holding
  # the manual-review code, plus unmatched diseases (review on every coverage)
  a <- as.data.table(applied)
  matched_u <- melt(a[matched == 1L], id.vars = c("id", "kcd_main"),
                    measure.vars = decision_cols, variable.name = "coverage",
                    value.name = "code", variable.factor = FALSE)[
                    code == manual_review, .(id, kcd_main, coverage)]
  unmatched_u <- a[matched == 0L, .(id, kcd_main)][, .(coverage = decision_cols),
                                                   by = .(id, kcd_main)]
  u_src <- rbindlist(list(matched_u, unmatched_u))
  u_src <- u_src[u_cells, on = .(id, coverage), nomatch = NULL]   # keep real review cells

  u_src[, n_src := uniqueN(kcd_main), by = .(id, coverage)]
  sole <- u_src[n_src == 1L]                                       # sole cause -> flips if relaxed
  out <- sole[, .(n_id = uniqueN(id), n_flipped = .N), by = kcd_main]
  out[, auto_lift := n_flipped / total_cells]
  setorder(out, -n_flipped)
  out[]
}
