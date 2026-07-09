#' Decompose the joint lift of relaxing several rules together
#'
#' Relaxing a set of rules at once lifts the automation rate by *more* than the
#' sum of relaxing each alone: a cell held on manual review by two of them flips
#' only when both are relaxed, so it is credited to neither marginal. This splits
#' the joint effect into the sum of the marginals (`individual`), the actual
#' combined lift (`combined`), and the `synergy` between them -- the co-held cells
#' the marginals miss. Every piece is an exact re-combine via [relax_rule()].
#'
#' @param applied Per-disease decisions from [match_rule()] (`$applied`).
#' @param final The baseline wide decision table from [combine_decision()].
#' @param kcd_main A character vector of two or more exact representative codes
#'   (e.g. `c("M543", "M542", "M13")`) to relax together. Each is matched exactly;
#'   for regex families relax them with [relax_rule()] directly.
#' @param coverage Optional coverage name(s) to decompose over (e.g. `"adb"` or
#'   `c("hos", "sur")`); default `NULL` decomposes over every coverage.
#' @return A three-row `data.table` with `component`
#'   (`individual`/`combined`/`synergy`), `n_flipped` (insured x coverage cells
#'   moved off manual review), and `auto_lift` (`n_flipped` over the decision
#'   cells). `individual` is the sum of each rule relaxed alone, `combined` the
#'   set relaxed at once, `synergy` their difference.
#' @seealso [relax_rule()] for one rule's per-coverage detail,
#'   [list_rule_impact()] for every rule's marginal impact.
#' @export
decompose_relaxed_rule <- function(applied, final, kcd_main, coverage = NULL) {
  targets <- unique(kcd_main)
  if (length(targets) < 2L)
    stop("`kcd_main` must name at least two representative codes to relax together.")

  # denominator = evaluated (non-empty) decision cells over the scoped coverages,
  # matching list_rule_impact()
  final_dt <- as.data.table(final)
  cov_cols <- setdiff(names(final_dt), "id")
  if (!is.null(coverage)) cov_cols <- intersect(cov_cols, coverage)
  total_cells <- sum(vapply(cov_cols,
                            function(cc) sum(!is.na(final_dt[[cc]]) & nzchar(final_dt[[cc]])),
                            integer(1)))

  flips <- function(regex) sum(relax_rule(applied, final, regex, coverage = coverage)$n_flipped)
  marginal   <- vapply(targets, function(code) flips(paste0("^", code, "$")), numeric(1))
  individual <- sum(marginal)
  combined   <- flips(paste0("^(", paste(targets, collapse = "|"), ")$"))

  out <- data.table(
    component = factor(c("individual", "combined", "synergy"),
                       levels = c("individual", "combined", "synergy")),
    n_flipped = c(individual, combined, combined - individual))
  out[, auto_lift := n_flipped / total_cells]
  out[]
}
