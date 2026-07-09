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
#' @param by_coverage If `TRUE`, break the decomposition down per coverage --
#'   columns `coverage`, `component`, `n_flipped`, `auto_lift`. Default `FALSE`
#'   aggregates over the (scoped) coverages into the three components.
#' @param coverage Optional coverage name(s) to restrict to (e.g. `"adb"` or
#'   `c("hos", "sur")`); default `NULL` uses every coverage.
#' @return A `data.table`. By default three rows -- `component`
#'   (`individual`/`combined`/`synergy`), `n_flipped` (insured x coverage cells
#'   moved off manual review), and `auto_lift` (`n_flipped` over the decision
#'   cells). With `by_coverage = TRUE`, one row per `(coverage, component)` with a
#'   leading `coverage` column and that coverage's own `auto_lift`.
#' @seealso [relax_rule()] for one rule's per-coverage detail,
#'   [list_rule_impact()] for every rule's marginal impact.
#' @export
decompose_relaxed_rule <- function(applied, final, kcd_main, by_coverage = FALSE,
                                   coverage = NULL) {
  targets <- unique(kcd_main)
  if (length(targets) < 2L)
    stop("`kcd_main` must name at least two representative codes to relax together.")

  final_dt <- as.data.table(final)
  cov_cols <- setdiff(names(final_dt), "id")
  if (!is.null(coverage)) cov_cols <- intersect(cov_cols, coverage)

  # per-coverage n_flipped for a regex, scoped to cov_cols
  flips_cov <- function(regex)
    relax_rule(applied, final, regex, coverage = cov_cols)[, .(coverage, n_flipped)]

  combined_cov <- flips_cov(paste0("^(", paste(targets, collapse = "|"), ")$"))
  setnames(combined_cov, "n_flipped", "combined")
  marginal_cov <- rbindlist(lapply(targets, function(code) flips_cov(paste0("^", code, "$"))))
  marginal_cov <- marginal_cov[, .(individual = sum(n_flipped)), by = coverage]

  m <- merge(combined_cov, marginal_cov, by = "coverage", all = TRUE)
  m[is.na(combined), combined := 0L]
  m[is.na(individual), individual := 0L]
  m[, synergy := combined - individual]
  cells <- data.table(coverage = cov_cols,
                      n_cov = vapply(cov_cols, function(cc)
                        sum(!is.na(final_dt[[cc]]) & nzchar(final_dt[[cc]])), integer(1)))
  m <- merge(m, cells, by = "coverage", all.x = TRUE)

  parts <- c("individual", "combined", "synergy")
  if (by_coverage) {
    out <- melt(m, id.vars = c("coverage", "n_cov"), measure.vars = parts,
                variable.name = "component", value.name = "n_flipped")
    out[, auto_lift := n_flipped / n_cov]
    out[, `:=`(n_cov = NULL, component = factor(component, levels = parts))]
    setcolorder(out, c("coverage", "component", "n_flipped", "auto_lift"))
    setorder(out, coverage, component)
  } else {
    total_cells <- sum(m$n_cov)
    out <- data.table(component = factor(parts, levels = parts),
                      n_flipped = c(sum(m$individual), sum(m$combined), sum(m$synergy)))
    out[, auto_lift := n_flipped / total_cells]
  }
  out[]
}
