#' Decompose the joint rule impact into marginal, joint, and synergy per coverage
#'
#' Relaxing a set of rules at once lifts the automation rate by *more* than the
#' sum of relaxing each alone: a cell referred by two of them flips
#' only when both are relaxed, so it is credited to neither marginal. This splits
#' the joint rule impact, per coverage, into the sum of the marginals
#' (`individual`), the actual joint lift (`joint`), and the `synergy`
#' between them -- the co-held cells the marginals miss. Every piece is an exact
#' re-combine via [relax_rule()].
#'
#' @param applied Per-disease decisions from [match_rule()] (`$applied`).
#' @param combined The baseline wide decision table from [combine_decision()].
#' @param kcd_main A character vector of two or more exact representative codes
#'   (e.g. `c("M543", "M542", "M13")`) to relax together. Each is matched exactly;
#'   for regex families relax them with [relax_rule()] directly.
#' @param coverage Optional coverage name(s) to restrict to (e.g. `"adb"` or
#'   `c("hos", "sur")`); default `NULL` decomposes every coverage.
#' @return A `data.table` with one row per `(coverage, component)` -- `coverage`,
#'   `component` (`individual`/`joint`/`synergy`), `n_flipped` (insured x
#'   coverage cells no longer referred), and `auto_lift` (`n_flipped` over
#'   that coverage's cells).
#' @seealso [relax_rule()] for one rule's per-coverage detail,
#'   [list_rule_impact()] for every rule's marginal impact.
#' @export
decompose_rule_impact <- function(applied, combined, kcd_main, coverage = NULL) {
  targets <- unique(kcd_main)
  if (length(targets) < 2L)
    stop("`kcd_main` must name at least two representative codes to relax together.")

  combined_dt <- as.data.table(combined)
  cov_cols <- setdiff(names(combined_dt), "id")
  if (!is.null(coverage)) cov_cols <- intersect(cov_cols, coverage)

  # per-coverage n_flipped for a regex, scoped to cov_cols
  flips_cov <- function(regex)
    relax_rule(applied, combined, regex, coverage = cov_cols)[, .(coverage, n_flipped)]

  joint_cov <- flips_cov(paste0("^(", paste(targets, collapse = "|"), ")$"))
  setnames(joint_cov, "n_flipped", "joint")
  marginal_cov <- rbindlist(lapply(targets, function(code) flips_cov(paste0("^", code, "$"))))
  marginal_cov <- marginal_cov[, .(individual = sum(n_flipped)), by = coverage]

  merged <- merge(joint_cov, marginal_cov, by = "coverage", all = TRUE)
  merged[is.na(joint), joint := 0L]
  merged[is.na(individual), individual := 0L]
  merged[, synergy := joint - individual]
  cells <- data.table(coverage = cov_cols,
                      n_cov = vapply(cov_cols, function(cc)
                        sum(!is.na(combined_dt[[cc]]) & nzchar(combined_dt[[cc]])), integer(1)))
  merged <- merge(merged, cells, by = "coverage", all.x = TRUE)

  parts <- c("individual", "joint", "synergy")
  out <- melt(merged, id.vars = c("coverage", "n_cov"), measure.vars = parts,
              variable.name = "component", value.name = "n_flipped")
  out[, auto_lift := n_flipped / n_cov]
  out[, `:=`(n_cov = NULL, component = factor(component, levels = parts))]
  setcolorder(out, c("coverage", "component", "n_flipped", "auto_lift"))
  setorder(out, coverage, component)
  out[]
}
