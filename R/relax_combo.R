#' Decompose the joint auto-rate lift of relaxing several diseases together
#'
#' Relaxing a set of representative diseases at once lifts the automation rate by
#' *more* than the sum of relaxing each alone: a cell held on manual review by two
#' of them flips only when both are relaxed, so it is credited to neither
#' marginal. This splits the joint effect into the sum of the marginals, the
#' actual combined lift, and the `synergy` between them (the co-held cells the
#' marginals miss).
#'
#' The combined and marginal lifts are exact re-combines via [relax_disease()], so
#' the same `mode` applies to both. Under `"review_only"` the synergy is always
#' non-negative (co-holding only). Under `"full"` it also absorbs decline
#' unmasking, so it can be negative -- read it as the interaction, not purely
#' co-holding.
#'
#' @param applied Per-disease decisions from [match_rule()] (`$applied`).
#' @param final The baseline wide decision table from [combine_decision()].
#' @param kcd_main A character vector of two or more exact representative codes
#'   (e.g. `c("M543", "M542", "M13")`) to relax together. Each is matched exactly;
#'   for regex families relax them with [relax_disease()] directly.
#' @param mode Passed to [relax_disease()]: `"review_only"` (default) or `"full"`.
#' @return A three-row `data.table` with `component`
#'   (`individual`/`combined`/`synergy`), `n_flipped` (insured x coverage cells
#'   moved off manual review), and `auto_lift` (`n_flipped` over the decision
#'   cells). `individual` is the sum of each disease relaxed alone, `combined` the
#'   set relaxed at once, `synergy` their difference.
#' @seealso [relax_disease()] for one disease's per-coverage detail,
#'   [relax_impact()] to rank diseases individually.
#' @export
relax_combo <- function(applied, final, kcd_main, mode = c("review_only", "full")) {
  mode    <- match.arg(mode)
  targets <- unique(kcd_main)
  if (length(targets) < 2L)
    stop("`kcd_main` must name at least two representative codes to relax together.")
  total_cells <- nrow(final) * (ncol(final) - 1L)

  flips <- function(regex) sum(relax_disease(applied, final, regex, mode = mode)$n_flipped)
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
