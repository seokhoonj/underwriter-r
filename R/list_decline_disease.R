#' Rank the diseases behind automated declines, by coverage
#'
#' For every representative disease (`kcd_main`) that decides `D` on a coverage,
#' the number of `(insured x coverage)` cells it drives to an automated decline.
#' Where [list_rule_impact()] asks "which referral would relaxing a rule flip",
#' this asks the plainer question the decline analysis needs: which diseases
#' produce the declines that already stand.
#'
#' `D` outranks every other code in the combiner, so a disease that decides `D`
#' on a coverage decides the insured's final decision there. Counts are per
#' `(insured x disease x coverage)` contribution: one declined cell can be driven
#' by several diseases at once, so these contributions sum to MORE than the
#' declined-cell count. That is deliberate -- the question is "which diseases
#' cause declines", not "how many cells declined" -- but it means `share` is a
#' share of contributions, not of cells. The declined-cell total is returned as
#' the `decline_cells` attribute for an honest denominator.
#'
#' @param applied Per-disease decisions from [match_rule()] (`$applied`), carrying
#'   its `decision_cols` attribute.
#' @param coverage Optional coverage(s) to restrict to; `NULL` for all.
#' @param n_top How many diseases to return.
#' @return A `data.table` of `kcd_main`, `n` (contribution count), `share`
#'   (percent of contributions), ranked by descending count, with a
#'   `decline_cells` attribute holding the true number of declined cells.
#' @seealso [list_rule_impact()] for the relax-to-flip counterfactual,
#'   [tabulate_decision()] for the decision distribution.
#' @export
list_decline_disease <- function(applied, coverage = NULL, n_top = 20L) {
  kcd_main <- code <- n <- NULL   # data.table NSE
  decision_cols <- attr(applied, "decision_cols")
  if (is.null(decision_cols))
    stop("`applied` must come from match_rule() (missing `decision_cols` attribute).")

  cols <- if (is.null(coverage)) decision_cols else intersect(coverage, decision_cols)
  if (!length(cols))
    stop("none of `coverage` are decision columns; have: ",
         paste(decision_cols, collapse = ", "))

  long <- melt(as.data.table(applied)[, c("id", "kcd_main", cols), with = FALSE],
               id.vars = c("id", "kcd_main"), variable.name = "coverage",
               value.name = "code", variable.factor = FALSE)
  declined <- long[code == "D"]
  if (!nrow(declined)) {
    out <- data.table(kcd_main = character(), n = integer(), share = numeric())
    setattr(out, "decline_cells", 0L)
    return(out[])
  }

  # one (id x disease x coverage) row is one contribution
  contrib <- declined[, .(n = .N), by = kcd_main]
  contrib[, share := round(100 * n / sum(n), 1)]
  setorder(contrib, -n)

  # true declined-cell count: an (id, coverage) cell counts once no matter how
  # many diseases drove it -- the honest denominator behind `share`.
  setattr(contrib, "decline_cells", nrow(unique(declined[, .(id, coverage)])))
  head(contrib, n_top)[]
}
