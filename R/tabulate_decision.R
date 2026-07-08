#' Tabulate the decision distribution per coverage
#'
#' Summarizes a per-insured final decision table (one row per id, one column per
#' coverage, as returned by [combine_decision()]) into the decision distribution
#' of every coverage: for each coverage, how many insured got each decision and
#' what share of the book that is. The result is long -- one row per
#' `(coverage, decision)` -- sorted within each coverage by descending count, so
#' the most common outcome (standard, decline, review) leads and the long tail
#' of exclusion combinations follows.
#'
#' @param final A wide final-decision table from [combine_decision()].
#' @param id_col Name of the id column to exclude from the coverages
#'   (default `"id"`).
#' @return A long `data.table` with columns `coverage`, `decision`,
#'   `decision_category` (the decision's distinct class letters, e.g.
#'   `"R03(34),R04(34)"` -> `"R"`, `"R(99),L(24),E(25)"` -> `"E,L,R"`), `n`
#'   (insured count), and `ratio` (`n` over the coverage's total, so each
#'   coverage's ratios sum to 1).
#' @seealso [combine_decision()].
#' @export
tabulate_decision <- function(final, id_col = "id") {
  final <- as.data.table(final)
  coverages <- setdiff(names(final), id_col)
  long <- melt(final, id.vars = id_col, measure.vars = coverages,
               variable.name = "coverage", value.name = "decision",
               variable.factor = FALSE)
  out <- long[, .(n = .N), by = .(coverage, decision)]
  out[, decision_category := .decision_category(decision)]
  out[, ratio := n / sum(n), by = coverage]
  setcolorder(out, c("coverage", "decision", "decision_category", "n", "ratio"))
  setorder(out, coverage, -n)
  out[]
}

# Reduce a decision to its distinct class letters: split on commas, take each
# token's leading letter run (the class letter, e.g. "R03(34)" -> "R"), then
# keep the unique letters sorted. "R(99),L(24),E(25)" -> "E,L,R".
.decision_category <- function(decision) {
  vapply(strsplit(decision, ",", fixed = TRUE), function(tokens) {
    class_letters <- sub("^([A-Za-z]+).*$", "\\1", tokens)
    paste(sort(unique(class_letters)), collapse = ",")
  }, character(1))
}
