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
#' @param final A wide final-decision table from [combine_decision()]. Its
#'   `decision_table` attribute (attached by [combine_decision()]) supplies the
#'   manual-review code for the `auto` column; when that attribute is absent,
#'   `auto` is omitted.
#' @param id_col Name of the id column to exclude from the coverages
#'   (default `"id"`).
#' @return A long `data.table` with columns `coverage`, `decision`, `category`
#'   (the decision's distinct class letters, e.g. `"R03(34),R04(34)"` -> `"R"`,
#'   `"R(99),L(24),E(25)"` -> `"E,L,R"`), `n` (insured count), and `ratio` (`n`
#'   over the coverage's total, so each coverage's ratios sum to 1). When `final`
#'   carries a `decision_table` attribute, an `auto` column is inserted after
#'   `category` (`0` when the decision is manual review, `1` for every
#'   auto-decided outcome).
#' @seealso [combine_decision()].
#' @export
tabulate_decision <- function(final, id_col = "id") {
  decision_table <- attr(final, "decision_table")
  final <- as.data.table(final)
  coverages <- setdiff(names(final), id_col)
  long <- melt(final, id.vars = id_col, measure.vars = coverages,
               variable.name = "coverage", value.name = "decision",
               variable.factor = FALSE)
  out <- long[, .(n = .N), by = .(coverage, decision)]
  out[, category := .decision_category(decision)]
  if (!is.null(decision_table)) {
    role <- decision_table$role
    manual_review <- decision_table$code[!is.na(role) & role == "manual_review"][1L]
    out[, auto := as.integer(category != manual_review)]
  }
  out[, ratio := n / sum(n), by = coverage]
  cols <- c("coverage", "decision", "category", if (!is.null(decision_table)) "auto", "n", "ratio")
  setcolorder(out, cols)
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
