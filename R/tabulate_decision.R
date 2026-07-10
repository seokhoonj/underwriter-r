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
#' @param combined A wide combined-decision table from [combine_decision()],
#'   carrying the `decision_table` attribute it attaches; that table's `auto`
#'   column flags each code as auto (`1`) or manual review (`0`).
#' @param id_col Name of the id column to exclude from the coverages
#'   (default `"id"`).
#' @return A long `data.table` of class `tabulated_decision` (plottable with
#'   [plot()]) with columns `coverage`, `decision`, `category`
#'   (the decision's distinct class letters, e.g. `"R03(34),R04(34)"` -> `"R"`,
#'   `"R(99),L(24),E(25)"` -> `"E,L,R"`), `auto` (a factor with levels `0`/`1`:
#'   `1` only when every code in the decision is flagged auto, `0` when any code
#'   needs manual review -- the auto/manual line is controlled by editing
#'   `decision_table`'s `auto` column),
#'   `n` (insured count), and `prop` (`n` over the coverage's total, so each
#'   coverage's proportions sum to 1).
#' @seealso [combine_decision()].
#' @export
tabulate_decision <- function(combined, id_col = "id") {
  decision_table <- attr(combined, "decision_table")
  if (is.null(decision_table))
    stop("`combined` has no `decision_table` attribute; produce `combined` with combine_decision().")
  combined <- as.data.table(combined)
  coverages <- setdiff(names(combined), id_col)
  long <- melt(combined, id.vars = id_col, measure.vars = coverages,
               variable.name = "coverage", value.name = "decision",
               variable.factor = FALSE)
  long <- long[!is.na(decision) & nzchar(decision)]   # skip coverages an id was never evaluated on
  out <- long[, .(n = .N), by = .(coverage, decision)]
  out[, category := .decision_category(decision)]
  # per-code auto flag from decision_table; a decision counts as auto only when
  # every code in it is auto (0 if any component needs manual review).
  # coerce the auto flag to 0/1 whatever its storage: factor "0"/"1" via character,
  # logical TRUE/FALSE via as.integer directly, numeric/integer/character as-is.
  auto_raw <- decision_table$auto
  if (is.factor(auto_raw)) auto_raw <- as.character(auto_raw)
  auto_by_code <- setNames(as.integer(auto_raw), decision_table$code)
  out[, auto := factor(
    vapply(strsplit(category, ",", fixed = TRUE),
           function(codes) min(auto_by_code[codes]), integer(1)),
    levels = c(0L, 1L))]
  out[, prop := n / sum(n), by = coverage]
  setcolorder(out, c("coverage", "decision", "category", "auto", "n", "prop"))
  setorder(out, coverage, -n)
  # carry the rule-set coverage order so plot()'s order = "column" can read it here
  setattr(out, "decision_cols", attr(combined, "decision_cols"))
  setattr(out, "class", c("tabulated_decision", "data.table", "data.frame"))
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
