#' Match aggregated inputs against a rule set
#'
#' For each `(id, kcd_main)` input, finds the rule row whose bands it falls in
#' and attaches that rule's per-product decisions. Matching is a non-equi band
#' join: `kcd_main` equality plus `age`, `elp_day`, `sur_cnt`, `hos_day` bands.
#' `out_day` is left unconstrained in the rule set, so `out_cnt` is not matched
#' on. Only no-declaration rules (`decl_yn == 0`) are used -- declaration-
#' dependent rows hinge on facts the claim feed does not carry. A disease with no
#' matching rule is kept with `matched == 0` so it can be referred to the underwriter.
#'
#' Near-all multi-matches are identical duplicates; the lowest-`ord` match is
#' kept and the few genuine conflicts are flagged in `conflict`. The rule set is
#' expected to follow a fixed schema: keys, band bounds, and declaration
#' attributes (the non-decision columns), plus one decision column per product.
#'
#' @param aggregated Per-`(id, kcd_main)` inputs from [aggregate_disease()],
#'   carrying `age`.
#' @param ruleset A rule-set `data.table`.
#' @param decision_cols The coverage decision columns. By default every column not
#'   in the fixed set of key / band / declaration-attribute columns
#'   (`.NON_DECISION_COLS`) -- which assumes the rule set carries only those
#'   attributes plus one column per coverage. Adding an unrecognised non-coverage
#'   column to the sheet would make the default treat it as a phantom coverage, so
#'   pass `decision_cols` explicitly when the rule set carries extra attribute
#'   columns.
#' @return A list with `applied` (the input plus `matched`, `conflict`, and one
#'   decision column per product; the decision-column names are stored on the
#'   `"decision_cols"` attribute), `decision_cols`, `unmatched` (the input rows
#'   no rule matched), `multi_matched` (each multi-matched input joined to the
#'   overlapping rules it hit, for rule-set cleanup), `conflict` (the subset of
#'   `multi_matched` whose matched rules disagree on a decision), and three
#'   diagnostic counts: `n_unmatched` (inputs no rule matched), `n_multi_matched`
#'   (inputs matched by more than one rule), and `n_conflict` (inputs whose
#'   matched rules disagree, so `n_conflict <= n_multi_matched`).
#' @seealso [combine_decision()].
#' @export
match_rule <- function(aggregated, ruleset,
                       decision_cols = setdiff(names(ruleset), .NON_DECISION_COLS)) {
  ruleset <- as.data.table(ruleset)[decl_yn == 0L]
  decision_cols <- intersect(decision_cols, names(ruleset))

  input <- as.data.table(copy(aggregated))
  input[, rid := .I]

  # keep only the band-match columns + rule id + decisions; the non-equi join
  # scrambles the band columns, so we carry nothing else and merge results back
  # onto the clean input by rid.
  rule_bands <- ruleset[, c("kcd_main", "age_min", "age_max", "elp_day_min", "elp_day_max",
                         "sur_cnt_min", "sur_cnt_max", "hos_day_min", "hos_day_max",
                         "no", "ord", decision_cols), with = FALSE]
  joined <- rule_bands[input, on = .(kcd_main,
      age_min     <= age,     age_max     >= age,
      elp_day_min <= elp_day, elp_day_max >= elp_day,
      sur_cnt_min <= sur_cnt, sur_cnt_max >= sur_cnt,
      hos_day_min <= hos_day, hos_day_max >= hos_day),
      nomatch = NA, allow.cartesian = TRUE]        # one row per (input x matching rule); carries rid
  joined[, matched := as.integer(!is.na(no))]

  # one decision per input row: the lowest-ord match (near-all ties are identical)
  setorder(joined, rid, ord)
  first_match <- unique(joined, by = "rid")
  first_match <- first_match[, c("rid", "matched", "no", "ord", decision_cols), with = FALSE]

  # inputs matched by more than one rule; near-all are identical duplicates, but
  # some disagree on a decision -- those are the genuine conflicts.
  matches_per_input <- joined[matched == 1L, .N, by = rid]
  n_distinct        <- joined[matched == 1L, uniqueN(.SD), by = rid, .SDcols = decision_cols]
  conflict_ids      <- n_distinct[V1 > 1L, rid]
  multi_matched_ids <- matches_per_input[N > 1L, rid]

  applied <- merge(input, first_match, by = "rid", all.x = TRUE)
  applied[, conflict := rid %in% conflict_ids]
  setattr(applied, "decision_cols", decision_cols)   # combine_decision reads it from the attribute

  # each multi-matched input joined to the overlapping rules it hit, so an
  # over-broad or duplicated rule set can be found and cleaned.
  multi_matched <- merge(input[rid %in% multi_matched_ids],
                         joined[rid %in% multi_matched_ids, c("rid", "no", "ord", decision_cols), with = FALSE],
                         by = "rid", allow.cartesian = TRUE)
  setorder(multi_matched, rid, ord)
  conflict <- multi_matched[rid %in% conflict_ids]   # the multi-matched rules that disagree on a decision

  list(
    applied         = applied,
    decision_cols   = decision_cols,
    unmatched       = applied[matched == 0L, .SD, .SDcols = names(aggregated)],
    multi_matched   = multi_matched,
    conflict        = conflict,
    n_unmatched     = applied[matched == 0L, .N],
    n_multi_matched = length(multi_matched_ids),
    n_conflict      = length(conflict_ids)
  )
}

# rule-set columns that are keys / conditions / declaration attributes; every
# other column is a per-product decision output.
.NON_DECISION_COLS <- c("no", "kcd_main", "kcd_main_ko", "n", "ord", "decl_yn",
                        "age_min", "age_max", "elp_day_min", "elp_day_max",
                        "sur_cnt_min", "sur_cnt_max", "hos_day_min", "hos_day_max",
                        "out_day_min", "out_day_max",
                        "recover", "recur", "treat", "severe", "cause", "medical_checkup")
