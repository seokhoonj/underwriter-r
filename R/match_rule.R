#' Match aggregated inputs against a rule set
#'
#' For each `(id, kcd_main)` input, finds the rule row whose bands it falls in
#' and attaches that rule's per-product decisions. Matching is a non-equi band
#' join: `kcd_main` equality plus `age`, `elp_day`, `sur_cnt`, `hos_day` bands.
#' `out_day` is left unconstrained in the rule set, so `out_cnt` is not matched
#' on. Only no-declaration rules (`decl_yn == 0`) are used -- declaration-
#' dependent rows hinge on facts the claim feed does not carry. A disease with no
#' matching rule is kept with `matched == 0` so it can route to manual review.
#'
#' Near-all multi-matches are identical duplicates; the lowest-`ord` match is
#' kept and the few genuine conflicts are flagged in `conflict`. The rule set is
#' expected to follow the kdb schema: keys, band bounds, and declaration
#' attributes (the non-decision columns), plus one decision column per product.
#'
#' @param agg Per-`(id, kcd_main)` inputs from [aggregate_disease_info()],
#'   carrying `age`.
#' @param rule A rule-set `data.table`.
#' @return A list with `applied` (the input plus `matched`, `conflict`, and one
#'   decision column per product; the decision-column names are stored on the
#'   `"decision_cols"` attribute), `decision_cols`, `n_norule`, `n_conflict`.
#' @seealso [combine_decision()].
#' @export
match_rule <- function(agg, rule) {
  rule <- as.data.table(rule)[decl_yn == 0L]
  decision_cols <- setdiff(names(rule), .NON_DECISION_COLS)

  input <- as.data.table(copy(agg))
  input[, rid := .I]

  # keep only the band-match columns + rule id + decisions; the non-equi join
  # scrambles the band columns, so we carry nothing else and merge results back
  # onto the clean input by rid.
  rule_bands <- rule[, c("kcd_main", "age_min", "age_max", "elp_day_min", "elp_day_max",
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

  # flag the few inputs whose matches disagree on any decision
  n_distinct   <- joined[matched == 1L, uniqueN(.SD), by = rid, .SDcols = decision_cols]
  conflict_ids <- n_distinct[V1 > 1L, rid]

  applied <- merge(input, first_match, by = "rid", all.x = TRUE)
  applied[, conflict := rid %in% conflict_ids]
  setattr(applied, "decision_cols", decision_cols)   # combine_decision reads it from the attribute
  list(
    applied       = applied,
    decision_cols = decision_cols,
    n_norule      = applied[matched == 0L, .N],
    n_conflict    = length(conflict_ids)
  )
}

# rule-set columns that are keys / conditions / declaration attributes; every
# other column is a per-product decision output.
.NON_DECISION_COLS <- c("no", "kcd_main", "kcd_main_ko", "n", "ord", "decl_yn",
                        "age_min", "age_max", "elp_day_min", "elp_day_max",
                        "sur_cnt_min", "sur_cnt_max", "hos_day_min", "hos_day_max",
                        "out_day_min", "out_day_max",
                        "recover", "recur", "treat", "severe", "cause", "medical_checkup")
