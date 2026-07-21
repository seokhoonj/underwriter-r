# Every SI table -- product windows, coverage vocabulary, carve-out bands,
# critical-illness classes, decline reasons -- lives in one workbook, the
# simplified-issue counterpart of the standard rulebook. Nothing insurer-specific
# is stated in code: adding a product or a coverage is a workbook row, never an
# edit here. The `disease` (kcd -> kcd_main) table is deliberately NOT part of it;
# the SI and standard paths share the standard table, and a second copy would drift.

# Reason keys the engine can emit. Kept as one vector because two places must
# agree on it: the loader checks the workbook supplies wording for each, and the
# engine writes exactly these into a decline's `reason`.
.SI_REASON_KEYS <- c("recent_treatment", "critical_disease", "no_rule",
                     "hos_day_over", "sur_cnt_over", "elp_day_short",
                     "age_out_of_band", "kcd_n_over")

# The three roles the engine speaks. It never names a code letter; the workbook
# maps role -> letter, so renaming a code is a cell edit, not an engine edit.
.SI_ROLES <- c("decline", "underwriter", "standard")

#' Read a simplified-issue rulebook workbook
#'
#' Reads the seven sheets of a simplified-issue rulebook and returns them as one
#' validated list, the counterpart of the standard [combine_ruleset()] input. The
#' workbook is assembled offline from the delivered rule sheets; this function
#' only reads and checks.
#'
#' Besides the sheets it adds three derived lookups the engine relies on: `code`
#' (role -> code letter), `rank` (code letter -> priority, lower is worse), and
#' `auto` (code letter -> 1 automated / 0 referred). Validation fails loudly at
#' the boundary rather than deep in the engine: a duplicated band key, a missing
#' role, a reason key with no wording, or a `priority` that does not order
#' decline worst all stop here, where the cause is visible.
#'
#' @param path Path to the rulebook workbook (`.xlsx`).
#' @return A named list with the sheet tables `product`, `coverage`, `decision`,
#'   `ruleset`, `sentinel`, `critical_disease`, `reason`, plus the derived
#'   lookups `code` (role -> letter), `rank` (letter -> priority) and `auto`
#'   (letter -> 0/1).
#' @seealso [si_product()], [match_si_rule()], [combine_si_decision()].
#' @export
load_si_rulebook <- function(path) {
  si_type <- coverage <- kcd_main <- class <- critical_disease_set <- N <- NULL  # NSE

  sh <- function(s) as.data.table(suppressWarnings(readxl::read_excel(path, sheet = s)))
  rb <- list(product          = sh("product"),
             coverage         = sh("coverage"),
             decision         = sh("decision"),
             ruleset          = sh("ruleset"),
             sentinel         = sh("ruleset_sentinel"),
             critical_disease = sh("critical_disease"),
             reason           = sh("reason"))

  missing_role <- setdiff(.SI_ROLES, rb$decision$role)
  if (length(missing_role))
    stop("`decision` sheet is missing role(s): ", paste(missing_role, collapse = ", "),
         "\nthe engine emits these three roles and looks up their codes here.")

  missing_reason <- setdiff(.SI_REASON_KEYS, rb$reason$reason)
  if (length(missing_reason))
    stop("`reason` sheet is missing key(s) the engine emits: ",
         paste(missing_reason, collapse = ", "))

  # a key present but with a blank wording surfaces later as an NA in
  # list_si_rule_impact()'s join, far from the empty cell that caused it
  blank_reason <- rb$reason$reason[is.na(rb$reason$reason_ko) | rb$reason$reason_ko == ""]
  if (length(blank_reason))
    stop("`reason` sheet has key(s) with no reason_ko wording: ",
         paste(blank_reason, collapse = ", "))

  bad_role <- setdiff(rb$sentinel$role, rb$decision$role)
  if (length(bad_role))
    stop("`ruleset_sentinel` names role(s) absent from `decision`: ",
         paste(bad_role, collapse = ", "))

  # a repeated role or code letter would make the lookups below pick one silently
  if (anyDuplicated(rb$decision$role) || anyDuplicated(rb$decision$code))
    stop("`decision` sheet repeats a role or a code; each must be unique.")

  rb$code <- setNames(as.character(rb$decision$code), rb$decision$role)
  rb$rank <- setNames(as.integer(rb$decision$priority), rb$decision$code)
  rb$auto <- setNames(as.integer(rb$decision$auto),    rb$decision$code)

  # combine_si_decision() takes the smallest-rank answer as the worst, which is
  # only correct if decline < underwriter < standard. That ordering is a workbook
  # choice with nothing enforcing it, so a `priority` authored the other way would
  # silently accept every decline. Pin it here.
  if (anyNA(rb$auto) || !all(rb$auto %in% c(0L, 1L)))
    stop("`decision$auto` must be 0/1; got: ", paste(unique(rb$decision$auto), collapse = ", "))
  if (anyNA(rb$rank))
    stop("`decision$priority` must be an integer on every row.")
  if (!(rb$rank[rb$code[["decline"]]] < rb$rank[rb$code[["underwriter"]]] &&
        rb$rank[rb$code[["underwriter"]]] < rb$rank[rb$code[["standard"]]]))
    stop("`decision$priority` must order decline < underwriter < standard ",
         "(smaller is worse); the fold takes the smallest as the worst.")

  # read_excel returns every number as double; the month windows are counted,
  # compared and passed to .minus_months() as integers, so coerce at the boundary.
  rb$product[, si_type := as.character(si_type)]
  month_cols <- intersect(c("medical_advice_mon", "medical_advice_lookback_mon",
                            "inpatient_surgery_mon", "critical_disease_mon", "care_mon"),
                          names(rb$product))
  rb$product[, (month_cols) := lapply(.SD, as.integer), .SDcols = month_cols]
  rb$critical_disease[, class := as.integer(class)]
  rb$ruleset[, si_type := as.character(si_type)]

  # a band bound left blank makes .match_band()'s comparison return NA, which
  # combine_si_decision() halts on -- but one function later, pointing at the fold
  # rather than the offending cell. Catch it here, where the row is named.
  band_cols <- intersect(c("age_min", "age_max", "elp_day_min", "sur_cnt_min",
                           "sur_cnt_max", "hos_day_min", "hos_day_max", "kcd_max"),
                         names(rb$ruleset))
  blank_band <- rb$ruleset[rowSums(is.na(rb$ruleset[, ..band_cols])) > 0L,
                           .(si_type, coverage, kcd_main)]
  if (nrow(blank_band))
    stop("`ruleset` has band row(s) with a blank bound (", nrow(blank_band),
         "); the first is ", paste(unlist(blank_band[1L]), collapse = "/"), ".")

  # a band is looked up by (si_type, coverage, kcd_main) and expected to return
  # one row; a repeat would pick whichever sorted first
  dup <- rb$ruleset[, .N, by = .(si_type, coverage, kcd_main)][N > 1L]
  if (nrow(dup))
    stop("`ruleset` key (si_type, coverage, kcd_main) repeats ", nrow(dup), " time(s).")
  cd_dup <- rb$critical_disease[, .N, by = .(critical_disease_set, coverage, kcd_main)][N > 1L]
  if (nrow(cd_dup))
    stop("`critical_disease` key (critical_disease_set, coverage, kcd_main) repeats ",
         nrow(cd_dup), " time(s).")

  absent <- setdiff(unique(rb$product$critical_disease_set),
                    unique(rb$critical_disease$critical_disease_set))
  if (length(absent))
    stop("critical_disease_set(s) named in `product` but absent from `critical_disease`: ",
         paste(absent, collapse = ", "))

  setkey(rb$ruleset, si_type, coverage, kcd_main)
  setkey(rb$critical_disease, critical_disease_set, coverage, kcd_main)
  rb
}
