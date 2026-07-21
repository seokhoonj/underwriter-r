#' Resolve one simplified-issue product's configuration
#'
#' Pulls a single product's windows and coverage set out of a rulebook. A product
#' code's digits are its three application-form windows -- 325 is 3 months
#' medical-advice, 2 years inpatient-surgery, 5 years critical-disease -- but the
#' codes are not machine-parseable, so the `product` sheet states each window
#' explicitly and this reads them off it.
#'
#' @param si_type Product code, e.g. `"325"` or `"3105"`.
#' @param rulebook A rulebook from [load_si_rulebook()].
#' @return A named list: `si_type`; the four window months (`medical_advice_mon`,
#'   `medical_advice_lookback_mon`, `inpatient_surgery_mon`,
#'   `critical_disease_mon`); `critical_disease_set`; `coverages`, the coverages
#'   the product actually sells (read off the rule sheet, so 3105's absent `care`
#'   coverage cannot be mis-stated); a `critical_disease_window(coverage)` closure
#'   giving each coverage its window; and `critical_disease`, the product's slice
#'   of the critical-disease table.
#' @seealso [load_si_rulebook()], [match_si_rule()].
#' @export
si_product <- function(si_type, rulebook) {
  coverage <- care_mon <- NULL  # NSE

  # `target_type`, not a reused `si_type`: inside a data.table `i` a bare
  # `si_type` binds to the COLUMN, so `[si_type == si_type]` matches every row.
  target_type <- as.character(si_type)
  cfg <- rulebook$product[rulebook$product$si_type == target_type]
  if (!nrow(cfg))
    stop("unknown si_type: ", target_type, "\nthe `product` sheet carries: ",
         paste(rulebook$product$si_type, collapse = ", "))
  if (nrow(cfg) != 1L)
    stop("`product` sheet has ", nrow(cfg), " rows for si_type ", target_type,
         "; exactly one is required (a duplicate would vectorise every window).")

  # which coverages a product sells is a property of the rule sheet, not a
  # constant -- 3105 carries no care rows at all -- so read it off the sheet.
  coverages <- sort(unique(rulebook$ruleset[rulebook$ruleset$si_type == target_type,
                                            coverage]))
  if (!length(coverages))
    stop("`ruleset` carries no rows for si_type ", target_type, "; it covers: ",
         paste(sort(unique(rulebook$ruleset$si_type)), collapse = ", "))

  # care gets its own critical-disease window where the product defines one;
  # every other coverage uses the product's critical_disease_mon.
  critical_disease_window <- function(coverage) {
    ifelse(coverage == "care" & !is.na(cfg$care_mon), cfg$care_mon, cfg$critical_disease_mon)
  }

  list(si_type                     = target_type,
       medical_advice_mon          = cfg$medical_advice_mon,
       medical_advice_lookback_mon = cfg$medical_advice_lookback_mon,
       inpatient_surgery_mon       = cfg$inpatient_surgery_mon,
       critical_disease_mon        = cfg$critical_disease_mon,
       critical_disease_set        = cfg$critical_disease_set,
       coverages                   = coverages,
       critical_disease_window     = critical_disease_window,
       critical_disease            = rulebook$critical_disease[
                                       rulebook$critical_disease$critical_disease_set ==
                                       cfg$critical_disease_set])
}
