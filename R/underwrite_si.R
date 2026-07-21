#' Run the whole simplified-issue pipeline for one product
#'
#' Cleanses raw claims, maps diagnoses, matches the three questions and folds them
#' to one decision per insured and coverage -- the simplified-issue counterpart of
#' running the standard chain end to end. The front end (clean / filter / melt /
#' map) is shared with the standard path unchanged.
#'
#' @inheritParams match_si_rule
#' @param dt Raw ICIS claims.
#' @param disease_table The `disease` sheet from the standard rulebook. The two
#'   products share one diagnosis mapping, so it is not duplicated in the SI
#'   rulebook and is passed in here.
#' @param kcd_cols Diagnosis-code columns of `dt`.
#' @return The per-`(id, coverage)` decision table from [combine_si_decision()].
#' @seealso [match_si_rule()], [combine_si_decision()], [tabulate_si_decision()].
#' @export
underwrite_si <- function(dt, disease_table, rulebook, product,
                          source = c("icis", "declaration"),
                          kcd_cols = paste0("kcd", 0:4)) {
  cleaned <- filter_latest_inquiry(clean_icis(dt, kcd_cols))
  mapped  <- map_disease(melt_kcd(cleaned), disease_table)
  matched <- match_si_rule(mapped, rulebook, product, source)
  # ids come from the raw input, not from `mapped`: an insured whose every claim
  # line was dropped in cleansing must still appear in the result.
  combine_si_decision(matched, unique(as.data.table(dt)$id), rulebook, product)
}
