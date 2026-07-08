#' Map diagnosis codes to their representative disease and scope flags
#'
#' Joins each `kcd` to `disease_table` to attach the representative disease
#' (`kcd_main`), the sub-diagnosis review flag (`sub_chk`), and the lookback
#' window in months (`lookback_mon`): exact match first, then a 3-character
#' fallback, then `"ZZZ"` for the still-unmapped (kept, never dropped, so they
#' can route to manual review). Adds:
#' \describe{
#'   \item{`review`}{`1` when the row should be reviewed: the main diagnosis
#'     always, a sub-diagnosis only when its code is flagged `sub_chk == 1`.}
#'   \item{`in_lookback`}{`1` when the most recent treatment date is within the
#'     disease's per-disease window (`lookback_mon` months). `NA` for `"ZZZ"`
#'     (no lookback defined). Scopes the elapsed-days aggregation.}
#'   \item{`in_5yr`}{`1` when within a fixed 60-month window. Universal, so it is
#'     defined for every row (a >5-year treatment is `0`, not `NA`), including
#'     `"ZZZ"`. Scopes the counts.}
#' }
#'
#' @param long A long table from [melt_kcd()].
#' @param disease_table A lookup table with columns `kcd`, `kcd_main`, `sub_chk`,
#'   `lookback_mon`.
#' @return `long` with `kcd_main`, `sub_chk`, `lookback_mon`, `review`,
#'   `in_lookback`, `in_5yr` added.
#' @export
map_disease <- function(long, disease_table) {
  long <- as.data.table(copy(long))
  disease_table <- as.data.table(disease_table)
  long[disease_table, on = .(kcd),
       `:=`(kcd_main = i.kcd_main, sub_chk = i.sub_chk, lookback_mon = i.lookback_mon)]
  unmapped <- long[is.na(kcd_main), which = TRUE]
  if (length(unmapped)) {
    fallback <- disease_table[.(substr(long$kcd[unmapped], 1L, 3L)), on = .(kcd),
                             .(kcd_main, sub_chk, lookback_mon)]
    long[unmapped, `:=`(kcd_main = fallback$kcd_main, sub_chk = fallback$sub_chk,
                        lookback_mon = fallback$lookback_mon)]
  }
  long[is.na(kcd_main), `:=`(kcd_main = "ZZZ", sub_chk = 1L)]   # lookback_mon stays NA for ZZZ
  long[, review := as.integer(sub_kcd == 0L | sub_chk == 1L)]
  long[, tdate := pmax(acc_date, sdate, edate, na.rm = TRUE)]   # most recent treatment date
  long[, in_lookback := {
    cutoff <- as.POSIXlt(inq_date); cutoff$mon <- cutoff$mon - lookback_mon
    as.integer(tdate >= as.Date(cutoff))
  }]
  long[, in_5yr := {
    cutoff <- as.POSIXlt(inq_date); cutoff$mon <- cutoff$mon - 60L
    as.integer(tdate >= as.Date(cutoff))
  }]
  long[, tdate := NULL]
  long[]
}
