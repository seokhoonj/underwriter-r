#' Map diagnosis codes to their representative disease and scope flags
#'
#' Joins each `kcd` to `disease_table` to attach the representative disease
#' (`kcd_main`), the sub-diagnosis review flag (`sub_chk`), and the lookback
#' window in months (`lookback_mon`): exact match first, then a 3-character
#' fallback, then `"ZZZ"` for the still-unmapped (kept, never dropped, so they
#' can be referred to the underwriter).
#'
#' The two reserved codes [clean_icis()] writes are not diagnoses and are not
#' looked up: `"AAA"` (nothing to underwrite) and `"ZZZ"` (unreadable) carry
#' through as their own `kcd_main`, whatever `disease_table` holds. `"AAA"` is
#' always in its lookback window, since nothing was diagnosed that could age out
#' of one. So the guarantee that an insured never disappears lives in this code,
#' not in a row someone has to remember to write.
#'
#' Adds:
#' \describe{
#'   \item{`review`}{`1` when the row should be reviewed: the main diagnosis
#'     always, a sub-diagnosis only when its code is flagged `sub_chk == 1`.}
#'   \item{`in_lookback`}{`1` when the most recent treatment date is within the
#'     disease's per-disease window (`lookback_mon` months). Always `1` for
#'     `"AAA"` (no diagnosis, so nothing to age out) and `NA` for `"ZZZ"` (no
#'     lookback defined). Scopes the elapsed-days aggregation.}
#'   \item{`in_5yr`}{`1` when within a fixed 60-month window. Universal, so it is
#'     defined for every row (a >5-year treatment is `0`, not `NA`), including
#'     `"ZZZ"`. Scopes the counts.}
#' }
#'
#' @param melted A melted table from [melt_kcd()].
#' @param disease_table A lookup table with columns `kcd`, `kcd_main`, `sub_chk`,
#'   `lookback_mon`. It needs no rows for the reserved codes `"AAA"` and `"ZZZ"`;
#'   any it does carry are ignored.
#' @return `melted` with `kcd_main`, `sub_chk`, `lookback_mon`, `review`,
#'   `in_lookback`, `in_5yr` added.
#' @export
map_disease <- function(melted, disease_table) {
  melted <- as.data.table(copy(melted))
  disease_table <- as.data.table(disease_table)
  melted[disease_table, on = .(kcd),
       `:=`(kcd_main = i.kcd_main, sub_chk = i.sub_chk, lookback_mon = i.lookback_mon)]
  unmapped <- melted[is.na(kcd_main), which = TRUE]
  if (length(unmapped)) {
    fallback <- disease_table[.(substr(melted$kcd[unmapped], 1L, 3L)), on = .(kcd),
                             .(kcd_main, sub_chk, lookback_mon), mult = "first"]   # one row per key
    melted[unmapped, `:=`(kcd_main = fallback$kcd_main, sub_chk = fallback$sub_chk,
                        lookback_mon = fallback$lookback_mon)]
  }
  melted[is.na(kcd_main), `:=`(kcd_main = .KCD_UNMAPPED, sub_chk = 1L)]   # lookback_mon stays NA

  # The two reserved codes are not diagnoses, so no disease table decides them: pin
  # them here rather than trust one to carry a row for each. A table missing the
  # no-diagnosis row would otherwise fail its exact match, fail the 3-character
  # fallback, and hand an insured with nothing to underwrite to the underwriter as an
  # unmapped diagnosis -- the one outcome the reserved code exists to prevent.
  melted[kcd %chin% c(.KCD_NO_DIAGNOSIS, .KCD_UNMAPPED), `:=`(kcd_main = kcd, sub_chk = 1L)]

  melted[, review := as.integer(sub_kcd == 0L | sub_chk == 1L)]
  melted[, tdate := pmax(acc_date, sdate, edate, na.rm = TRUE)]   # most recent treatment date
  melted[, in_lookback := as.integer(tdate >= .minus_months(inq_date, lookback_mon))]
  melted[, in_5yr      := as.integer(tdate >= .minus_months(inq_date, 60L))]
  # nothing was diagnosed, so nothing can age out of a window -- not even a claim line
  # that carried no date at all, whose `tdate` is NA and whose window test is NA with it
  melted[kcd_main == .KCD_NO_DIAGNOSIS, in_lookback := 1L]
  melted[, tdate := NULL]
  melted[]
}

# `date` minus `n` months, clamped to the target month's last day so that, e.g.,
# 03-31 minus 1 month is 02-29, not 03-02 (which raw POSIXlt month subtraction
# gives when the day overflows a shorter month). `n` may be `NA` (-> `NA`).
.minus_months <- function(date, n) {
  lt <- as.POSIXlt(date)
  day <- lt$mday
  lt$mday <- 1L                          # first of month: month shift can't overflow
  lt$mon  <- lt$mon - n
  first <- as.Date(lt)
  nxt <- as.POSIXlt(first); nxt$mon <- nxt$mon + 1L
  days_in_month <- as.integer(as.Date(nxt) - first)
  first + (pmin(day, days_in_month) - 1L)
}
