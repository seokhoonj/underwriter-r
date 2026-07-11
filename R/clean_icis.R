#' Cleanse a raw ICIS claim table into a wide master
#'
#' One row per claim line: parse the dates, reconcile the admission/discharge
#' window through `hos_day`, normalize the diagnosis codes, pull them leftward
#' (so `kcd0` is the main diagnosis), and drop exact duplicate rows.
#'
#' No row is dropped for lacking a diagnosis code. A line that arrived without
#' one gets `kcd0 = "VACANT"` (nothing to underwrite); a line whose every code cell
#' failed to parse gets `kcd0 = "IRREGULAR"` (unreadable, routes to review). Both
#' are ordinary codes the disease table and rule set decide, so an insured is never
#' lost between the raw feed and the final decision.
#'
#' `method` picks which admission/discharge endpoint is trusted and which is
#' derived from `hos_day`:
#' \describe{
#'   \item{`"sdate"`}{trust admission; discharge = `sdate + hos_day - 1`.}
#'   \item{`"edate"`}{trust discharge; admission = `edate - hos_day + 1`.}
#'   \item{`"auto"`}{per row prefer the sdate basis, but if its derived discharge
#'     would fall after pay/inquiry (an ordering violation) switch to the edate
#'     basis, and if that derived admission would fall before the accident date
#'     fall back to the sdate basis.}
#' }
#' With only one raw endpoint the anchor is forced to it. `hos_day == 0` is an
#' outpatient visit (no stay): `sdate` is its visit date and is kept; `edate`
#' stays `NA`. A large `hos_day` can push the derived endpoint past the inquiry
#' date (still hospitalized); that is left visible and handled at aggregation.
#'
#' @param dt A raw ICIS claim table (`data.table` or coercible). Expected columns
#'   include `id`, `gender`, `age`, `inq_date`, `pay_date`, `acc_date`, `sdate`,
#'   `edate`, `hos_day`, `hos_cnt`, `sur_cnt`, and the diagnosis columns; dates
#'   are `YYYYMMDD` strings.
#' @param kcd_cols Character vector of diagnosis-code column names
#'   (default `kcd0..kcd4`).
#' @param method Endpoint-reconciliation basis; one of `"sdate"` (default),
#'   `"edate"`, `"auto"`.
#' @return A cleansed wide `data.table`, one row per (deduplicated) claim line.
#' @seealso [filter_latest_inquiry()], [melt_kcd()].
#' @export
clean_icis <- function(dt, kcd_cols = paste0("kcd", 0:4),
                       method = c("sdate", "edate", "auto")) {
  method <- match.arg(method)
  dt <- as.data.table(copy(dt))

  # 0. force the code columns to character. A column that arrived all-NA is logical,
  #    and `set()` coerces its RHS to the column type -- so `.redistribute_multi()`
  #    writing a code into such a column would silently turn it back to NA, losing the
  #    diagnosis. Fixing the type up front also settles pack_left()'s type check.
  dt[, (kcd_cols) := lapply(.SD, as.character), .SDcols = kcd_cols]

  # 1. note which rows arrived with a diagnosis code. No row is dropped for it:
  #    step 4 marks the codeless rows instead, so an insured with no diagnosis
  #    stays in the feed rather than vanishing and being re-added downstream.
  #    `has_code` is POSITIONAL -- it is used at step 4 by row index -- so nothing
  #    between here and there may reorder `dt`; every step in between edits in place.
  has_code <- Reduce(`|`, lapply(kcd_cols, function(col) {
    v <- dt[[col]]
    !is.na(v) & nzchar(trimws(v))
  }))

  # 2. parse dates, set gender, reconcile the admission/discharge window
  .reconcile_window(dt, method)

  # 3. redistribute the rare multi-code cells, then normalize every code column
  is_multi <- Reduce(`|`, lapply(kcd_cols, function(col) {
    v <- dt[[col]]
    !is.na(v) & grepl(",", v, fixed = TRUE)
  }))
  if (any(is_multi)) .redistribute_multi(dt, which(is_multi), kcd_cols)
  dt[, (kcd_cols) := lapply(.SD, normalize_kcd), .SDcols = kcd_cols]

  # 4. pack codes leftward (kcd0 = main), then mark the rows that ended up with
  #    no usable code, by why: nothing was recorded (VACANT -- no diagnosis to
  #    underwrite) or a code was recorded but no cell parsed to the KCD shape
  #    (IRREGULAR -- unreadable, so it routes to review). Then drop exact duplicate
  #    rows (~28% of the feed are repeated transmission lines; deduping on the final
  #    cleaned columns also collapses rows made identical by the reconciliation).
  instead::pack_left(dt, kcd_cols)
  no_kcd <- Reduce(`&`, lapply(kcd_cols, function(col) is.na(dt[[col]])))
  set(dt, which(no_kcd & !has_code), "kcd0", .KCD_VACANT)     # cells were empty
  set(dt, which(no_kcd &  has_code), "kcd0", .KCD_IRREGULAR)  # written but no cell parsed
  dt <- unique(dt)

  ord <- c("id", "gender", "age", "inq_date", "pay_date", "acc_date",
           "sdate", "edate", "hos_day", "hos_cnt", "sur_cnt", kcd_cols)
  setcolorder(dt, intersect(ord, names(dt)))
  dt[]
}

#' Keep only each id's most recent inquiry
#'
#' A customer re-quoted several times appears under several `inq_date`s -- each
#' application triggers a fresh inquiry that returns the claim history as of that
#' date, so the latest inquiry carries the most complete, current history. All
#' rows sharing an id's maximum `inq_date` are kept (one inquiry spans many claim
#' rows) and every id is preserved. Apply after [clean_icis()] and before
#' aggregation so a multi-quote customer is evaluated once, on their latest
#' inquiry, not double-counted across older inquiries.
#'
#' A missing `inq_date` is ignored when choosing the latest: `max(..., na.rm)`
#' takes the newest real inquiry, so a stray undated row does not win. An id whose
#' every `inq_date` is `NA` has no inquiry to pick, so all its rows are kept -- the
#' insured is never dropped for a missing date.
#'
#' @param dt A cleansed claim table with `id` and `inq_date` columns.
#' @return The subset of `dt` restricted to each id's latest `inq_date` (all rows
#'   for an id whose dates are all `NA`).
#' @export
filter_latest_inquiry <- function(dt) {
  dt <- as.data.table(dt)
  latest <- dt[, .(inq_date = suppressWarnings(max(inq_date, na.rm = TRUE))), by = id]
  latest <- latest[is.finite(inq_date)]           # drop ids with no real inquiry date
  kept <- dt[latest, on = .(id, inq_date)]
  no_date <- dt[!id %in% latest$id]               # every inq_date NA: keep the id whole
  if (nrow(no_date)) rbind(kept, no_date) else kept
}

# Parse dates, set gender, and reconcile the admission/discharge window in place.
.reconcile_window <- function(dt, method) {
  parse_ymd <- function(x) as.Date(x, format = "%Y%m%d")
  dt[, `:=`(
    inq_date = parse_ymd(inq_date),
    acc_date = parse_ymd(acc_date),
    pay_date = parse_ymd(pay_date),
    sdate    = parse_ymd(sdate),
    edate    = parse_ymd(edate)
  )]
  dt[, gender := factor(gender, levels = c("1", "2"))]

  # fifelse / Date subset assignment keep a proper Date vector; a scalar
  # `col := as.Date(NA)` on a character column would keep the character type.
  if (method == "sdate") {
    dt[is.na(sdate) & hos_day > 0 & !is.na(edate), sdate := edate - hos_day + 1L]
    dt[, edate := fifelse(hos_day > 0 & !is.na(sdate), sdate + hos_day - 1L, as.Date(NA_character_))]
  } else if (method == "edate") {
    dt[is.na(edate) & hos_day > 0 & !is.na(sdate), edate := sdate + hos_day - 1L]
    dt[hos_day == 0, edate := as.Date(NA_character_)]
    dt[hos_day > 0 & !is.na(edate), sdate := edate - hos_day + 1L]
  } else {
    .reconcile_auto(dt)
  }
  dt
}

# "auto" reconciliation: per row prefer the sdate basis, fall back per the rules
# in clean_icis(). Modifies sdate/edate in place.
.reconcile_auto <- function(dt) {
  raw_sdate <- dt$sdate
  raw_edate <- dt$edate
  hos_day   <- dt$hos_day
  discharge_upper  <- pmin(dt$pay_date, dt$inq_date, na.rm = TRUE)  # discharge can't exceed pay/inquiry
  admit_lower      <- dt$acc_date                                   # admission can't precede accident
  edate_from_sdate <- raw_sdate + hos_day - 1L                      # sdate basis: derived discharge
  sdate_from_edate <- raw_edate - hos_day + 1L                      # edate basis: derived admission

  is_hosp   <- hos_day > 0
  is_hosp[is.na(is_hosp)] <- FALSE          # NA hos_day: treat as no stay (outpatient)
  has_sdate <- !is.na(raw_sdate)
  has_edate <- !is.na(raw_edate)
  sdate_basis_ok <- has_sdate & (is.na(discharge_upper) | edate_from_sdate <= discharge_upper)
  edate_basis_ok <- has_edate & (is.na(admit_lower)     | sdate_from_edate >= admit_lower)

  # anchor on edate when sdate is absent, or the sdate basis violates order and
  # the edate basis does not; otherwise anchor on sdate (preferred / fallback).
  use_edate_basis <- is_hosp & has_edate & (!has_sdate | (!sdate_basis_ok & edate_basis_ok))
  use_edate_basis[is.na(use_edate_basis)] <- FALSE   # order-check NA -> keep the sdate basis
  use_sdate_basis <- is_hosp & has_sdate & !use_edate_basis

  final_sdate <- raw_sdate
  final_edate <- raw_edate
  final_sdate[use_edate_basis] <- sdate_from_edate[use_edate_basis]
  final_edate[use_sdate_basis] <- edate_from_sdate[use_sdate_basis]
  final_edate[!is_hosp] <- as.Date(NA)   # outpatient: no discharge
  dt[, `:=`(sdate = final_sdate, edate = final_edate)]
}

# Redistribute the rare multi-code cells (a cell holding several comma-separated
# codes) across kcd0..kcd{n-1}: all codes of the row, in order, first -> main.
# Loops row-by-row -- fine because the subset is tiny.
.redistribute_multi <- function(dt, rows, kcd_cols) {
  n_codes <- length(kcd_cols)
  for (i in rows) {
    codes <- unlist(lapply(kcd_cols, function(col) split_kcd(dt[[col]][i])))
    codes <- head(c(codes, rep(NA_character_, n_codes)), n_codes)
    for (j in seq_along(kcd_cols)) set(dt, i = i, j = kcd_cols[j], value = codes[j])
  }
  dt
}
