#' Diagnose the data quality of an ICIS claim table
#'
#' Reports per-id consistency, row-level anomalies, and a value profile for a raw
#' or cleansed ICIS feed. Each anomaly is counted as affected rows AND affected
#' insured, with percentages. Prints a readable report (when `verbose`) and
#' invisibly returns the same numbers as a named list. Aggregates only -- no raw
#' rows are printed.
#'
#' Sections:
#' \describe{
#'   \item{`id_consistency`}{insured whose gender, age, or inquiry date vary
#'     across their rows (e.g. multiple ages under one inquiry).}
#'   \item{`date_chronology`}{rows violating
#'     `acc_date <= sdate <= edate <= pay_date <= inq_date`.}
#'   \item{`row_anomaly`}{hospitalization with no admission date, discharge with
#'     no admission date, date-span vs `hos_day` disagreement (by direction),
#'     `hos_day < hos_cnt`, duplicate rows.}
#'   \item{`hos_day_span`}{among rows with both dates present, how often the
#'     date-span (`edate - sdate + 1`) matches `hos_day` (percentages within that
#'     base).}
#'   \item{`recompute_bound`}{when one endpoint is missing a recompute is forced;
#'     how often each anchor (sdate-forward vs edate-backward) produces an
#'     impossible date, by span direction.}
#'   \item{`gender_dist`, `age_stat`, `missing_required`}{value profile.}
#'   \item{`no_diagnosis`}{rows with every `kcd` empty (which `clean_icis()`
#'     drops) -- each still a real claim -- split by treatment: inpatient/surgery
#'     (`hos_day`/`sur_cnt`/`hos_cnt` > 0), outpatient (a visit date only), or
#'     truly empty (no treatment and no date). Plus the count of insured with no
#'     coded row at all.}
#' }
#'
#' @param dt An ICIS claim table (`data.table` or coercible).
#' @param verbose If `TRUE` (default) print a report; the list is always returned
#'   invisibly.
#' @return Invisibly, a named list with `n_row`, `n_id`, and the sections above.
#' @export
diagnose_icis <- function(dt, verbose = TRUE) {
  dt <- as.data.table(dt)
  n_row <- nrow(dt); n_id <- uniqueN(dt$id)
  id <- dt$id
  .has_col <- function(col) col %in% names(dt)
  # rows + affected ids (+ percentages) for a row mask
  .count_mask <- function(mask) {
    mask[is.na(mask)] <- FALSE
    nr <- sum(mask); ni <- uniqueN(id[mask])
    c(n_row = nr, n_id = ni,
      pct_row = round(100 * nr / n_row, 3), pct_id = round(100 * ni / n_id, 3))
  }
  # like .count_mask but percentages are relative to a base subset (e.g. "within
  # the span>hos_day rows"), not the whole table -- for conditional rates.
  .count_within <- function(mask, base) {
    mask[is.na(mask)] <- FALSE; base[is.na(base)] <- FALSE
    hit <- mask & base; nr <- sum(hit); ni <- uniqueN(id[hit])
    base_nr <- max(sum(base), 1L); base_ni <- max(uniqueN(id[base]), 1L)
    c(n_row = nr, n_id = ni,
      pct_row = round(100 * nr / base_nr, 3), pct_id = round(100 * ni / base_ni, 3))
  }

  # parse each date column once (present ones only) and the hospital-stay span
  date_of  <- function(col) if (.has_col(col)) .parse_date(dt[[col]]) else NULL
  acc_date <- date_of("acc_date")
  sdate    <- date_of("sdate")
  edate    <- date_of("edate")
  pay_date <- date_of("pay_date")
  inq_date <- date_of("inq_date")
  span     <- if (!is.null(sdate) && !is.null(edate)) as.integer(edate - sdate + 1L) else NULL

  # --- id_consistency -------------------------------------------------------
  per_id <- dt[, .(
    n_gender = if (.has_col("gender")) uniqueN(gender) else NA_integer_,
    n_age    = if (.has_col("age"))    uniqueN(age)    else NA_integer_,
    n_inq    = if (.has_col("inq_date")) uniqueN(inq_date) else NA_integer_,
    age_span = if (.has_col("age")) max(age, na.rm = TRUE) - min(age, na.rm = TRUE) else NA_real_
  ), by = id]

  id_consistency <- list(
    rows_per_id_median   = as.numeric(dt[, .N, by = id][, median(N)]),
    multi_gender_ids     = sum(per_id$n_gender > 1, na.rm = TRUE),
    gender_kinds_max     = max(per_id$n_gender, na.rm = TRUE),
    multi_age_ids        = sum(per_id$n_age > 1, na.rm = TRUE),
    age_kinds_max        = max(per_id$n_age, na.rm = TRUE),
    age_kinds_dist       = table(per_id$n_age),
    age_span_max         = max(per_id$age_span, na.rm = TRUE),
    age_span_ge2_ids     = sum(per_id$age_span >= 2, na.rm = TRUE),
    multi_inq_ids        = sum(per_id$n_inq > 1, na.rm = TRUE),
    inq_kinds_max        = max(per_id$n_inq, na.rm = TRUE),
    multi_age_single_inq = sum(per_id$n_age > 1 & per_id$n_inq == 1, na.rm = TRUE)
  )

  # --- date_chronology: acc_date <= sdate <= edate <= pay_date <= inq_date ---
  date_chronology <- NULL
  if (!is.null(acc_date) && !is.null(sdate) && !is.null(edate) && !is.null(pay_date) && !is.null(inq_date)) {
    chain  <- list(acc_date, sdate, edate, pay_date, inq_date)
    labels <- c("acc_date", "sdate", "edate", "pay_date", "inq_date")
    date_chronology <- lapply(seq_len(4L), function(i) .count_mask(chain[[i]] > chain[[i + 1L]]))
    names(date_chronology) <- sprintf("%-8s > %s", labels[-5L], labels[-1L])
  }

  # --- row_anomaly ----------------------------------------------------------
  # sdate present with hos_day==0 is NOT an anomaly -- it is an outpatient visit.
  # hos_cnt is unreliable (hos_day < hos_cnt is impossible); the pipeline ignores
  # it, this is monitoring only. raw span vs hos_day is meaningful on the RAW feed
  # (cleansed edate is derived from hos_day, so both directions are 0).
  span_over <- span_under <- NULL
  if (!is.null(span) && .has_col("hos_day")) {
    is_hosp <- dt$hos_day > 0 & !is.na(span)
    span_over  <- .count_mask(is_hosp & span > dt$hos_day)
    span_under <- .count_mask(is_hosp & span < dt$hos_day)
  }
  row_anomaly <- list(
    hos_day_no_sdate   = if (.has_col("hos_day") && .has_col("sdate")) .count_mask(dt$hos_day > 0 & !.is_present(dt$sdate)) else NULL,
    edate_no_sdate     = if (.has_col("sdate") && .has_col("edate")) .count_mask(!.is_present(dt$sdate) & .is_present(dt$edate)) else NULL,
    span_gt_hos_day    = span_over,
    span_lt_hos_day    = span_under,
    hos_day_lt_hos_cnt = if (.has_col("hos_day") && .has_col("hos_cnt")) .count_mask(dt$hos_day < dt$hos_cnt) else NULL,
    duplicate          = .count_mask(duplicated(dt))
  )

  # --- hos_day vs date-span consistency (within the both-present base) -------
  hos_day_span <- NULL
  if (!is.null(span) && .has_col("hos_day")) {
    both_present <- .is_present(dt$sdate) & .is_present(dt$edate) & dt$hos_day > 0 & !is.na(span)
    hos_day_span <- list(
      n_base  = .count_mask(both_present),   # base size as % of the whole table
      match   = .count_within(span == dt$hos_day, both_present),
      span_gt = .count_within(span >  dt$hos_day, both_present),
      span_lt = .count_within(span <  dt$hos_day, both_present)
    )
  }

  # --- recompute vs bound (which missing-endpoint anchor is safer) ----------
  # sdate-anchored (forward)  edate' = sdate + hos_day - 1  -> edate' > pay_date
  # edate-anchored (backward) sdate' = edate - hos_day + 1  -> sdate' < acc_date
  recompute_bound <- NULL
  if (!is.null(span) && !is.null(pay_date) && !is.null(acc_date) && .has_col("hos_day")) {
    hos_day   <- dt$hos_day
    fwd_edate <- sdate + hos_day - 1L
    bwd_sdate <- edate - hos_day + 1L
    over  <- hos_day > 0 & !is.na(span) & span > hos_day
    under <- hos_day > 0 & !is.na(span) & span < hos_day
    recompute_bound <- list(
      span_gt = list(n_base           = .count_mask(over),
                     fwd_edate_gt_pay = .count_within(!is.na(pay_date) & fwd_edate > pay_date, over),
                     bwd_sdate_lt_acc = .count_within(!is.na(acc_date) & bwd_sdate < acc_date, over)),
      span_lt = list(n_base           = .count_mask(under),
                     fwd_edate_gt_pay = .count_within(!is.na(pay_date) & fwd_edate > pay_date, under),
                     bwd_sdate_lt_acc = .count_within(!is.na(acc_date) & bwd_sdate < acc_date, under))
    )
  }

  # --- value profile --------------------------------------------------------
  gender_dist <- if (.has_col("gender")) table(dt$gender, useNA = "ifany") else NULL
  age_stat    <- if (.has_col("age")) list(min = min(dt$age, na.rm = TRUE), max = max(dt$age, na.rm = TRUE),
                                           n_zero = sum(dt$age == 0, na.rm = TRUE), n_na = sum(is.na(dt$age))) else NULL
  required <- intersect(c("id", "gender", "age", "inq_date", "acc_date", "pay_date", "kcd0"), names(dt))
  missing_required <- vapply(dt[, .SD, .SDcols = required],
                             function(v) sum(is.na(v) | (is.character(v) & !nzchar(trimws(v)))), numeric(1))

  # --- no-diagnosis rows (every kcd empty; clean_icis drops these) ----------
  # every such row is still a real claim, split by the treatment it carries:
  # inpatient/surgery (hos_day/sur_cnt/hos_cnt > 0), outpatient (no such value but
  # a visit date), or truly empty (no treatment and no date). Only the last is
  # safe to auto-pass; the first two are real claims missing a diagnosis.
  no_diagnosis <- NULL
  kcd_cols <- intersect(paste0("kcd", 0:4), names(dt))
  if (length(kcd_cols)) {
    no_code <- Reduce(`&`, lapply(kcd_cols, function(col) !.is_present(dt[[col]])))
    tx_cols <- intersect(c("hos_day", "sur_cnt", "hos_cnt"), names(dt))
    has_tx    <- if (length(tx_cols))
      Reduce(`|`, lapply(tx_cols, function(col) !is.na(dt[[col]]) & dt[[col]] > 0)) else logical(n_row)
    has_visit <- if (.has_col("acc_date")) .is_present(dt$acc_date) else logical(n_row)
    no_diagnosis <- list(
      all_empty     = .count_mask(no_code),
      inpatient     = .count_within(has_tx, no_code),
      outpatient    = .count_within(!has_tx & has_visit, no_code),
      empty         = .count_within(!has_tx & !has_visit, no_code),
      all_empty_ids = n_id - uniqueN(id[!no_code])   # insured with no coded row at all
    )
  }

  out <- list(
    n_row            = n_row,
    n_id             = n_id,
    id_consistency   = id_consistency,
    date_chronology  = date_chronology,
    row_anomaly      = row_anomaly,
    hos_day_span     = hos_day_span,
    recompute_bound  = recompute_bound,
    gender_dist      = gender_dist,
    age_stat         = age_stat,
    missing_required = missing_required,
    no_diagnosis     = no_diagnosis
  )

  if (verbose) .print_diagnose(out)
  invisible(out)
}

# Print the diagnose_icis report. Every line is "  <label pad to 26> : <value>",
# so the ":" and the numbers line up across all sections; small distributions are
# formatted inline rather than dumped as raw tables.
.print_diagnose <- function(out) {
  .comma <- function(x) format(x, big.mark = ",")
  .dist  <- function(tb) paste(sprintf("%s=%s", names(tb),
                               format(as.integer(tb), big.mark = ",", trim = TRUE)), collapse = " | ")
  .line  <- function(label, value) cat(sprintf("  %-28s : %s\n", label, value))
  .header <- function(title) cat(sprintf("\n== %s ==\n", title))
  # "lhs op rhs" with lhs/op padded so the operator and rhs line up across rows
  .cmp   <- function(lhs, op, rhs) sprintf("%-9s %-2s %s", lhs, op, rhs)
  # a fixed-width "count (pct%)" cell so every column lines up; a missing pct (for
  # a plain count row like "base rows") leaves the percentage slot blank.
  .cell <- function(count, pct = NA) {
    num <- sprintf("%9s", .comma(count))
    pct <- if (is.na(pct)) strrep(" ", 9L) else sprintf("(%6.2f%%)", pct)
    paste(num, pct)
  }
  .counts <- function(label, v) .line(label, sprintf("n_row %s | n_id %s",
                                       .cell(v[["n_row"]], v[["pct_row"]]), .cell(v[["n_id"]], v[["pct_id"]])))
  # an insured-count line: count right-aligned + its share of all insured
  .icount <- function(label, count, note = "") {
    v <- sprintf("%7s (%6.2f%%)", .comma(count), 100 * count / out$n_id)
    .line(label, if (nzchar(note)) paste0(v, "  ", note) else v)
  }

  ic <- out$id_consistency
  cat(sprintf("n_row=%s | n_id=%s | rows/id median=%s\n", .comma(out$n_row), .comma(out$n_id), ic$rows_per_id_median))

  .header("id_consistency (insured counts, % of all)")
  .icount("multi-gender ids", ic$multi_gender_ids)
  .icount("multi-age ids", ic$multi_age_ids, sprintf("age-span >=2yr: %s", .comma(ic$age_span_ge2_ids)))
  .icount("multi-inq ids", ic$multi_inq_ids)
  .icount("multi-age under one inq", ic$multi_age_single_inq)

  .header("date_chronology (acc <= sdate <= edate <= pay <= inq)")
  if (!is.null(out$date_chronology)) for (nm in names(out$date_chronology)) .counts(nm, out$date_chronology[[nm]])

  ra <- out$row_anomaly
  .header("row_anomaly")
  if (!is.null(ra$hos_day_no_sdate))   .counts(.cmp("hos_day", ">", "0 & no sdate"), ra$hos_day_no_sdate)
  if (!is.null(ra$edate_no_sdate))     .counts("edate but no sdate", ra$edate_no_sdate)
  if (!is.null(ra$span_gt_hos_day))    .counts(.cmp("date-span", ">", "hos_day"), ra$span_gt_hos_day)
  if (!is.null(ra$span_lt_hos_day))    .counts(.cmp("date-span", "<", "hos_day"), ra$span_lt_hos_day)
  if (!is.null(ra$hos_day_lt_hos_cnt)) .counts(.cmp("hos_day", "<", "hos_cnt"), ra$hos_day_lt_hos_cnt)
  .counts("duplicate rows", ra$duplicate)

  if (!is.null(out$hos_day_span)) {
    hs <- out$hos_day_span
    .header("hos_day vs date-span (date-span = edate-sdate+1; both present; % within base)")
    .counts("base rows", hs$n_base)   # base size as % of the whole table
    .counts(.cmp("date-span", "==", "hos_day (match)"), hs$match)
    .counts(.cmp("date-span", ">", "hos_day"), hs$span_gt)
    .counts(.cmp("date-span", "<", "hos_day"), hs$span_lt)
  }
  if (!is.null(out$recompute_bound)) {
    rb <- out$recompute_bound
    .header("recompute vs bound (direction row = % of all; indented = % within direction)")
    .counts("date-span > hos_day", rb$span_gt$n_base)
    .counts("  sdate-basis edate>pay", rb$span_gt$fwd_edate_gt_pay)
    .counts("  edate-basis sdate<acc", rb$span_gt$bwd_sdate_lt_acc)
    .counts("date-span < hos_day", rb$span_lt$n_base)
    .counts("  sdate-basis edate>pay", rb$span_lt$fwd_edate_gt_pay)
    .counts("  edate-basis sdate<acc", rb$span_lt$bwd_sdate_lt_acc)
  }

  .header("value profile")
  if (!is.null(out$gender_dist)) .line("gender", .dist(out$gender_dist))
  if (!is.null(out$age_stat)) .line("age", sprintf("range %s-%s | zero %s | missing %s",
        out$age_stat$min, out$age_stat$max, .comma(out$age_stat$n_zero), .comma(out$age_stat$n_na)))
  miss <- out$missing_required[out$missing_required > 0]
  .line("missing/empty (required)", if (length(miss)) .dist(miss) else "none")

  if (!is.null(out$no_diagnosis)) {
    nd <- out$no_diagnosis
    .header("no diagnosis (all kcd empty; clean_icis drops these rows)")
    .counts("all kcd empty", nd$all_empty)
    .counts("  inpatient/surgery", nd$inpatient)
    .counts("  outpatient (visit only)", nd$outpatient)
    .counts("  empty (no treatment/date)", nd$empty)
    .icount("ids with no coded row", nd$all_empty_ids, "-> dropped entirely")
  }
}

# --- low-level helpers ------------------------------------------------------

# Parse a column to Date whether it is already Date or a character string in ISO
# ("2024-01-05") or compact ("20240105") form. A single %Y%m%d assumption
# silently turns ISO strings into NA, so try both formats.
.parse_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  x <- as.character(x)
  d <- as.Date(x, format = "%Y-%m-%d")
  todo <- is.na(d) & !is.na(x) & nzchar(trimws(x))
  d[todo] <- as.Date(x[todo], format = "%Y%m%d")
  d
}

# TRUE where a value is present (non-NA and, for character, non-empty).
.is_present <- function(x) if (is.character(x)) !is.na(x) & nzchar(trimws(x)) else !is.na(x)
