# Treatment-line primitives shared by the standard and simplified-issue paths.
#
# Both products read the same claim lines and must agree on what a treatment date
# is, which lines count as hospitalization / surgery / outpatient, and how those
# are counted. They differ only in the WINDOW each applies and in what they do
# with the counts. Keeping the primitives here means a change to the definition --
# say, whether a surgery line may also count as outpatient -- lands on both paths
# at once instead of silently diverging.
#
# These are internal: the products' own verbs (aggregate_disease, match_si_rule)
# are the public surface.

#' Tag claim lines with their treatment date, elapsed days, and treatment type
#'
#' Adds, by reference:
#'   `tdate`   the line's treatment date -- the latest of accident / admission /
#'             discharge.
#'   `elapsed` days from `tdate` to the inquiry, clamped at 0. A treatment dated
#'             after the inquiry (a long stay pushing `edate` past it) means
#'             "still current", i.e. 0 days -- never negative.
#'   `is_hos` / `is_sur` / `is_out`  treatment-type membership. NON-EXCLUSIVE for
#'             the first two: a line that is both a hospitalization and a surgery
#'             feeds both counts. `is_out` is the complement of the two, so an
#'             outpatient line is one with neither a stay nor a procedure.
#'
#' @param dt A mapped claim table (from [map_disease()]).
#' @return `dt`, modified in place and returned invisibly for chaining.
#' @keywords internal
.tag_treatment <- function(dt) {
  tdate <- acc_date <- sdate <- edate <- inq_date <- NULL  # data.table NSE
  hos_day <- sur_cnt <- NULL
  dt[, tdate   := pmax(acc_date, sdate, edate, na.rm = TRUE)]
  dt[, elapsed := pmax(0L, as.integer(inq_date - tdate))]
  dt[, is_hos  := hos_day > 0L]
  dt[, is_sur  := sur_cnt > 0L]
  dt[, is_out  := hos_day == 0L & sur_cnt == 0L]
  invisible(dt[])
}

#' Count hospital days, surgeries, and outpatient visits per group
#'
#' `hos_day` is the dedup-union of hospital calendar days ([instead::count_stay()]),
#' not the sum of the per-line figures: overlapping stays must not double-count.
#' Discharge is clamped to the inquiry date so a stay running past it contributes
#' only the days already served. `sur_cnt` and `out_cnt` count distinct accident
#' dates, so several lines from one episode count once.
#'
#' Groups with no line of a given type get `0`, not `NA` -- "treated zero times"
#' is a fact, and leaving it missing makes every downstream band comparison
#' propagate `NA`.
#'
#' @param dt Claim lines already tagged by [.tag_treatment()].
#' @return A `data.table` keyed on `id`, `kcd_main` with `hos_day`, `sur_cnt`,
#'   `out_cnt`.
#' @keywords internal
.count_treatment <- function(dt) {
  id <- kcd_main <- sdate <- edate <- inq_date <- acc_date <- NULL  # data.table NSE
  is_hos <- is_sur <- is_out <- hos_day <- sur_cnt <- out_cnt <- stay <- NULL
  # the hospitalization branch below counts by (id, kcd_main) explicitly, so the
  # grouping is fixed rather than a parameter no caller varies.
  by    <- c("id", "kcd_main")
  empty <- dt[0L, ..by]
  if (!nrow(dt))
    return(cbind(empty, hos_day = integer(), sur_cnt = integer(), out_cnt = integer()))

  hosp <- dt[is_hos == TRUE]
  if (nrow(hosp)) hosp[, edate := pmin(edate, inq_date)]
  hos <- if (nrow(hosp))
    instead::count_stay(hosp, id, kcd_main, sdate, edate)[, .(id, kcd_main, hos_day = stay)]
  else cbind(empty, hos_day = integer())
  sur <- dt[is_sur == TRUE, .(sur_cnt = uniqueN(acc_date)), by = by]
  out <- dt[is_out == TRUE, .(out_cnt = uniqueN(acc_date)), by = by]

  res <- Reduce(function(x, y) merge(x, y, by = by, all.x = TRUE),
                list(unique(dt[, ..by]), hos, sur, out))
  res[is.na(hos_day), hos_day := 0L]
  res[is.na(sur_cnt), sur_cnt := 0L]
  res[is.na(out_cnt), out_cnt := 0L]
  setkeyv(res, by)
  res[]
}

#' Keep the lines treated within a month window of the inquiry
#'
#' Month arithmetic via `.minus_months()`, not `window_mon * 30`: a day
#' approximation drifts up to five days at 60 months, which is enough to move a
#' line across a product boundary and make two products' results incomparable.
#'
#' @param dt Claim lines already tagged by [.tag_treatment()].
#' @param window_mon Window in months; `NA` selects nothing (a product that asks
#'   no question over this window).
#' @return The subset of `dt` inside the window.
#' @keywords internal
.within_months <- function(dt, window_mon) {
  tdate <- inq_date <- NULL  # data.table NSE
  # an NA window selects nothing; an empty input has nothing to select and must
  # short-circuit before .minus_months(), which errors on a zero-length date
  # (it happens when every claim line is a sentinel, so the questions get 0 rows).
  if (is.na(window_mon) || !nrow(dt)) return(dt[0L])
  dt[tdate >= .minus_months(inq_date, as.integer(window_mon))]
}
