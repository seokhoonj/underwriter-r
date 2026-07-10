#' Aggregate mapped diagnoses to per-insured underwriting inputs
#'
#' Collapses the long, mapped table to one row per `(id, kcd_main)` with the
#' inputs a rule set bands on. Reviewed rows only (`review == 1`). Counts use a
#' fixed 5-year window; elapsed days use each disease's lookback window (with the
#' 5-year window as the fallback for unmapped `"ZZZ"` codes, which are kept so
#' they can be referred to the underwriter).
#'
#' Elapsed days are computed per treatment type -- `hos_elp_day` (hospitalization),
#' `sur_elp_day` (surgery), `out_elp_day` (outpatient) -- because products combine
#' them differently (general insurance uses the minimum of all three, which is
#' returned as `elp_day`; simplified-issue questions use other subsets). A line
#' that is both a hospitalization and a surgery feeds both. `hos_day` is the
#' dedup-union of hospital calendar days ([instead::count_stay()]); `sur_cnt` and
#' `out_cnt` count distinct accident dates.
#'
#' Every `id` in `mapped` gets at least one row. An insured whose every diagnosis
#' fell outside its lookback window has nothing in scope to underwrite, and is
#' carried on the no-diagnosis code `"AAA"` with zero counts and the days since
#' their most recent treatment.
#'
#' @param mapped A long, mapped table from [map_disease()].
#' @return A `data.table`, one row per `(id, kcd_main)`, with `age`, `hos_day`,
#'   `sur_cnt`, `out_cnt`, `hos_elp_day`, `sur_elp_day`, `out_elp_day`, `elp_day`.
#' @export
aggregate_disease <- function(mapped) {
  reviewed <- as.data.table(mapped)[review == 1L]
  # elapsed days since the most recent treatment. clamp at 0: a treatment dated
  # after the inquiry (edate pushed past inquiry by a large hos_day = still
  # hospitalized) means "current", i.e. 0 days elapsed -- never negative.
  reviewed[, elapsed := pmax(0L, as.integer(inq_date - pmax(acc_date, sdate, edate, na.rm = TRUE)))]

  # (id, kcd_main) universe: within the disease lookback window; for ZZZ (no
  # lookback) fall back to the 5-year window.
  in_scope <- reviewed[in_lookback == 1L | (kcd_main == .KCD_UNMAPPED & in_5yr == 1L)]

  # a codeless claim line marks the line, not the insured: someone with any real
  # diagnosis in scope has something to underwrite, so their codeless lines add
  # nothing. Keep the no-diagnosis code only where it is the whole story.
  underwritable <- unique(in_scope[kcd_main != .KCD_NO_DIAGNOSIS, id])
  in_scope      <- in_scope[kcd_main != .KCD_NO_DIAGNOSIS | !id %in% underwritable]
  id_disease    <- unique(in_scope[, .(id, kcd_main)])

  # per-treatment-type elapsed days, each the most recent (min) within scope
  hos_elp_day <- in_scope[hos_day > 0,                 .(hos_elp_day = min(elapsed)), by = .(id, kcd_main)]
  sur_elp_day <- in_scope[sur_cnt > 0,                 .(sur_elp_day = min(elapsed)), by = .(id, kcd_main)]
  out_elp_day <- in_scope[hos_day == 0 & sur_cnt == 0, .(out_elp_day = min(elapsed)), by = .(id, kcd_main)]

  # counts over the fixed 5-year window
  within_5yr <- reviewed[in_5yr == 1L]
  hosp       <- within_5yr[hos_day > 0]
  hosp[, edate := pmin(edate, inq_date)]   # don't count hospital days after inquiry
  hospital_days <- if (nrow(hosp))
    instead::count_stay(hosp, id, kcd_main, sdate, edate)[, .(id, kcd_main, hos_day = stay)]
  else data.table(id = character(), kcd_main = character(), hos_day = integer())
  surgery_count    <- within_5yr[sur_cnt > 0,                 .(sur_cnt = uniqueN(acc_date)), by = .(id, kcd_main)]
  outpatient_count <- within_5yr[hos_day == 0 & sur_cnt == 0, .(out_cnt = uniqueN(acc_date)), by = .(id, kcd_main)]

  result <- Reduce(function(x, y) merge(x, y, by = c("id", "kcd_main"), all.x = TRUE),
                   list(id_disease, hospital_days, surgery_count, outpatient_count,
                        hos_elp_day, sur_elp_day, out_elp_day))
  result[is.na(hos_day), hos_day := 0L]
  result[is.na(sur_cnt), sur_cnt := 0L]
  result[is.na(out_cnt), out_cnt := 0L]
  # general-insurance elapsed = days since the most recent treatment of any type
  result[, elp_day := pmin(hos_elp_day, sur_elp_day, out_elp_day, na.rm = TRUE)]
  # age (per insured) travels with each disease row -- the rule bands on it
  ages <- reviewed[, .(age = max(as.integer(age))), by = id]
  result[ages, on = .(id), age := i.age]

  # an insured whose every diagnosis fell outside its lookback window has nothing
  # in scope to underwrite, so they would otherwise leave no row here at all.
  # Keep them on the no-diagnosis code, carrying the days since their most recent
  # treatment (a real elapsed count, unlike a fabricated 0, which would read as
  # "still under treatment"), so the id survives to the final decision.
  no_scope <- reviewed[!id %in% in_scope$id, .(elp_day = min(elapsed)), by = id]
  if (nrow(no_scope)) {
    no_scope[ages, on = .(id), age := i.age]
    result <- rbind(result, no_scope[, .(id, kcd_main = .KCD_NO_DIAGNOSIS, age,
                                         hos_day = 0L, sur_cnt = 0L, out_cnt = 0L,
                                         hos_elp_day = NA_integer_, sur_elp_day = NA_integer_,
                                         out_elp_day = NA_integer_, elp_day)],
                    use.names = TRUE)
  }
  setcolorder(result, c("id", "kcd_main", "age", "hos_day", "sur_cnt", "out_cnt",
                        "hos_elp_day", "sur_elp_day", "out_elp_day", "elp_day"))
  result[]
}
