#' Aggregate mapped diagnoses to per-insured underwriting inputs
#'
#' Collapses the long, mapped table to one row per `(id, kcd_main)` with the
#' inputs a rule set bands on. Reviewed rows only (`review == 1`). Counts use a
#' fixed 5-year window; elapsed days use each disease's lookback window (with the
#' 5-year window as the fallback for the `"IRREGULAR"` and `"UNMAPPED"` codes, which
#' have no lookback of their own and are kept so they can be referred).
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
#' fell outside its lookback window, and who has no codeless (`"VACANT"`) line to
#' survive on, is carried on the `"EXPIRED"` code with zero counts and the days since
#' their most recent treatment, so the id survives to the final decision.
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

  # (id, kcd_main) universe: within the disease lookback window. IRREGULAR and
  # UNMAPPED have no lookback of their own, so they fall back to the 5-year window;
  # VACANT was pinned in-window by map_disease() so a codeless-only insured survives.
  no_window <- c(.KCD_IRREGULAR, .KCD_UNMAPPED)
  in_scope  <- reviewed[in_lookback == 1L | (kcd_main %chin% no_window & in_5yr == 1L)]

  # a codeless (VACANT) line marks the line, not the insured: someone with any real
  # diagnosis in scope has something to underwrite, so their VACANT lines add nothing.
  # Keep VACANT only where it is the whole story.
  underwritable <- unique(in_scope[kcd_main != .KCD_VACANT, id])
  in_scope      <- in_scope[kcd_main != .KCD_VACANT | !id %in% underwritable]
  id_disease    <- unique(in_scope[, .(id, kcd_main)])

  # per-treatment-type elapsed days, each the most recent (min) within scope
  hos_elp_day <- .min_elapsed(in_scope[hos_day > 0],                 "hos_elp_day")
  sur_elp_day <- .min_elapsed(in_scope[sur_cnt > 0],                 "sur_elp_day")
  out_elp_day <- .min_elapsed(in_scope[hos_day == 0 & sur_cnt == 0], "out_elp_day")

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
  # age (per insured) travels with each disease row -- the rule bands on it. Take it
  # from the whole mapped table, not just the reviewed rows, so an insured with no
  # reviewed row at all (below) still has an age.
  all_rows <- as.data.table(mapped)
  all_rows[, elapsed := pmax(0L, as.integer(inq_date - pmax(acc_date, sdate, edate, na.rm = TRUE)))]
  ages <- all_rows[, .(age = suppressWarnings(max(as.integer(age), na.rm = TRUE))), by = id]
  ages[!is.finite(age), age := NA_integer_]
  result[ages, on = .(id), age := i.age]

  # every id in `mapped` must leave with a row -- the no-insured-left-behind
  # invariant, guaranteed HERE and not just upstream. Any id not already in `result`
  # had nothing in scope: an insured all of whose diagnoses aged out AND who (unlike a
  # codeless one) has no VACANT line to survive on, or -- the defensive case, which
  # the normal pipeline never produces since clean_icis() gives every line a main
  # kcd0 -- an insured with no reviewed row at all. Give each one EXPIRED placeholder
  # to keep the id: it always resolves to standard, so its counts feed no rule and are
  # 0, but `elp_day` carries the days since their most recent treatment (a real
  # figure, not a fabricated 0 that would read as "still under treatment").
  #
  # How several expired diagnoses collapse to ONE row: those rows never entered
  # `id_disease` (it is built from in_scope, which they failed), so `result` has none
  # of them. Here `by = id` folds an insured's many expired lines -- M51, M54, K40,
  # each a different kcd_main -- into a single group, and the original codes are then
  # dropped and replaced wholesale by `.KCD_EXPIRED`. There is no merge of kcd_main
  # values; the fold is `by = id`, and the per-disease detail is gone from here on
  # (diagnose_icis() reports it instead).
  #
  # `elp_day` is `min(elapsed)` over the id's REVIEWED lines -- the SAME definition
  # every other row uses, days since the most recent reviewed treatment -- so the
  # EXPIRED row is not a special case. Reviewed-only, not all lines: a non-reviewed
  # line (e.g. a rejected or duplicate claim) is not a treatment we would date, so it
  # must not pull `elp_day` forward and make an aged-out insured read as recently
  # treated. Note this is the most recent TREATMENT, not the most recent EXPIRY: a
  # short-lookback diagnosis can expire while still being the person's latest visit,
  # so a small `elp_day` here does NOT mean "barely expired". This one number cannot
  # explain why several diagnoses each aged out; diagnose_icis()'s scope section tells
  # that story (which diagnoses, out of which windows). An id with no dated reviewed
  # line at all gets `elp_day = NA` (never treated), not a fabricated number.
  #
  # `outside` groups over ALL of an id's lines so a no-reviewed-line id is still kept,
  # but the `elp_day` sub-expression reads only its `review == 1` elapsed.
  outside  <- all_rows[!id %in% result$id]
  no_scope <- outside[, .(elp_day = { e <- elapsed[review == 1L & !is.na(elapsed)]
                                      if (length(e)) min(e) else NA_integer_ }), by = id]
  if (nrow(no_scope)) {
    no_scope[ages, on = .(id), age := i.age]
    result <- rbind(result, no_scope[, .(id, kcd_main = .KCD_EXPIRED, age,
                                         hos_day = 0L, sur_cnt = 0L, out_cnt = 0L,
                                         hos_elp_day = NA_integer_, sur_elp_day = NA_integer_,
                                         out_elp_day = NA_integer_, elp_day)],
                    use.names = TRUE)
  }
  setcolorder(result, c("id", "kcd_main", "age", "hos_day", "sur_cnt", "out_cnt",
                        "hos_elp_day", "sur_elp_day", "out_elp_day", "elp_day"))
  result[]
}

# Days since the most recent treatment of one kind, per (id, kcd_main). A book with
# no surgeries at all, or an insured whose every claim line is an inpatient one,
# leaves one of these subsets empty -- and data.table still evaluates `j` once on an
# empty subset, so `min()` of nothing would warn and return `Inf`.
.min_elapsed <- function(rows, col) {
  if (!nrow(rows)) {
    empty <- rows[0L, .(id, kcd_main)]
    return(empty[, (col) := integer()])
  }
  out <- rows[, .(min_elapsed = min(elapsed)), by = .(id, kcd_main)]
  setnames(out, "min_elapsed", col)
  out[]
}
