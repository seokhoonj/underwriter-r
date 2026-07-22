# Simplified-issue underwriting evaluates three application-form questions
# independently, then folds them per coverage with decline > underwriter >
# standard. The front end is shared with the standard path and unchanged:
#   clean_icis() -> filter_latest_inquiry() -> melt_kcd() -> map_disease()
# The fork starts here. Unlike the standard path there is no single aggregation
# the three questions could share -- each windows and counts differently -- so
# there is no aggregate_disease() counterpart; match_si_rule() reads mapped lines
# directly and hands each question the line table.
#
# Two elapsed variants, because whether outpatient counts differs by question
# (icis_spec 2.702 rows 151, 156, 161):
#   elp_hos_sur = min elapsed over hospitalization/surgery lines (Q2 uses this)
#   elp_all     = min elapsed over every line incl. outpatient   (Q1, Q3 use it)

# One row per mapped claim line, tagged with treatment date, elapsed days and
# treatment type by the shared primitive (see .tag_treatment). NOT collapsed to
# (id, kcd_main): each question windows and counts differently, so collapsing
# here would lose the per-line dates. Unlike aggregate_disease(), SI does not
# filter on review == 1 -- it counts main and sub diagnoses regardless of sub_chk.
.tag_lines <- function(mapped) {
  dt <- as.data.table(copy(mapped))
  .tag_treatment(dt)
  dt[]
}

# The empty answer shell every .assess_* returns when its question raises nothing.
# `id` is typed from the input, never hardcoded character(): a numeric-id book must
# keep its id type, or rbindlist() with a non-empty answer flips the whole column to
# character and the fold's join in combine_si_decision() fails on a type mismatch.
# Same reason .count_in_window() derives its empty shell from the data.
.empty_answer <- function(lines) {
  id <- NULL  # NSE
  lines[0L, .(id)][, `:=`(coverage = character(), kcd_main = character(),
                          dec = character(), reason = character())][]
}

# Per-(id, kcd_main) counts and elapsed over a month window. Reused by every
# question so the counting rule -- unique accident dates for surgery/outpatient,
# dedup-union of calendar days for hospitalization -- stays in one place. The
# empty shell is derived from the window slice (dt[0L, .(...)]) rather than typed
# by hand, so the `id` column keeps the input's type; a hardcoded character()
# would flip a numeric-id column to character on rbind and break the later join.
.count_in_window <- function(lines, window_mon) {
  id <- kcd_main <- is_hos <- is_sur <- elapsed <- NULL  # NSE
  win <- .within_months(lines, window_mon)
  if (!nrow(win)) {
    empty <- win[0L, .(id, kcd_main)]
    return(cbind(empty, hos_day = integer(), sur_cnt = integer(), out_cnt = integer(),
                 elp_hos_sur = integer(), elp_all = integer()))
  }

  counted <- .count_treatment(win)
  # two elapsed variants -- see file header. elp_hos_sur is deliberately left NA
  # when the disease has no hospitalization or surgery line: Q2 must tell "no such
  # treatment" apart from "treated today", and a 0 fill would read as the latter.
  elp_hs  <- win[is_hos == TRUE | is_sur == TRUE,
                 .(elp_hos_sur = min(elapsed)), by = .(id, kcd_main)]
  elp_all <- win[, .(elp_all = min(elapsed)), by = .(id, kcd_main)]

  Reduce(function(x, y) merge(x, y, by = c("id", "kcd_main"), all.x = TRUE),
         list(counted, elp_hs, elp_all))[]
}

# The SI analog of a rule-set band match, shared by Q2's core and Q1's defer
# branch. (si_type, coverage, kcd_main) is unique in the sheet, so this is an
# equi-join plus band tests -- NOT the non-equi cartesian join match_rule() needs;
# at most one band applies, so there is nothing to rank. No matching band ->
# not carved out -> decline.
#
# `checks` names the band tests that apply. Q2 evaluates the whole band; Q1's
# defer branch evaluates only disease-existence and elapsed, which is all
# icis_spec 2.702 row 143 names for it, so age / hospital days / surgery count /
# disease count are not its business. Running the full band there would decline
# applicants on facts the 3-month question never looks at.
#
# A missing input fails its band. `NA >= age_min` is NA, not FALSE, and an
# unguarded NA would flow to the fold and be laundered into a standard accept --
# accepting an applicant whose age or treatment count could not be read. The
# `!is.na()` guards make an unknown fail the carve-out, i.e. fall to decline.
#
# `recover` (completed-recovery status) is NOT tested: it applies only with
# declaration data, which an ICIS-only run never has (rows 21, 143).
.match_band <- function(agg, ruleset, product, code,
                        checks = c("rule", "age", "hos", "sur", "elp", "kcd_n")) {
  id <- kcd_main <- coverage <- si_type <- age <- hos_day <- sur_cnt <- NULL  # NSE
  elp <- kcd_n <- age_min <- age_max <- elp_day_min <- kcd_max <- NULL
  sur_cnt_min <- sur_cnt_max <- hos_day_min <- hos_day_max <- NULL
  has_rule <- ok_age <- ok_hos <- ok_sur <- ok_elp <- ok_kcd_n <- dec <- NULL

  rule <- as.data.table(ruleset)[si_type == product$si_type]
  cand <- as.data.table(copy(agg))[, .SD, .SDcols = c("id", "kcd_main", "coverage",
                                                      "age", "hos_day", "sur_cnt",
                                                      "elp", "kcd_n")]
  out <- merge(cand, rule[, .(coverage, kcd_main, age_min, age_max, elp_day_min,
                              sur_cnt_min, sur_cnt_max, hos_day_min, hos_day_max, kcd_max)],
               by = c("coverage", "kcd_main"), all.x = TRUE)

  out[, has_rule := !is.na(age_min)]
  out[, ok_age   := has_rule & !is.na(age)     & age     >= age_min     & age     <= age_max]
  out[, ok_hos   := has_rule & !is.na(hos_day) & hos_day >= hos_day_min & hos_day <= hos_day_max]
  out[, ok_sur   := has_rule & !is.na(sur_cnt) & sur_cnt >= sur_cnt_min & sur_cnt <= sur_cnt_max]
  out[, ok_elp   := has_rule & !is.na(elp)     & elp     >= elp_day_min]  # elp is one-sided
  out[, ok_kcd_n := has_rule & !is.na(kcd_n)   & kcd_n   <= kcd_max]

  ok <- rep(TRUE, nrow(out))
  if ("age"   %in% checks) ok <- ok & out$ok_age
  if ("hos"   %in% checks) ok <- ok & out$ok_hos
  if ("sur"   %in% checks) ok <- ok & out$ok_sur
  if ("elp"   %in% checks) ok <- ok & out$ok_elp
  if ("kcd_n" %in% checks) ok <- ok & out$ok_kcd_n
  out[, dec := fifelse(has_rule & ok, code[["standard"]], code[["decline"]])]

  # only a test that was actually applied may name the reason
  out[, reason := fcase(
    dec == code[["standard"]],       NA_character_,
    !has_rule,                       "no_rule",
    "age"   %in% checks & !ok_age,   "age_out_of_band",
    "hos"   %in% checks & !ok_hos,   "hos_day_over",
    "sur"   %in% checks & !ok_sur,   "sur_cnt_over",
    "elp"   %in% checks & !ok_elp,   "elp_day_short",
    "kcd_n" %in% checks & !ok_kcd_n, "kcd_n_over",
    default =                        NA_character_)]
  out[, .(id, coverage, kcd_main, dec, reason)]
}

# --- Q1: medical advice (3-month outpatient history) ------------------------
# Over a fixed 5-year lookback, outpatient lines only. A disease raises the
# question if it was seen within medical_advice_mon months. If every such visit
# is recent -> treatment still in progress -> decline (referral instead on
# declaration data, which carries context ICIS cannot). If seen both long ago and
# recently -> ongoing management -> defer to the same carve-out band Q2 uses,
# judged on disease code + elapsed only (row 143), not on the full band.
.assess_medical_advice <- function(lines, ruleset, product, code, source) {
  id <- kcd_main <- coverage <- is_out <- tdate <- inq_date <- age <- NULL  # NSE
  is_recent <- elapsed <- any_recent <- all_recent <- elp_all <- kcd_n <- elp <- NULL
  reject_dec <- if (source == "icis") code[["decline"]] else code[["underwriter"]]
  empty <- .empty_answer(lines)

  out <- .within_months(lines, product$medical_advice_lookback_mon)[is_out == TRUE]
  if (!nrow(out)) return(empty)

  # "recent" is medical_advice_mon months back, via the same month arithmetic
  # every window uses rather than a fixed 91 days: three calendar months run 89
  # to 92 days, and a fixed value shifts the boundary by up to three days.
  recent_from <- .minus_months(out$inq_date, product$medical_advice_mon)
  out[, is_recent := tdate >= recent_from]

  span <- out[, .(any_recent = any(is_recent), all_recent = all(is_recent),
                  elp_all = min(elapsed)), by = .(id, kcd_main)][any_recent == TRUE]
  if (!nrow(span)) return(empty)

  reject <- span[all_recent == TRUE]    # every visit inside the advice window
  defer  <- span[all_recent == FALSE]   # seen both long ago and recently

  res <- list()
  if (nrow(reject))
    res$reject <- reject[CJ(id = unique(reject$id), coverage = product$coverages),
                         on = "id", allow.cartesian = TRUE][
      , .(id, coverage, kcd_main, dec = reject_dec, reason = "recent_treatment")]

  if (nrow(defer)) {
    # The band is judged on disease code + elapsed (row 143), so the counts come
    # from the OUTPATIENT lines Q1 selected -- not every line. Counting all lines
    # let a years-old admission, which Q1 does not look at and Q2's shorter window
    # already excluded, decline the applicant on hospital days. Elapsed here
    # includes outpatient (elp_all), unlike Q2.
    agg <- .count_in_window(lines[is_out == TRUE],
                            window_mon = product$medical_advice_lookback_mon)[
      defer, on = .(id, kcd_main), nomatch = NULL]
    age <- unique(lines[, .(id, age)], by = "id")
    agg <- agg[age, on = "id", nomatch = NULL]
    agg <- agg[CJ(id = unique(agg$id), coverage = product$coverages),
               on = "id", allow.cartesian = TRUE]
    # this branch checks only rule + elp (row 143), so kcd_n never enters the
    # decision; .match_band still projects the column, so carry a placeholder
    # rather than scanning every window to build a count nothing here reads.
    agg[, kcd_n := 0L]
    agg[, elp := elp_all]                # Q1 includes outpatient
    res$defer <- .match_band(agg, ruleset, product, code, checks = c("rule", "elp"))
  }

  out_res <- rbindlist(res, use.names = TRUE, fill = TRUE)
  if (!nrow(out_res)) empty else out_res[]
}

# --- Q2: inpatient / surgery over the product's N-year window ---------------
# The disease count is per insured over the CRITICAL-disease window (care 24
# months, else the product's), over all treatment types (rows 148-150); the
# per-disease inputs are over the product's admission window, and only diseases
# with a hospitalization or surgery line raise the question at all (row 21).
.assess_inpatient_surgery <- function(lines, ruleset, product, code) {
  id <- kcd_main <- coverage <- hos_day <- sur_cnt <- age <- NULL  # NSE
  kcd_n <- window_mon <- elp <- elp_hos_sur <- NULL
  empty <- .empty_answer(lines)
  if (is.na(product$inpatient_surgery_mon)) return(empty)  # 305-family asks no Q2

  windows <- unique(vapply(product$coverages, product$critical_disease_window, integer(1)))
  kcd_n <- rbindlist(lapply(windows, function(w) {
    .within_months(lines, w)[, .(kcd_n = uniqueN(kcd_main)), by = id][, window_mon := w]
  }))

  agg <- .count_in_window(lines, window_mon = product$inpatient_surgery_mon)
  agg <- agg[hos_day > 0L | sur_cnt > 0L]
  if (!nrow(agg)) return(empty)

  age <- unique(lines[, .(id, age)], by = "id")
  agg <- agg[age, on = "id", nomatch = NULL]

  # one row per coverage the product sells; care's 24-month count differs from the rest
  agg <- agg[CJ(id = unique(agg$id), coverage = product$coverages),
             on = "id", allow.cartesian = TRUE]
  agg[, window_mon := product$critical_disease_window(coverage)]
  agg[kcd_n, on = .(id, window_mon), kcd_n := i.kcd_n]
  agg[is.na(kcd_n), kcd_n := 0L]
  agg[, elp := elp_hos_sur]              # Q2 excludes outpatient

  .match_band(agg, ruleset, product, code)
}

# --- Q3: critical disease, per coverage -------------------------------------
# Each coverage classes a disease 0-3 by how its treatment history is judged
# (rows 163-175): 0 not critical -> standard; 1 admission/surgery -> decline,
# outpatient ignored; 2 admission/surgery -> decline, outpatient-only -> referral;
# 3 any treatment -> decline. Windows differ by coverage (care 24 vs the rest),
# so presence is computed per window.
.assess_critical_disease <- function(lines, product, code) {
  id <- kcd_main <- coverage <- is_hos <- is_sur <- is_out <- NULL  # NSE
  has_hos_sur <- has_outpatient <- class <- dec <- NULL
  cls <- as.data.table(product$critical_disease)

  res <- rbindlist(lapply(product$coverages, function(cvg) {
    win <- .within_months(lines, product$critical_disease_window(cvg))
    if (!nrow(win)) return(NULL)
    presence <- win[, .(has_hos_sur = any(is_hos | is_sur), has_outpatient = any(is_out)),
                    by = .(id, kcd_main)]
    # inner join: a disease absent from the table is not critical, so it produces
    # no decision here and cannot worsen the coverage.
    presence[cls[coverage == cvg, .(kcd_main, class)], on = "kcd_main", nomatch = NULL][
      , .(id, coverage = cvg, kcd_main, class, has_hos_sur, has_outpatient)]
  }))
  if (!length(res) || !nrow(res))
    return(.empty_answer(lines))

  res[, dec := fcase(
    class == 0L,                           code[["standard"]],
    class == 1L & has_hos_sur,             code[["decline"]],
    class == 1L,                           code[["standard"]],
    class == 2L & has_hos_sur,             code[["decline"]],
    class == 2L & has_outpatient,          code[["underwriter"]],
    class == 2L,                           code[["standard"]],
    class == 3L & (has_hos_sur | has_outpatient), code[["decline"]],
    class == 3L,                           code[["standard"]],
    default =                              code[["standard"]])]
  res[, reason := fifelse(dec == code[["standard"]], NA_character_, "critical_disease")]
  res[, .(id, coverage, kcd_main, dec, reason)]
}


# Settle the insureds in the roster who raised no question, so match_si_rule() holds
# the whole population. They split by whether any history is still in reach:
#  - aged out (every KNOWN in_5yr flag is 0): no reviewable history, so settle as the
#    EXPIRED sentinel, its role read from the workbook.
#  - otherwise -- usable in-window history that tripped nothing, OR no readable date
#    (in_5yr all NA, e.g. a missing inquiry date, which must NOT drop the insured) --
#    a presence row (no decision here) that combine settles at the baseline standard.
# `max()` over a group with any NA would be NA and lose the insured from both sides,
# so aged-out is judged on the known flags only.
.settle_roster <- function(lines, roster_ids, product, code, sent) {
  id <- in_5yr <- is_aged_out <- coverage <- kcd_main <- role <- NULL  # NSE
  stopifnot("in_5yr" %in% names(lines))
  age_status   <- lines[id %chin% roster_ids,
                        .(is_aged_out = { v <- in_5yr[!is.na(in_5yr)]
                                          length(v) > 0L && max(v) == 0L }), by = id]
  expired_ids  <- age_status[is_aged_out == TRUE,  id]
  presence_ids <- age_status[is_aged_out == FALSE, id]
  rows <- list()
  if (length(expired_ids))
    rows$expired <- CJ(id = expired_ids, coverage = product$coverages)[
      , .(question = "sentinel", id, coverage, kcd_main = .KCD_EXPIRED,
          dec = unname(code[sent[kcd_main == .KCD_EXPIRED, role]]), reason = NA_character_)]
  if (length(presence_ids))
    rows$presence <- CJ(id = presence_ids, coverage = product$coverages)[
      , .(question = NA_character_, id, coverage, kcd_main = NA_character_,
          dec = NA_character_, reason = NA_character_)]
  rbindlist(rows, use.names = TRUE)
}

#' Match mapped claim lines against a simplified-issue rule set
#'
#' The simplified-issue counterpart of [match_rule()]. Where the standard path
#' matches one band per disease, this evaluates the three application-form
#' questions independently and returns every answer they produced;
#' [combine_si_decision()] folds them per coverage. Because each question windows
#' and counts differently there is no shared aggregation step -- SI has no
#' `aggregate_disease` counterpart -- so this reads `mapped` directly.
#'
#' The four reserved sentinel codes (no diagnosis, all aged out, unreadable, not
#' in the mapping table) are structural states, not diseases; putting them to the
#' questions would decline an applicant for having no claim history. They are
#' withheld from the questions and settled from the `ruleset_sentinel` sheet.
#'
#' Every insured is represented in the result, so it is the whole roster and
#' [combine_si_decision()] folds it without a separate id list. An insured that
#' raised no question is carried through one of two ways: if every line is aged out
#' of the 5-year window they are settled as the `EXPIRED` sentinel (its role from
#' the workbook); otherwise -- usable, in-window history that tripped nothing --
#' they are a `question`-`NA` presence row that combine settles at the baseline
#' standard.
#'
#' @param mapped Mapped claim lines from [map_disease()].
#' @param rulebook A rulebook from [load_si_rulebook()].
#' @param product Product configuration from [si_product()].
#' @param source `"icis"` (claim history alone) or `"declaration"`. Q1's recent
#'   history declines on claim data but only refers on a declaration, which
#'   carries context claim history alone cannot show.
#' @return A `data.table`, one row per answer, with `question`
#'   (`Q1`/`Q2`/`Q3`/`sentinel`, or `NA` for an insured that tripped nothing),
#'   `id`, `coverage`, `kcd_main`, `dec`, `reason`.
#' @seealso [combine_si_decision()], [si_product()], [load_si_rulebook()].
#' @export
match_si_rule <- function(mapped, rulebook, product,
                          source = c("icis", "declaration")) {
  id <- kcd_main <- coverage <- role <- NULL  # NSE
  source  <- match.arg(source)
  lines   <- .tag_lines(mapped)
  code    <- rulebook$code
  ruleset <- rulebook$ruleset

  sent    <- as.data.table(rulebook$sentinel)
  is_sent <- lines$kcd_main %chin% sent$kcd_main
  sentinel_answer <- if (any(is_sent)) {
    unique(lines[is_sent, .(id, kcd_main)])[
      , .(coverage = product$coverages), by = .(id, kcd_main)][
      sent, on = "kcd_main", nomatch = NULL][
      , .(id, coverage, kcd_main, dec = unname(code[role]), reason = NA_character_)]
  } else NULL

  answers <- rbindlist(list(
    Q1 = .assess_medical_advice(   lines[!is_sent], ruleset, product, code, source),
    Q2 = .assess_inpatient_surgery(lines[!is_sent], ruleset, product, code),
    Q3 = .assess_critical_disease( lines[!is_sent],          product, code),
    sentinel = sentinel_answer),
    use.names = TRUE, fill = TRUE, idcol = "question")

  # Every insured with no question answer is carried through, so match_si_rule()
  # holds the whole roster and combine_si_decision() needs no external id list.
  roster_ids <- setdiff(unique(lines$id), unique(answers$id))
  if (length(roster_ids))
    answers <- rbind(answers, .settle_roster(lines, roster_ids, product, code, sent),
                     use.names = TRUE)
  answers[]
}
