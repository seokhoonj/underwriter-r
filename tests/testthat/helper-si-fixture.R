# A tiny synthetic simplified-issue fixture (no real claim data). One product,
# two coverages, a handful of diseases, and the workbook tables the engine reads.
# Used across the SI engine tests.

# A rulebook of the shape load_si_rulebook() returns: the seven sheet tables plus
# the three derived lookups (code / rank / auto). Kept minimal but complete -- one
# product "325", coverages life and care, a couple of carve-out bands, one
# critical disease, and the four sentinels.
si_fixture_rulebook <- function() {
  decision <- data.table::data.table(
    priority = c(1L, 2L, 3L),
    code     = c("D", "U", "S"),
    name     = c("decline", "refer", "accept"),
    role     = c("decline", "underwriter", "standard"),
    auto     = c(1L, 0L, 1L))

  # D1 carved out on both coverages (a mild disease that stays acceptable);
  # bands are wide so an in-range applicant passes, one elp floor to exercise
  # "elp_day_short". K1 has NO band -> "no_rule" decline when it trips a question.
  ruleset <- data.table::data.table(
    si_type     = "325",
    coverage    = c("life", "care"),
    kcd_main    = c("D1", "D1"),
    age_min     = 0L,   age_max     = 80L,
    elp_day_min = 30L,
    sur_cnt_min = 0L,   sur_cnt_max = 5L,
    hos_day_min = 0L,   hos_day_max = 30L,
    kcd_max     = 25L,  recover     = 1L)

  critical_disease <- data.table::data.table(
    critical_disease_set = "base",
    coverage             = c("life", "care"),
    kcd_main             = c("C1", "C1"),
    kcd_main_ko          = "critical-one",
    class                = c(2L, 3L))     # life: class 2, care: class 3

  product <- data.table::data.table(
    si_type                     = "325",
    medical_advice_mon          = 3L,
    medical_advice_lookback_mon = 60L,
    inpatient_surgery_mon       = 24L,
    critical_disease_mon        = 60L,
    care_mon                    = 24L,
    critical_disease_set        = "base")

  sentinel <- data.table::data.table(
    kcd_main    = c("VACANT", "EXPIRED", "IRREGULAR", "UNMAPPED"),
    kcd_main_ko = c("no-dx", "aged-out", "unreadable", "unmapped"),
    role        = c("standard", "standard", "underwriter", "underwriter"))

  reason <- data.table::data.table(
    reason    = c("recent_treatment", "critical_disease", "no_rule",
                  "hos_day_over", "sur_cnt_over", "elp_day_short",
                  "age_out_of_band", "kcd_n_over"),
    reason_ko = c("recent_treatment", "critical_disease", "no_rule",
                  "hos_day_over", "sur_cnt_over", "elp_day_short",
                  "age_out_of_band", "kcd_n_over"),
    question  = c("Q1", "Q3", "Q2", "Q2", "Q2", "Q2", "Q2", "Q2"))

  coverage <- data.table::data.table(
    coverage = c("life", "care"), ko = c("life", "care"))

  rb <- list(product = product, coverage = coverage, decision = decision,
             ruleset = ruleset, sentinel = sentinel,
             critical_disease = critical_disease, reason = reason)
  rb$code <- stats::setNames(as.character(decision$code), decision$role)
  rb$rank <- stats::setNames(as.integer(decision$priority), decision$code)
  rb$auto <- stats::setNames(as.integer(decision$auto), decision$code)
  data.table::setkey(rb$ruleset, si_type, coverage, kcd_main)
  data.table::setkey(rb$critical_disease, critical_disease_set, coverage, kcd_main)
  rb
}

# One RAW ICIS claim line, before clean_icis() -- dates as YYYYMMDD strings, one
# diagnosis in kcd0. Used to exercise the full underwrite_si() front end. `inq` is
# the inquiry date; the treatment is dated `elapsed` days before it. With `hos > 0`
# clean_icis() derives the discharge from sdate + hos_day; otherwise it is an
# outpatient visit (edate stays NA).
si_raw <- function(id, kcd, inq, elapsed, hos = 0L, sur = 0L, age = 40L) {
  ymd   <- function(d) format(d, "%Y%m%d")
  inq_d <- as.Date(inq)
  tdate <- inq_d - elapsed
  data.table::data.table(
    id = as.character(id), gender = "1", age = as.integer(age),
    inq_date = ymd(inq_d), pay_date = ymd(inq_d), acc_date = ymd(tdate),
    sdate = ymd(tdate), edate = NA_character_,
    hos_day = as.integer(hos), hos_cnt = if (hos > 0L) 1L else 0L,
    sur_cnt = as.integer(sur),
    kcd0 = kcd, kcd1 = NA_character_, kcd2 = NA_character_,
    kcd3 = NA_character_, kcd4 = NA_character_)
}

# The disease mapping the SI products share with the standard path, as map_disease()
# expects it: each raw `kcd` to its representative `kcd_main`. `kcd` must be a real
# KCD code, since clean_icis() marks a malformed one IRREGULAR before it ever reaches
# the mapping.
si_disease_table <- function(kcd, kcd_main) {
  data.table::data.table(kcd = kcd, kcd_main = kcd_main,
                         sub_chk = 0L, lookback_mon = 60L)
}

# Write a fixture rulebook list back out as the seven-sheet workbook that
# load_si_rulebook() reads, so the real loader (and its boundary checks) can be
# exercised end to end rather than reimplemented in the test.
si_write_fixture_workbook <- function(rb, path) {
  writexl::write_xlsx(list(product          = rb$product,
                           coverage         = rb$coverage,
                           decision         = rb$decision,
                           ruleset          = rb$ruleset,
                           ruleset_sentinel = rb$sentinel,
                           critical_disease = rb$critical_disease,
                           reason           = rb$reason), path)
}

# One claim line in the shape map_disease() emits. `inq` is the inquiry date; each
# treatment is dated `elapsed` days before it. `hos`/`sur` set the treatment type;
# a line with neither is outpatient.
si_line <- function(id, kcd_main, inq, elapsed, hos = 0L, sur = 0L, age = 40L) {
  tdate <- as.Date(inq) - elapsed
  data.table::data.table(
    id = as.character(id), gender = "1", age = as.numeric(age),
    inq_date = as.Date(inq), pay_date = as.Date(NA), acc_date = tdate,
    sdate = if (hos > 0L) tdate else as.Date(NA),
    edate = if (hos > 0L) tdate + (hos - 1L) else as.Date(NA),
    hos_day = as.numeric(hos), hos_cnt = if (hos > 0L) 1 else 0,
    sur_cnt = as.numeric(sur),
    kcd = kcd_main, sub_kcd = 0L, kcd_main = kcd_main,
    sub_chk = 0, lookback_mon = 60, review = 1L, in_lookback = 1L, in_5yr = 1L)
}
