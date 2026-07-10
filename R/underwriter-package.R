#' @keywords internal
"_PACKAGE"

#' @import data.table
#' @importFrom ggplot2 .data
#' @importFrom stats median setNames
#' @importFrom utils head
NULL

# Reserved `kcd_main` values: not diagnoses, but the reason an insured reached the
# rule set with no diagnosis to underwrite. Every insured keeps a row through the
# whole pipeline, so when nothing real is left, one of these takes the `kcd_main`
# slot and records WHY. None is wired to a decision here; the rule set decides each
# by a row of its own, so onboarding another insurer is a spreadsheet edit. A code
# these words could never be (`normalize_kcd()` admits only `^[A-Z][0-9]{2,}$`), so
# they cannot collide with a real diagnosis.
#
#   tier         value      set by               rule    counts / elp_day
#   -----------  ---------  -------------------  ----    ----------------------------
#   code slot    VACANT     clean_icis()         S       real row: 5-yr counts, in-scope elp_day
#   (per line)   IRREGULAR  clean_icis()         U       real row: 5-yr counts, in-scope elp_day
#                UNMAPPED   map_disease()        U       real row: 5-yr counts, in-scope elp_day
#   per person   EXPIRED    aggregate_disease()  S       placeholder: counts 0, elp_day real
#
# THREE are code-level: the diagnosis SLOT itself holds no usable KCD code, stamped
# per claim line as it moves through the early stages. VACANT (empty) passes; the
# other two route to review. Because they occupy a `kcd_main` like any diagnosis,
# aggregate_disease() computes their inputs the ordinary way -- hospital days,
# surgery and outpatient counts over the fixed 5-year window, elapsed days within
# scope -- so a rule set could band on them (an IRREGULAR with many hospital days
# could be treated differently from one with none). EXPIRED is the exception: a
# synthesised placeholder, so its counts stay 0 (see below).
.KCD_VACANT    <- "VACANT"
.KCD_IRREGULAR <- "IRREGULAR"
.KCD_UNMAPPED  <- "UNMAPPED"

# EXPIRED is the odd one out, and the one worth understanding. Its codes are
# perfectly good -- real, parsed, mapped -- so it is not a bad code but a per-PERSON
# verdict: EVERY one of the insured's diagnoses is older than the window that disease
# is reviewed within (`lookback_mon`, which varies by disease: a code looked back 1
# year expires at 13 months, one looked back 5 years survives to 59). Past its window
# a diagnosis stops being underwritten, so an insured whose diagnoses have all aged
# out has nothing left to review.
#
# Such an insured would leave NO row at all -- their expired diagnoses are dropped
# (kept, they would wrongly match their old rules and draw an exclusion or decline),
# and unlike a codeless insured they have no VACANT line to survive on. So
# aggregate_disease() gives them one EXPIRED placeholder, the only sentinel it must
# synthesise rather than read off a line. It exists to keep the id in the feed, not
# to be banded on: it always resolves to standard, so its counts feed no rule and are
# left 0. `elp_day` IS kept -- days since the most recent treatment (e.g. 466 -> last
# seen ~15 months ago, which is exactly why a 1-year window let it lapse) -- because
# an auditor reads it to see why the insured expired, and a fabricated 0 would
# misread as "still in treatment". To underwrite these people a company widens
# `lookback_mon` so a diagnosis stays in its window; it does not band this row.
.KCD_EXPIRED   <- "EXPIRED"

# data.table's non-standard evaluation references column names as bare symbols,
# which R CMD check would otherwise flag as undefined global variables. Register
# every column symbol the pipeline uses so the check stays clean.
utils::globalVariables(c(
  # data.table specials / non-equi join refs used as bare symbols
  ".", "i.kcd_main", "i.sub_chk", "i.lookback_mon", "i.age",
  # claim / cleansing columns
  "id", "gender", "age", "inq_date", "pay_date", "acc_date", "sdate", "edate",
  "hos_day", "sur_cnt", "kcd", "ord", "sub_kcd", "N",
  # disease mapping + scope flags
  "kcd_main", "sub_chk", "lookback_mon", "review", "tdate", "in_lookback", "in_5yr",
  # aggregation
  "elapsed", "hos_elp_day", "sur_elp_day", "out_elp_day", "elp_day", "out_cnt", "stay", "min_elapsed",
  # rule matching
  "decl_yn", "age_min", "age_max", "elp_day_min", "elp_day_max",
  "sur_cnt_min", "sur_cnt_max", "hos_day_min", "hos_day_max",
  "no", "matched", "rid", "conflict", "V1",
  # decision combining
  "code", "coverage", "method", "rank", "dec", "token",
  "site", "n_site", "mark", "months", "index", "total", "decision",
  "at_least", "class_letter", "has_terminal", "pos",
  "reason", "i.reason", "n_cell", "rule_no", "i.rule_no", "i.kcd_main",
  # decision tabulation
  "n", "prop", "category", "auto",
  # decision tracing
  "diseases", "computed", "stored", "ok",
  # disease relaxation experiment
  "auto_base", "auto_relaxed", "lift", "n_flipped", "n_total",
  "n_causes", "n_id", "auto_lift", "n_cov", "state", "share",
  "component", "individual", "joint", "synergy"
))
