#' @keywords internal
"_PACKAGE"

#' @import data.table
#' @importFrom ggplot2 .data
#' @importFrom stats median setNames
#' @importFrom utils head
NULL

# Reserved `kcd_main` values, ordered by leniency: AAA passes, ZZZ reviews.
#
# `AAA` marks an insured with nothing to underwrite -- the claim line carried no
# diagnosis code, or every diagnosis fell outside its lookback window. `ZZZ`
# marks a diagnosis the disease table does not cover, or a code cell that parsed
# to nothing.
#
# Neither is wired to a decision here. Both are ordinary `kcd_main` values that
# the rule set decides, so an insured is never dropped from the feed to be
# re-added later, and a missing rule row refers them to the underwriter rather
# than passing them silently.
.KCD_NO_DIAGNOSIS <- "AAA"
.KCD_UNMAPPED     <- "ZZZ"

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
  "elapsed", "hos_elp_day", "sur_elp_day", "out_elp_day", "elp_day", "out_cnt", "stay",
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
