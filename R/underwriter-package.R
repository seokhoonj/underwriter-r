#' @keywords internal
"_PACKAGE"

#' @import data.table
#' @importFrom stats median setNames
#' @importFrom utils head
#' @importFrom ggplot2 .data
NULL

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
  "area", "mark", "months", "index", "total", "lower", "decision",
  # decision tabulation
  "n", "prop", "category", "auto",
  # decision tracing
  "diseases", "computed", "stored", "ok",
  # disease relaxation experiment
  "auto_base", "auto_relaxed", "lift", "n_flipped", "n_total",
  "n_src", "n_id", "auto_lift", "n_cov", "state", "share"
))
