#' @keywords internal
"_PACKAGE"

#' @import data.table
#' @importFrom stats setNames
NULL

# data.table's non-standard evaluation references column names as bare symbols,
# which R CMD check would otherwise flag as undefined global variables. Register
# every column symbol the pipeline uses so the check stays clean.
utils::globalVariables(c(
  # claim / cleansing columns
  "id", "gender", "age", "inq_date", "pay_date", "acc_date", "sdate", "edate",
  "hos_day", "hos_cnt", "sur_cnt", "kcd", "ord", "sub_kcd", "N",
  # disease mapping + scope flags
  "kcd_main", "sub_chk", "lookback_mon", "review", "tdate", "in_lookback", "in_5yr",
  # aggregation
  "elapsed", "hos_elp_day", "sur_elp_day", "out_elp_day", "elp_day", "out_cnt", "stay",
  # rule matching
  "decl_yn", "age_min", "age_max", "elp_day_min", "elp_day_max",
  "sur_cnt_min", "sur_cnt_max", "hos_day_min", "hos_day_max",
  "no", "matched", "rid", "conflict", "V1",
  # decision combining
  "code", "coverage", "method", "priority", "rank", "dec", "token",
  "area", "mark", "months", "index", "total", "lower", "decision",
  # decision tabulation
  "n", "ratio", "category", "auto",
  # decision tracing
  "diseases", "computed", "stored", "ok"
))
