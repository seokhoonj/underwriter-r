# A tiny synthetic underwriting fixture (no real claim data). Two coverages,
# a handful of insured, a decision-code table, and a per-disease `applied` table
# that produces a known `combined`. Used across the decision/relaxation tests.
#
# `applied` layout (id x kcd_main, one row each), decision on cov1/cov2:
#   A: M543 (cov1=U, cov2=S), M542 (cov1=S, cov2=U)   -> cov1=U (M543 sole), cov2=U (M542 sole)
#   B: M543 (cov1=U, cov2=D)                          -> cov1=U (M543 sole), cov2=D
#   C: N50  unmatched -> manual review everywhere      -> cov1=U (N50 sole),  cov2=U (N50 sole)
#   D: M543 (cov1=U), M542 (cov1=U)  co-hold cov1      -> cov1=U (two causes -> synergy), cov2=S

fixture <- function() {
  decision_cols <- c("cov1", "cov2")

  decision_table <- data.table::data.table(
    code     = c("S", "U", "D", "R", "E", "L"),
    priority = c(5L, 2L, 1L, 3L, 4L, 4L),
    combiner = c("priority", "priority", "priority", "exclusion", "loading", "reduction"),
    role     = c("standard", "manual_review", "decline", NA, NA, NA),
    auto     = c(1L, 0L, 1L, 1L, 1L, 1L)
  )
  exclusion_table <- data.table::data.table(mark = c("5i", "3", "99"))
  reduction_table <- data.table::data.table(mark = c("5i", "3", "99"))
  loading_table   <- data.table::data.table(lower = c(0L, 25L, 50L),
                                             decision = c("E(0)", "E(25)", "E(50)"))

  applied <- data.table::data.table(
    id       = c("A", "A", "B", "C", "D", "D"),
    kcd_main = c("M543", "M542", "M543", "N50", "M543", "M542"),
    elp_day  = c(100L, 200L, 100L, 50L, 100L, 100L),
    matched  = c(1L, 1L, 1L, 0L, 1L, 1L),
    cov1     = c("U", "S", "U", "U", "U", "U"),
    cov2     = c("S", "U", "D", "U", "S", "S")
  )
  data.table::setattr(applied, "decision_cols", decision_cols)

  combined <- combine_decision(applied, decision_table, exclusion_table,
                               reduction_table, loading_table)

  list(applied = applied, combined = combined,
       decision_table = decision_table, exclusion_table = exclusion_table,
       reduction_table = reduction_table, loading_table = loading_table,
       decision_cols = decision_cols)
}
