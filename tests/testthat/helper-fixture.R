# A tiny synthetic underwriting fixture (no real claim data). Two coverages,
# a handful of insured, a decision-code table, and a per-disease `applied` table
# that produces a known `combined`. Used across the decision/relaxation tests.
#
# `applied` layout (id x kcd_main, one row each), decision on cov1/cov2:
#   A: M543 (cov1=U, cov2=S), M542 (cov1=S, cov2=U)   -> cov1=U (M543 sole), cov2=U (M542 sole)
#   B: M543 (cov1=U, cov2=D)                          -> cov1=U (M543 sole), cov2=D
#   C: N50  unmatched -> underwriter everywhere      -> cov1=U (N50 sole),  cov2=U (N50 sole)
#   D: M543 (cov1=U), M542 (cov1=U)  co-hold cov1      -> cov1=U (two causes -> synergy), cov2=S

# A decision table of the shape a company actually writes: two terminal codes
# (decline, underwriter), an identity (standard), two restrictions with no merge
# rule of their own (`C`, `M`, hence the `priority` combiner), and the three
# classes that merge by exclusion / loading / reduction. Used to test how the
# classes meet each other on one coverage.
compose_tables <- function(band_table = data.table::data.table(
                             class    = c("E", "E", "E", "R", "R"),
                             at_least = c(0L, 50L, 201L, 1L, 5L),
                             decision = c("S", "U", "D",  "R", "D"))) {
  list(
    decision_table = data.table::data.table(
      priority = c(1L, 2L, 3L, 4L, 5L, 5L, 5L, 6L),
      code     = c("D", "U", "C", "M", "E", "L", "R", "S"),
      combiner = c("priority", "priority", "priority", "priority",
                   "loading", "reduction", "exclusion", "priority"),
      role     = c("decline", "underwriter", NA, NA, NA, NA, NA, "standard"),
      auto     = c(1L, 0L, 1L, 1L, 1L, 1L, 1L, 1L)),
    exclusion_table = data.table::data.table(mark = c("1i", "3", "99")),
    reduction_table = data.table::data.table(mark = c("3", "99")),
    band_table      = band_table
  )
}

# The default bands with one class's staircase swapped out, so a test can rewrite
# just the exclusion or just the loading side. The argument is `letter`, not
# `class`: data.table would read a bare `class` in `i` as the column, not the
# argument, and quietly drop every row.
compose_bands <- function(letter, at_least, decision) {
  keep <- compose_tables()$band_table
  data.table::rbindlist(list(keep[keep$class != letter],
                             data.table::data.table(class = letter, at_least = at_least,
                                                    decision = decision)))
}

# One insured, one coverage: each element of `codes` is a separate disease's rule
# decision. Returns that coverage's final decision.
compose_one <- function(codes, elp_day = 0L, tables = compose_tables()) {
  applied <- data.table::data.table(
    id       = "X",
    kcd_main = paste0("K", seq_along(codes)),
    elp_day  = as.integer(elp_day),
    matched  = 1L,
    cov1     = codes
  )
  data.table::setattr(applied, "decision_cols", "cov1")
  combine_decision(applied, tables$decision_table, tables$exclusion_table,
                   tables$reduction_table, tables$band_table)$cov1
}

fixture <- function() {
  decision_cols <- c("cov1", "cov2")

  decision_table <- data.table::data.table(
    code     = c("S", "U", "D", "R", "E", "L"),
    priority = c(5L, 2L, 1L, 3L, 4L, 4L),
    combiner = c("priority", "priority", "priority", "exclusion", "loading", "reduction"),
    role     = c("standard", "underwriter", "decline", NA, NA, NA),
    auto     = c(1L, 0L, 1L, 1L, 1L, 1L)
  )
  exclusion_table <- data.table::data.table(mark = c("5i", "3", "99"))
  reduction_table <- data.table::data.table(mark = c("5i", "3", "99"))
  band_table      <- data.table::data.table(
    class    = c("E",    "E",     "E",     "R", "R"),
    at_least = c(0L,     25L,     50L,     1L,  5L),
    decision = c("E(0)", "E(25)", "E(50)", "R", "D"))

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
                               reduction_table, band_table)

  list(applied = applied, combined = combined,
       decision_table = decision_table, exclusion_table = exclusion_table,
       reduction_table = reduction_table, band_table = band_table,
       decision_cols = decision_cols)
}
