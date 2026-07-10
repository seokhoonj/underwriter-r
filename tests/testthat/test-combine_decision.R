test_that("combine_decision produces the expected per-insured decisions", {
  f <- fixture()
  combined <- f$combined
  expect_s3_class(combined, "combined_decision")
  expect_setequal(combined$id, c("A", "B", "C", "D"))
  expect_true(all(c("cov1", "cov2") %in% names(combined)))
  g <- function(i, cov) combined[id == i][[cov]]
  expect_equal(g("A", "cov1"), "U")   # M543 U wins over M542 S (worst code)
  expect_equal(g("A", "cov2"), "U")   # M542 U wins over M543 S
  expect_equal(g("B", "cov2"), "D")   # decline wins (lowest priority)
  expect_equal(g("C", "cov1"), "U")   # unmatched -> underwriter
  expect_equal(g("D", "cov1"), "U")   # co-held U
  expect_equal(g("D", "cov2"), "S")   # both standard
})

test_that("restrictions of different classes compose into one decision", {
  tables <- compose_tables(compose_bands("E", at_least = c(0L, 201L), decision = c("E", "D")))
  # exclusion unions the sites, loading sums the indices, reduction keeps the
  # period; standard drops out; the result is written in decision-table row order
  expect_equal(compose_one(c("R03(3)", "R12(3)", "E(50)", "E(25)", "L(3)", "S"), tables = tables),
               "E(75),L(36),R03(36),R12(36)")
})

test_that("a band carries the class's own output only when its decision is the bare letter", {
  bare    <- compose_tables(compose_bands("E", at_least = c(0L, 201L),  decision = c("E", "D")))
  literal <- compose_tables(compose_bands("E", at_least = c(0L, 50L),   decision = c("E(0)", "E(50)")))
  expect_equal(compose_one(c("E(50)", "E(25)"), tables = bare), "E(75)")     # sum carried in
  expect_equal(compose_one(c("E(50)", "E(25)"), tables = literal), "E(50)")  # band substituted
})

test_that("a band's at_least bound is inclusive", {
  tables <- compose_tables(compose_bands("E", at_least = c(0L, 50L), decision = c("E", "D")))
  expect_equal(compose_one("E(49)", tables = tables), "E(49)")
  expect_equal(compose_one("E(50)", tables = tables), "D")     # 50 lands in the D band
})

test_that("decline and underwriter are terminal and suppress every restriction", {
  expect_equal(compose_one(c("D", "R03(3)", "E(25)")), "D")
  expect_equal(compose_one(c("U", "R03(3)")), "U")
  expect_equal(compose_one(c("D", "U", "R03(3)")), "D")   # the worse terminal wins
})

test_that("standard is the identity and drops out of a composed decision", {
  expect_equal(compose_one(c("S", "R03(3)")), "R03(36)")
  expect_equal(compose_one(c("S", "S")), "S")
})

test_that("terminality is judged on the combiner output, not the input code", {
  # a summed loading that reaches the underwriter / decline band escalates a cell
  # holding nothing but restriction codes
  expect_equal(compose_one(c("E(50)", "E(25)", "R03(3)")), "U")
  expect_equal(compose_one(c("E(150)", "E(51)", "R03(3)")), "D")
  # so does an exclusion whose distinct site count reaches its decline band
  expect_equal(compose_one(c("R03(3)", "R12(3)", "R24(3)", "R31(3)", "R42(3)")), "D")
})

test_that("the exclusion bands on its distinct site count", {
  # four sites sit under the default decline band at five, and R31 repeats a site
  expect_equal(compose_one(c("R03(3)", "R12(3)", "R24(3)", "R31(3)", "R31(99)")),
               "R03(36),R12(36),R24(36),R31(99)")
  # a rule set can refer before it declines
  graded <- compose_tables(compose_bands("R", at_least = c(1L, 3L, 5L),
                                              decision = c("R", "U", "D")))
  expect_equal(compose_one("R03(3)", tables = graded), "R03(36)")
  expect_equal(compose_one(c("R03(3)", "R12(3)", "R24(3)"), tables = graded), "U")
  expect_equal(compose_one(c("R03(3)", "R12(3)", "R24(3)", "R31(3)", "R42(3)"), tables = graded), "D")
  # an expired site does not count toward the band
  tight <- compose_tables(compose_bands("R", at_least = c(1L, 2L), decision = c("R", "D")))
  expect_equal(compose_one(c("R03(3)", "R12(1i)"), elp_day = 400L, tables = tight), "R03(36)")
})

test_that("combine_decision rejects a band_table missing an accumulating class", {
  tables <- compose_tables()
  only_e <- tables$band_table[tables$band_table$class == "E"]
  applied <- data.table::data.table(id = "X", kcd_main = "K1", elp_day = 0L,
                                    matched = 1L, cov1 = "R03(3)")
  data.table::setattr(applied, "decision_cols", "cov1")
  expect_error(combine_decision(applied, tables$decision_table, tables$exclusion_table,
                                tables$reduction_table, only_e),
               "no rows for class \"R\"")
})

test_that("an expired exclusion leaves the coverage standard", {
  expect_equal(compose_one("R01(1i)", elp_day = 400L), "S")   # 1 year, 13 months elapsed
})

test_that("a priority-merged restriction composes with the merging classes", {
  expect_equal(compose_one(c("M(3)", "R03(3)")), "M(3),R03(36)")
  # C and M share the priority combiner, so they stay exclusive of each other
  expect_equal(compose_one(c("C(1)", "M(3)", "R03(3)")), "C(1),R03(36)")
})

test_that("combine_decision errors without an underwriter role", {
  f <- fixture()
  dec <- data.table::copy(f$decision_table)
  dec[role == "underwriter", role := NA]
  expect_error(
    combine_decision(f$applied, dec, f$exclusion_table, f$reduction_table, f$band_table),
    "underwriter"
  )
})

test_that("combine_decision decides every id in applied", {
  f <- fixture()
  expect_setequal(f$combined$id, unique(f$applied$id))
})

test_that("tabulate_decision flags auto vs referred and sums to 1 per coverage", {
  f <- fixture()
  tab <- tabulate_decision(f$combined)
  expect_true(all(c("coverage", "decision", "category", "auto", "n", "prop") %in% names(tab)))
  expect_equal(as.character(tab[decision == "U", unique(auto)]), "0")
  expect_equal(as.character(tab[decision == "D", unique(auto)]), "1")
  expect_equal(tab[, sum(prop), by = coverage]$V1, c(1, 1))
})

test_that("tabulate_decision handles a logical auto column", {
  f <- fixture()
  combined <- data.table::copy(f$combined)
  dec <- data.table::copy(f$decision_table)
  dec[, auto := auto == 1L]                    # logical TRUE/FALSE
  data.table::setattr(combined, "decision_table", dec)
  tab <- tabulate_decision(combined)
  expect_false(anyNA(tab$auto))                # not coerced to NA through as.character
  expect_equal(as.character(tab[decision == "U", unique(auto)]), "0")
})

test_that("combine_decision carries band_table so a recombine reproduces the table", {
  tables <- compose_tables(compose_bands("R", at_least = c(1L, 2L), decision = c("R", "D")))
  applied <- data.table::data.table(id = "X", kcd_main = c("K1", "K2"), elp_day = 0L,
                                    matched = 1L, cov1 = c("R03(3)", "R12(3)"))
  data.table::setattr(applied, "decision_cols", "cov1")
  tight <- combine_decision(applied, tables$decision_table, tables$exclusion_table,
                            tables$reduction_table, tables$band_table)
  expect_equal(tight$cov1, "D")   # two sites reach the decline band at two
  expect_equal(attr(tight, "band_table"), tables$band_table)

  # trace and relax recombine under the recorded bands, not some default of their own
  tr <- trace_decision(applied, tight, "X")
  expect_equal(tr$computed, "D")
  expect_true(all(tr$ok))
  expect_equal(relax_rule(applied, tight, "ZZZZ")$auto_base, 1)   # D is auto here
})

test_that("trace_decision reproduces a normal id", {
  f <- fixture()
  tr <- trace_decision(f$applied, f$combined, "A")
  expect_true(all(c("coverage", "diseases", "computed", "stored", "ok") %in% names(tr)))
  expect_true(all(tr$ok))
})

test_that("trace_decision rejects an id that combined has but applied does not", {
  f <- fixture()
  combined <- data.table::copy(f$combined)
  expect_error(trace_decision(f$applied[id != "A"], combined, "A"), "absent from")
})
