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
  tables <- compose_tables(data.table::data.table(decision = c("E", "D"), lower = c(0L, 201L)))
  # exclusion unions the sites, loading sums the indices, reduction keeps the
  # period; standard drops out; the result is written in decision-table row order
  expect_equal(compose_one(c("R03(3)", "R12(3)", "E(50)", "E(25)", "L(3)", "S"), tables = tables),
               "E(75),L(36),R03(36),R12(36)")
})

test_that("a loading band carries the summed index only when it is the bare letter", {
  bare    <- compose_tables(data.table::data.table(decision = c("E", "D"), lower = c(0L, 201L)))
  literal <- compose_tables(data.table::data.table(decision = c("E(0)", "E(50)"), lower = c(0L, 50L)))
  expect_equal(compose_one(c("E(50)", "E(25)"), tables = bare), "E(75)")
  expect_equal(compose_one(c("E(50)", "E(25)"), tables = literal), "E(50)")
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
  # so does an exclusion spanning more sites than max_sites
  expect_equal(compose_one(c("R03(3)", "R12(3)"), max_sites = 1L), "D")
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
    combine_decision(f$applied, dec, f$exclusion_table, f$reduction_table, f$loading_table),
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
