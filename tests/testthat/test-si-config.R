# The rulebook loader's boundary checks, and si_product()'s resolution, on inline
# fixtures. A malformed workbook must fail here with a legible message, not deep in
# the engine with a wrong number.

library(data.table)

test_that("si_product resolves windows and the coverages a product actually sells", {
  rb <- si_fixture_rulebook()
  pr <- si_product("325", rb)

  expect_equal(pr$inpatient_surgery_mon, 24L)
  expect_setequal(pr$coverages, c("life", "care"))
  # care gets its own critical-disease window; other coverages the product's
  expect_equal(pr$critical_disease_window("care"), 24L)
  expect_equal(pr$critical_disease_window("life"), 60L)
})

test_that("si_product rejects an unknown product code", {
  rb <- si_fixture_rulebook()
  expect_error(si_product("999", rb), "unknown si_type")
})

test_that("si_product rejects a duplicated product row", {
  rb <- si_fixture_rulebook()
  rb$product <- rbind(rb$product, rb$product)      # 325 twice
  expect_error(si_product("325", rb), "exactly one")
})

# --- load_si_rulebook()'s boundary checks, driven through the REAL loader: the
# fixture is written out as a workbook and read back, so a broken guard fails the
# test rather than passing a reimplementation of it. `si_write_fixture_workbook()`
# maps the fixture's sheet tables to the seven-sheet workbook the loader expects.
load_bad <- function(mutate) {
  rb   <- mutate(si_fixture_rulebook())             # each mutate returns the edited rb
  path <- tempfile(fileext = ".xlsx")
  si_write_fixture_workbook(rb, path)
  load_si_rulebook(path)
}

test_that("load_si_rulebook reads a well-formed workbook and derives its lookups", {
  rb   <- si_fixture_rulebook()
  path <- tempfile(fileext = ".xlsx")
  si_write_fixture_workbook(rb, path)
  got <- load_si_rulebook(path)

  expect_equal(got$code[["decline"]], "D")
  expect_true(got$rank[["D"]] < got$rank[["S"]])    # decline is worst
  expect_setequal(names(got$auto), c("D", "U", "S"))
})

test_that("load_si_rulebook rejects a priority that does not order decline worst", {
  expect_error(
    load_bad(function(rb) { rb$decision[, priority := c(3L, 2L, 1L)]; rb }),  # S best
    "decline < underwriter < standard")
})

test_that("load_si_rulebook rejects a decision sheet missing an engine role", {
  expect_error(
    load_bad(function(rb) { rb$decision <- rb$decision[role != "underwriter"]; rb }),
    "missing role")
})

test_that("load_si_rulebook rejects a duplicated ruleset band key", {
  expect_error(
    load_bad(function(rb) { rb$ruleset <- rbind(rb$ruleset, rb$ruleset[1L]); rb }),
    "repeats")
})

test_that("load_si_rulebook rejects a band row with a blank bound", {
  expect_error(
    load_bad(function(rb) { rb$ruleset[1L, hos_day_max := NA_integer_]; rb }),
    "blank bound")
})

test_that("load_si_rulebook rejects a reason key with no wording", {
  expect_error(
    load_bad(function(rb) { rb$reason[reason == "no_rule", reason_ko := NA_character_]; rb }),
    "no reason_ko wording")
})
