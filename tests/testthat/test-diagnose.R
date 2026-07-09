test_that("diagnose_icis errors on empty input", {
  expect_error(diagnose_icis(data.table::data.table(id = character())), "no rows")
})
