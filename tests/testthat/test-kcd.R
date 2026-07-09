test_that("normalize_kcd flattens and validates codes", {
  expect_equal(normalize_kcd("m00.0"), "M000")
  expect_equal(normalize_kcd("M51.13"), "M511")
  expect_equal(normalize_kcd("K63.5, S33"), "K635")   # first comma-separated code only
  expect_true(is.na(normalize_kcd("junk")))            # no digits -> invalid
  expect_true(is.na(normalize_kcd("M")))               # too short
})

test_that("split_kcd keeps every valid code in a cell", {
  expect_equal(split_kcd("K63.5, S33, junk"), c("K635", "S33"))   # junk dropped
  expect_equal(length(split_kcd("")), 0L)
})

test_that("melt_kcd melts diagnosis columns to long with a sub_kcd flag", {
  wide <- data.table::data.table(
    id = "A", kcd0 = "M511", kcd1 = "K635",
    kcd2 = NA_character_, kcd3 = NA_character_, kcd4 = NA_character_
  )
  long <- melt_kcd(wide)
  expect_setequal(long$kcd, c("M511", "K635"))
  expect_equal(long[kcd == "M511", sub_kcd], 0L)   # main diagnosis
  expect_equal(long[kcd == "K635", sub_kcd], 1L)   # sub-diagnosis
})
