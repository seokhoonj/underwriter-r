library(data.table)

# One melted claim row per diagnosis code, carrying the columns map_disease() reads
# plus the ones aggregate_disease() goes on to need.
melted_row <- function(kcd, treated = "2025-01-01", inq_date = "2025-06-01") {
  data.table(id = "A", kcd = kcd, sub_kcd = 0L, age = 40L,
             acc_date = as.Date(treated), sdate = as.Date(treated), edate = as.Date(NA),
             inq_date = as.Date(inq_date), hos_day = 0L, sur_cnt = 0L)
}

# A disease table that knows one real code and neither reserved code
disease <- data.table(kcd = "M543", kcd_main = "M543", sub_chk = 0L, lookback_mon = 12L)

test_that("a real code maps to its representative disease and its own window", {
  out <- map_disease(melted_row("M543"), disease)
  expect_equal(out$kcd_main, "M543")
  expect_equal(out$in_lookback, 1L)                            # treated 5 months before inquiry
  aged <- map_disease(melted_row("M543", treated = "2023-01-01"), disease)
  expect_equal(aged$in_lookback, 0L)                           # 29 months, past the 12-month window
})

test_that("a code the table does not cover becomes the unmapped code", {
  out <- map_disease(melted_row("K635"), disease)
  expect_equal(out$kcd_main, "ZZZ")
  expect_equal(out$sub_chk, 1L)
  expect_true(is.na(out$in_lookback))    # no lookback is defined for it
  expect_equal(out$in_5yr, 1L)           # the fixed window still scopes it
})

test_that("the no-diagnosis code survives a disease table that never mentions it", {
  # the guarantee lives in the code, not in a spreadsheet row: without this the exact
  # match fails, the 3-character fallback fails, and an insured with nothing to
  # underwrite becomes an unmapped diagnosis and is referred
  out <- map_disease(melted_row("AAA"), disease)
  expect_equal(out$kcd_main, "AAA")
  expect_equal(out$review, 1L)
  expect_equal(out$in_lookback, 1L)      # nothing was diagnosed, so nothing aged out
})

test_that("the no-diagnosis code is in its window even with no treatment date at all", {
  undated <- melted_row("AAA")[, `:=`(acc_date = as.Date(NA), sdate = as.Date(NA))]
  out <- map_disease(undated, disease)
  expect_equal(out$kcd_main, "AAA")
  expect_equal(out$in_lookback, 1L)
})

test_that("a disease table row for a reserved code is ignored", {
  meddling <- rbind(disease, data.table(kcd = "AAA", kcd_main = "M543",
                                        sub_chk = 0L, lookback_mon = 1L))
  out <- map_disease(melted_row("AAA"), meddling)
  expect_equal(out$kcd_main, "AAA")
  expect_equal(out$in_lookback, 1L)
})

test_that("aggregate_disease keeps an insured whose only row is the no-diagnosis code", {
  agg <- aggregate_disease(map_disease(melted_row("AAA"), disease))
  expect_equal(nrow(agg), 1L)
  expect_equal(agg$id, "A")
  expect_equal(agg$kcd_main, "AAA")
})

test_that("the no-diagnosis code drops out for an insured who has a real diagnosis", {
  mapped <- map_disease(rbind(melted_row("AAA"), melted_row("M543")), disease)
  agg <- aggregate_disease(mapped)
  expect_equal(agg$kcd_main, "M543")     # a codeless line marks the line, not the insured
})
