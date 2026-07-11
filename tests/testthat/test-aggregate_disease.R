library(data.table)

# A mapped row carrying exactly the columns aggregate_disease() reads. `elapsed` is
# controlled by dating acc/sdate/edate at `inq - elapsed`, so the elp_day arithmetic
# is exact. Defaults describe an in-scope, reviewed, outpatient line.
inq <- as.Date("2024-03-24")
mrow <- function(id, kcd_main, elapsed, review = 1L, in_lookback = 1L, in_5yr = 1L,
                 hos_day = 0L, sur_cnt = 0L, age = 40L) {
  treated <- inq - elapsed
  data.table(id = id, kcd_main = kcd_main, review = as.integer(review),
             in_lookback = as.integer(in_lookback), in_5yr = as.integer(in_5yr),
             age = as.integer(age), hos_day = as.integer(hos_day), sur_cnt = as.integer(sur_cnt),
             acc_date = treated, sdate = treated, edate = treated, inq_date = inq)
}

test_that("in-scope elp_day is the minimum elapsed across all treatment types", {
  # one insured, one disease, three treatment types: hos 30, sur 10, out 50
  mapped <- rbind(
    mrow("P1", "M510", 30L, hos_day = 3L),
    mrow("P1", "M510", 10L, sur_cnt = 1L),
    mrow("P1", "M510", 50L)
  )
  res <- aggregate_disease(mapped)
  expect_equal(res[id == "P1", elp_day], 10L)                 # pmin(30, 10, 50)
})

test_that("an aged-out insured is carried on EXPIRED with reviewed-only elp_day", {
  # every reviewed diagnosis out of scope (min reviewed elapsed 400); one NON-reviewed
  # line at 100 must NOT pull elp_day forward
  mapped <- rbind(
    mrow("P2", "M540", 400L, in_lookback = 0L, in_5yr = 0L),
    mrow("P2", "K400", 800L, in_lookback = 0L, in_5yr = 0L),
    mrow("P2", "L020", 100L, review = 0L, in_lookback = 0L, in_5yr = 0L)
  )
  res <- aggregate_disease(mapped)
  expect_equal(nrow(res[id == "P2"]), 1L)
  expect_equal(res[id == "P2", kcd_main], "EXPIRED")
  expect_equal(res[id == "P2", elp_day], 400L)               # reviewed min, not the 100 review==0 line
})

test_that("an insured with no reviewed line at all still survives with elp_day NA", {
  # defensive case: no review == 1 row anywhere -> keep the id, elp_day falls to NA
  mapped <- rbind(
    mrow("P3", "R000", 70L, review = 0L, in_lookback = 0L, in_5yr = 0L),
    mrow("P3", "R001", 90L, review = 0L, in_lookback = 0L, in_5yr = 0L)
  )
  res <- aggregate_disease(mapped)
  expect_equal(nrow(res[id == "P3"]), 1L)
  expect_equal(res[id == "P3", kcd_main], "EXPIRED")
  expect_true(is.na(res[id == "P3", elp_day]))
})

test_that("every id in mapped leaves with a row (no insured left behind)", {
  mapped <- rbind(
    mrow("P1", "M510", 10L, sur_cnt = 1L),
    mrow("P2", "M540", 400L, in_lookback = 0L, in_5yr = 0L),
    mrow("P3", "R000", 70L, review = 0L, in_lookback = 0L, in_5yr = 0L)
  )
  res <- aggregate_disease(mapped)
  expect_setequal(unique(res$id), c("P1", "P2", "P3"))
})
