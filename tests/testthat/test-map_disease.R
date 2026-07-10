library(data.table)

# One melted claim row per diagnosis code, carrying the columns map_disease() reads
# plus the ones aggregate_disease() goes on to need. `id` lets a test build a person
# out of several lines.
melted_row <- function(kcd, treated = "2025-01-01", inq_date = "2025-06-01", id = "A") {
  data.table(id = id, kcd = kcd, sub_kcd = 0L, age = 40L,
             acc_date = as.Date(treated), sdate = as.Date(treated), edate = as.Date(NA),
             inq_date = as.Date(inq_date), hos_day = 0L, sur_cnt = 0L)
}

# A disease table that knows one real code and none of the reserved codes
disease <- data.table(kcd = "M543", kcd_main = "M543", sub_chk = 0L, lookback_mon = 12L)

test_that("a real code maps to its representative disease and its own window", {
  out <- map_disease(melted_row("M543"), disease)
  expect_equal(out$kcd_main, "M543")
  expect_equal(out$in_lookback, 1L)                            # treated 5 months before inquiry
  aged <- map_disease(melted_row("M543", treated = "2023-01-01"), disease)
  expect_equal(aged$in_lookback, 0L)                           # 29 months, past the 12-month window
})

test_that("a valid code the table does not cover becomes UNMAPPED", {
  out <- map_disease(melted_row("K635"), disease)
  expect_equal(out$kcd_main, "UNMAPPED")
  expect_equal(out$sub_chk, 1L)
  expect_true(is.na(out$in_lookback))    # no lookback is defined for it
  expect_equal(out$in_5yr, 1L)           # the fixed window still scopes it
})

test_that("the code-slot reserved codes carry through, VACANT always in window", {
  # VACANT and IRREGULAR come from clean_icis(); map_disease() must not look them up
  vac <- map_disease(melted_row("VACANT"), disease)
  expect_equal(vac$kcd_main, "VACANT")
  expect_equal(vac$review, 1L)
  expect_equal(vac$in_lookback, 1L)      # nothing was diagnosed, so nothing can age out

  irr <- map_disease(melted_row("IRREGULAR"), disease)
  expect_equal(irr$kcd_main, "IRREGULAR")
  expect_true(is.na(irr$in_lookback))    # reviewed via the 5-year window, like UNMAPPED
})

test_that("VACANT is in its window even with no treatment date at all", {
  undated <- melted_row("VACANT")[, `:=`(acc_date = as.Date(NA), sdate = as.Date(NA))]
  out <- map_disease(undated, disease)
  expect_equal(out$kcd_main, "VACANT")
  expect_equal(out$in_lookback, 1L)
})

test_that("a disease table row for a reserved code is ignored", {
  meddling <- rbind(disease, data.table(kcd = "VACANT", kcd_main = "M543",
                                        sub_chk = 0L, lookback_mon = 1L))
  out <- map_disease(melted_row("VACANT"), meddling)
  expect_equal(out$kcd_main, "VACANT")
  expect_equal(out$in_lookback, 1L)
})

test_that("aggregate_disease keeps a codeless-only insured on VACANT", {
  agg <- aggregate_disease(map_disease(melted_row("VACANT"), disease))
  expect_equal(nrow(agg), 1L)
  expect_equal(agg$id, "A")
  expect_equal(agg$kcd_main, "VACANT")
})

test_that("a VACANT line drops out for an insured who also has a real diagnosis", {
  mapped <- map_disease(rbind(melted_row("VACANT"), melted_row("M543")), disease)
  agg <- aggregate_disease(mapped)
  expect_equal(agg$kcd_main, "M543")     # the codeless line marks the line, not the insured
})

test_that("an insured whose every diagnosis expired, with no VACANT line, becomes EXPIRED", {
  # two real diagnoses, both treated long ago -> both past the 12-month window
  old <- rbind(melted_row("M543", treated = "2022-01-01"),
               melted_row("M543", treated = "2021-06-01"))
  agg <- aggregate_disease(map_disease(old, disease))
  expect_equal(nrow(agg), 1L)            # several expired diagnoses fold to one row
  expect_equal(agg$kcd_main, "EXPIRED")
  expect_equal(agg$hos_day, 0L)          # placeholder: counts are 0
  expect_false(is.na(agg$elp_day))       # elp_day is real (days since most recent treatment)
})

test_that("an expired diagnosis is simply dropped when another is in scope", {
  # one in-window (recent) and one expired: the person is decided on the live one,
  # the expired one leaves no trace and no EXPIRED row appears
  mix <- rbind(melted_row("M543", treated = "2025-01-01"),
               melted_row("M543", treated = "2022-01-01"))
  agg <- aggregate_disease(map_disease(mix, disease))
  expect_equal(agg$kcd_main, "M543")
  expect_false("EXPIRED" %in% agg$kcd_main)
})
