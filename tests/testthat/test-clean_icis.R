test_that("clean_icis(method = 'auto') does not crash on NA hos_day", {
  dt <- data.table::data.table(
    id = c("A", "B"), gender = c("1", "2"), age = c(40L, 50L),
    inq_date = c("20240301", "20240301"), pay_date = c("20240201", "20240201"),
    acc_date = c("20240110", "20240110"), sdate = c("20240115", "20240115"),
    edate = c("20240120", NA), hos_day = c(5L, NA), hos_cnt = c(1L, 1L),
    sur_cnt = c(0L, 0L), kcd0 = c("M511", "K635"),
    kcd1 = NA_character_, kcd2 = NA, kcd3 = NA, kcd4 = NA
  )
  expect_no_error(out <- clean_icis(dt, method = "auto"))
  expect_equal(nrow(out), 2L)
})

test_that("clean_icis keeps only rows carrying a diagnosis code", {
  dt <- data.table::data.table(
    id = c("A", "B"), gender = c("1", "1"), age = c(40L, 40L),
    inq_date = "20240301", pay_date = "20240201", acc_date = "20240110",
    sdate = "20240115", edate = NA, hos_day = 0L, hos_cnt = 0L, sur_cnt = 0L,
    kcd0 = c("M511", NA), kcd1 = NA_character_, kcd2 = NA, kcd3 = NA, kcd4 = NA
  )
  out <- clean_icis(dt)
  expect_equal(out$id, "A")   # B has no code -> dropped
})
