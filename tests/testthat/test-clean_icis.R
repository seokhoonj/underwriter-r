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

test_that("clean_icis marks an empty-cell row VACANT and does not lose it", {
  dt <- data.table::data.table(
    id = c("A", "B"), gender = c("1", "1"), age = c(40L, 40L),
    inq_date = "20240301", pay_date = "20240201", acc_date = "20240110",
    sdate = "20240115", edate = NA, hos_day = 0L, hos_cnt = 0L, sur_cnt = 0L,
    kcd0 = c("M511", NA), kcd1 = NA_character_, kcd2 = NA, kcd3 = NA, kcd4 = NA
  )
  out <- clean_icis(dt)
  expect_equal(out$id, c("A", "B"))   # B has no code, but B is not lost
  expect_equal(out[id == "B", kcd0], "VACANT")
})

test_that("clean_icis keeps every code from a multi-code cell when a kcd column is all-NA", {
  # kcd4 arrives all-NA, hence logical; without the up-front character coercion,
  # redistributing five comma-separated codes into it would coerce the fifth to NA
  dt <- data.table::data.table(
    id = "x", gender = "1", age = 40L,
    inq_date = "20250601", pay_date = "20250201", acc_date = "20250101",
    sdate = "20250101", edate = NA_character_, hos_day = 0L, hos_cnt = 0L, sur_cnt = 0L,
    kcd0 = "M54.3, K63.5, S33, J06.9, A09",
    kcd1 = NA_character_, kcd2 = NA_character_, kcd3 = NA_character_, kcd4 = NA
  )
  expect_silent(out <- clean_icis(dt))              # no coercion warning
  codes <- unlist(out[, paste0("kcd", 0:4), with = FALSE], use.names = FALSE)
  expect_true(all(c("M543", "K635", "S33", "J069", "A09") %in% codes))
})

test_that("clean_icis marks an unreadable code IRREGULAR, not a pass", {
  dt <- data.table::data.table(
    id = "C", gender = "1", age = 40L,
    inq_date = "20240301", pay_date = "20240201", acc_date = "20240110",
    sdate = "20240115", edate = NA, hos_day = 0L, hos_cnt = 0L, sur_cnt = 0L,
    kcd0 = "1234", kcd1 = NA_character_, kcd2 = NA, kcd3 = NA, kcd4 = NA
  )
  out <- clean_icis(dt)                  # a code arrived, but none parses to KCD shape
  expect_equal(out$kcd0, "IRREGULAR")    # not VACANT: a parse failure must not auto-pass
})
