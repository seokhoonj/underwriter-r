test_that("combine_ruleset appends sentinel below and continues the numbering", {
  rs  <- data.table(no = 1:3, kcd_main = c("A", "B", "C"))
  sen <- data.table(kcd_main = c("VACANT", "EXPIRED"))
  out <- combine_ruleset(rs, sen)
  expect_equal(nrow(out), 5L)
  expect_equal(out$no, c(1, 2, 3, 4, 5))
  expect_equal(out$kcd_main, c("A", "B", "C", "VACANT", "EXPIRED"))
  expect_equal(anyDuplicated(out$no), 0L)
})

test_that("combine_ruleset numbers sentinels after the max no, not the row count", {
  rs  <- data.table(no = c(10, 25, 4), kcd_main = c("A", "B", "C"))
  sen <- data.table(kcd_main = "VACANT")
  out <- combine_ruleset(rs, sen)
  expect_equal(out[kcd_main == "VACANT", no], 26)   # max(25) + 1, not count(3) + 1
})

test_that("combine_ruleset overwrites a stale no on the sentinel rows", {
  rs  <- data.table(no = 1:2, kcd_main = c("A", "B"))
  sen <- data.table(no = c(1, 2), kcd_main = c("VACANT", "EXPIRED"))  # colliding no
  out <- combine_ruleset(rs, sen)
  expect_equal(out[kcd_main %in% c("VACANT", "EXPIRED"), no], c(3, 4))
  expect_equal(anyDuplicated(out$no), 0L)
})

test_that("combine_ruleset keeps no as the first column and preserves others", {
  rs  <- data.table(no = 1L, kcd_main = "A", cov1 = "S")
  sen <- data.table(kcd_main = "VACANT", cov1 = "S")
  out <- combine_ruleset(rs, sen)
  expect_equal(names(out)[1], "no")
  expect_equal(names(out), c("no", "kcd_main", "cov1"))
  expect_equal(nrow(out), 2L)
})

test_that("combine_ruleset returns the ruleset unchanged when there is no sentinel", {
  rs  <- data.table(no = 1:2, kcd_main = c("A", "B"))
  out <- combine_ruleset(rs, sen = rs[0])
  expect_equal(out$no, c(1, 2))
  expect_equal(nrow(out), 2L)
})
