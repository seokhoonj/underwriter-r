library(data.table)

# A minimal rule set: the key/band/attribute columns match_rule() treats as
# non-decision, plus two coverage columns. One rule row for kcd_main "A1".
mini_ruleset <- function(extra = NULL) {
  rs <- data.table(
    no = 1L, kcd_main = "A1", kcd_main_ko = "", n = 1L, ord = 1L, decl_yn = 0L,
    age_min = 0L, age_max = 999L, elp_day_min = 0L, elp_day_max = 9999L,
    sur_cnt_min = 0L, sur_cnt_max = 999L, hos_day_min = 0L, hos_day_max = 9999L,
    out_day_min = 0L, out_day_max = 9999L,
    recover = "*", recur = "*", treat = "*", severe = "*", cause = "*",
    medical_checkup = NA_character_,
    cov1 = "S", cov2 = "U")
  if (!is.null(extra)) rs[, (extra) := "note"]
  rs[]
}

input <- function() data.table(id = "X", kcd_main = "A1", age = 40L, elp_day = 0L,
                               sur_cnt = 0L, hos_day = 0L)

# two overlapping rules for the same kcd_main so one input matches both; the second
# rule's decisions are the argument -- equal to rule 1 (a duplicate, no conflict) or
# different (a genuine conflict).
two_rule_ruleset <- function(cov1_2 = "S", cov2_2 = "U") {
  r1 <- mini_ruleset()
  r2 <- copy(r1)[, `:=`(no = 2L, ord = 2L, cov1 = cov1_2, cov2 = cov2_2)]
  rbind(r1, r2)
}

test_that("match_rule infers the coverage columns and matches", {
  m <- match_rule(input(), mini_ruleset())
  expect_setequal(m$decision_cols, c("cov1", "cov2"))
  expect_equal(m$applied$matched, 1L)
  expect_equal(m$applied$cov1, "S")
  expect_equal(m$applied$cov2, "U")
})

test_that("an extra attribute column becomes a phantom coverage under the default", {
  m <- match_rule(input(), mini_ruleset(extra = "my_note"))
  expect_true("my_note" %in% m$decision_cols)          # the documented fragility
})

test_that("passing decision_cols explicitly keeps the extra column out", {
  m <- match_rule(input(), mini_ruleset(extra = "my_note"),
                  decision_cols = c("cov1", "cov2"))
  expect_setequal(m$decision_cols, c("cov1", "cov2"))
  expect_false("my_note" %in% m$decision_cols)
})

test_that("decision_cols not present in the rule set are ignored", {
  m <- match_rule(input(), mini_ruleset(), decision_cols = c("cov1", "cov2", "ghost"))
  expect_setequal(m$decision_cols, c("cov1", "cov2"))
})

test_that("identical overlapping rules multi-match but do not conflict", {
  m <- match_rule(input(), two_rule_ruleset(cov1_2 = "S", cov2_2 = "U"))
  expect_equal(m$n_multi_matched, 1L)
  expect_equal(m$n_conflict, 0L)
  expect_false(m$applied$conflict)
  expect_equal(m$applied$cov1, "S")                    # lowest-ord match kept
})

test_that("overlapping rules that disagree on a decision are flagged as a conflict", {
  m <- match_rule(input(), two_rule_ruleset(cov1_2 = "D", cov2_2 = "U"))
  expect_equal(m$n_multi_matched, 1L)
  expect_equal(m$n_conflict, 1L)
  expect_true(m$applied$conflict)
  expect_equal(nrow(m$conflict), 2L)                   # both overlapping rules listed
})
