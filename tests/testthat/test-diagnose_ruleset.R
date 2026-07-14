library(data.table)

# one rule row carrying every non-decision column diagnose_ruleset() expects,
# plus two coverage columns. Override any field via `...`.
rule_row <- function(no = 1L, kcd_main = "A1", ord = 1L, decl_yn = 0L,
                     age_min = 0L, age_max = 999L,
                     elp_day_min = 0L, elp_day_max = 9999L,
                     sur_cnt_min = 0L, sur_cnt_max = 999L,
                     hos_day_min = 0L, hos_day_max = 9999L,
                     out_day_min = 0L, out_day_max = 9999L,
                     recover = "*", recur = "*", treat = "*", severe = "*", cause = "*",
                     cov1 = "S", cov2 = "S") {
  data.table(no = no, kcd_main = kcd_main, kcd_main_ko = "", n = 1L, ord = ord, decl_yn = decl_yn,
             age_min = age_min, age_max = age_max,
             elp_day_min = elp_day_min, elp_day_max = elp_day_max,
             sur_cnt_min = sur_cnt_min, sur_cnt_max = sur_cnt_max,
             hos_day_min = hos_day_min, hos_day_max = hos_day_max,
             out_day_min = out_day_min, out_day_max = out_day_max,
             recover = recover, recur = recur, treat = treat, severe = severe, cause = cause,
             medical_checkup = NA_character_, cov1 = cov1, cov2 = cov2)
}

test_that("a clean rule set reports no defect in any section", {
  rs  <- rbind(rule_row(no = 1L), rule_row(no = 2L, kcd_main = "B2", cov1 = "U"))
  res <- diagnose_ruleset(rs, verbose = FALSE)
  expect_equal(res$shadow_condition$n_row, 0L)
  expect_equal(res$latent_conflict$n_pair, 0L)
  expect_equal(res$exact_duplicate$n_group, 0L)
  expect_equal(res$no_auto_rule$n_kcd, 0L)
})

test_that("shadow_condition flags a decl_yn==0 row conditioned on an unjoined fact", {
  rs  <- rbind(rule_row(no = 1L, recover = "cured"),
               rule_row(no = 2L, kcd_main = "B2", out_day_max = 30L))
  res <- diagnose_ruleset(rs, verbose = FALSE)
  expect_equal(res$shadow_condition$n_row, 2L)
  expect_equal(res$shadow_condition$n_kcd, 2L)
  expect_equal(res$shadow_condition$by_col[["recover"]], 1L)
  expect_equal(res$shadow_condition$by_col[["out_day"]], 1L)
})

test_that("a decl_yn==1 row is ignored even when it carries a shadow condition", {
  rs  <- rbind(rule_row(no = 1L), rule_row(no = 2L, decl_yn = 1L, recover = "cured"))
  res <- diagnose_ruleset(rs, verbose = FALSE)
  expect_equal(res$shadow_condition$n_row, 0L)
})

test_that("latent_conflict finds an overlapping disagreeing pair and calls it genuine", {
  # same bands, same (wildcard) shadow cols, different decision -> structural
  rs  <- rbind(rule_row(no = 1L, cov1 = "S"), rule_row(no = 2L, ord = 2L, cov1 = "U"))
  res <- diagnose_ruleset(rs, verbose = FALSE)
  expect_equal(res$latent_conflict$n_pair, 1L)
  expect_equal(res$latent_conflict$n_genuine, 1L)
  expect_false(res$latent_conflict$pairs$shadow_explained[1])
})

test_that("latent_conflict marks a shadow-driven disagreement as not genuine", {
  # overlap + disagree, but the two rows also differ in recover -> shadow-explained
  rs  <- rbind(rule_row(no = 1L, recover = "cured", cov1 = "S"),
               rule_row(no = 2L, ord = 2L, recover = "*", cov1 = "U"))
  res <- diagnose_ruleset(rs, verbose = FALSE)
  expect_equal(res$latent_conflict$n_pair, 1L)
  expect_equal(res$latent_conflict$n_genuine, 0L)
  expect_true(res$latent_conflict$pairs$shadow_explained[1])
})

test_that("non-overlapping bands do not conflict even when decisions differ", {
  rs  <- rbind(rule_row(no = 1L, elp_day_min = 0L,   elp_day_max = 180L, cov1 = "S"),
               rule_row(no = 2L, elp_day_min = 181L, elp_day_max = 9999L, cov1 = "U"))
  res <- diagnose_ruleset(rs, verbose = FALSE)
  expect_equal(res$latent_conflict$n_pair, 0L)
})

test_that("a disagreement distinguished only by out_day is shadow-explained, not genuine", {
  # match_rule() never joins on out_day, so a constrained out_day band is a
  # shadow condition; a pair differing only there must not count as genuine.
  rs  <- rbind(rule_row(no = 1L, out_day_max = 30L,   cov1 = "S"),
               rule_row(no = 2L, ord = 2L, out_day_max = 9999L, cov1 = "U"))
  res <- diagnose_ruleset(rs, verbose = FALSE)
  expect_equal(res$latent_conflict$n_pair, 1L)
  expect_equal(res$latent_conflict$n_genuine, 0L)
  expect_true(res$latent_conflict$pairs$shadow_explained[1])
})

test_that("an NA band bound does not crash latent_conflict", {
  # an NA min/max never matches match_rule()'s non-equi join, so it cannot
  # conflict; the validator must fold the NA to no-overlap, not abort on if(NA).
  rs  <- rbind(rule_row(no = 1L, elp_day_min = NA_integer_, cov1 = "S"),
               rule_row(no = 2L, ord = 2L, cov1 = "U"))
  res <- diagnose_ruleset(rs, verbose = FALSE)
  expect_equal(res$latent_conflict$n_pair, 0L)
})

test_that("exact_duplicate counts engine-equivalent rows", {
  rs  <- rbind(rule_row(no = 1L), rule_row(no = 2L, ord = 2L))   # identical bands + decisions
  res <- diagnose_ruleset(rs, verbose = FALSE)
  expect_equal(res$exact_duplicate$n_group, 1L)
  expect_equal(res$exact_duplicate$n_extra, 1L)
})

test_that("no_auto_rule flags a kcd_main with only declaration-dependent rows", {
  rs  <- rbind(rule_row(no = 1L), rule_row(no = 2L, kcd_main = "B2", decl_yn = 1L))
  res <- diagnose_ruleset(rs, verbose = FALSE)
  expect_equal(res$no_auto_rule$n_kcd, 1L)
  expect_equal(res$no_auto_rule$kcds, "B2")
})

test_that("missing_sentinel flags sentinel codes absent from the rule set", {
  rs  <- rbind(rule_row(no = 1L), rule_row(no = 2L, kcd_main = "B2"))
  res <- diagnose_ruleset(rs, verbose = FALSE)
  expect_equal(res$missing_sentinel$n_kcd, 4L)
  expect_setequal(res$missing_sentinel$kcds, c("VACANT", "IRREGULAR", "UNMAPPED", "EXPIRED"))
})

test_that("missing_sentinel is clear when every sentinel carries a decl_yn==0 row", {
  rs <- rbind(rule_row(no = 1L, kcd_main = "VACANT"),
              rule_row(no = 2L, kcd_main = "IRREGULAR"),
              rule_row(no = 3L, kcd_main = "UNMAPPED"),
              rule_row(no = 4L, kcd_main = "EXPIRED"))
  res <- diagnose_ruleset(rs, verbose = FALSE)
  expect_equal(res$missing_sentinel$n_kcd, 0L)
})

test_that("a sentinel present only on decl_yn==1 still counts as missing", {
  rs <- rbind(rule_row(no = 1L, kcd_main = "VACANT"),
              rule_row(no = 2L, kcd_main = "IRREGULAR"),
              rule_row(no = 3L, kcd_main = "UNMAPPED"),
              rule_row(no = 4L, kcd_main = "EXPIRED", decl_yn = 1L))
  res <- diagnose_ruleset(rs, verbose = FALSE)
  expect_equal(res$missing_sentinel$n_kcd, 1L)
  expect_equal(res$missing_sentinel$kcds, "EXPIRED")
})

test_that("explicit decision_cols restricts what counts as a decision", {
  rs <- rbind(rule_row(no = 1L, cov1 = "S", cov2 = "S"),
              rule_row(no = 2L, ord = 2L, cov1 = "S", cov2 = "U"))
  # cov2 disagrees, but if only cov1 is a decision the pair no longer conflicts
  res <- diagnose_ruleset(rs, decision_cols = "cov1", verbose = FALSE)
  expect_equal(res$latent_conflict$n_pair, 0L)
})
