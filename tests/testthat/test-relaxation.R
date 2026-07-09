test_that("list_rule_impact is per-coverage with sole-source marginals", {
  f <- fixture()
  li <- list_rule_impact(f$applied, f$final)
  expect_s3_class(li, "rule_impact_list")
  expect_equal(names(li), c("coverage", "kcd_main", "n_id", "n_flipped", "auto_lift"))
  n <- function(cov, k) {
    r <- li[coverage == cov & kcd_main == k]
    if (nrow(r)) r$n_flipped else 0L
  }
  # cov1: M543 sole on A, B (D is co-held -> credited to neither); N50 sole on C
  expect_equal(n("cov1", "M543"), 2L)
  expect_equal(n("cov1", "M542"), 0L)
  expect_equal(n("cov1", "N50"),  1L)
  # cov2: M542 sole on A; N50 sole on C
  expect_equal(n("cov2", "M542"), 1L)
  expect_equal(n("cov2", "N50"),  1L)
})

test_that("list_rule_impact coverage filter restricts to the given coverage", {
  f <- fixture()
  li <- list_rule_impact(f$applied, f$final, coverage = "cov1")
  expect_equal(unique(li$coverage), "cov1")
})

test_that("relax_rule flips the sole-source cells and only ever rises", {
  f <- fixture()
  rr <- relax_rule(f$applied, f$final, "M543")
  expect_s3_class(rr, "relaxed_rule")
  expect_equal(names(rr), c("coverage", "auto_base", "auto_relaxed", "lift", "n_flipped"))
  fl <- function(cov) rr[coverage == cov, n_flipped]
  expect_equal(fl("cov1"), 2L)   # A, B flip; D still held by M542
  expect_equal(fl("cov2"), 0L)
  expect_true(all(rr$lift >= 0))
})

test_that("relax_rule coverage filter returns only that coverage", {
  f <- fixture()
  rr <- relax_rule(f$applied, f$final, "M543", coverage = "cov1")
  expect_equal(rr$coverage, "cov1")
  expect_equal(nrow(rr), 1L)
})

test_that("decompose_rule_impact splits into individual/combined/synergy per coverage", {
  f <- fixture()
  dc <- decompose_rule_impact(f$applied, f$final, c("M543", "M542"))
  expect_equal(names(dc), c("coverage", "component", "n_flipped", "auto_lift"))
  g <- function(cov, comp) dc[coverage == cov & component == comp, n_flipped]
  # cov1: individual = M543(2) + M542(0); combined flips A, B, D = 3; synergy = 1 (D co-held)
  expect_equal(g("cov1", "individual"), 2)
  expect_equal(g("cov1", "combined"),   3)
  expect_equal(g("cov1", "synergy"),    1)
  # synergy = combined - individual on every coverage
  w <- data.table::dcast(dc, coverage ~ component, value.var = "n_flipped")
  expect_equal(w$synergy, w$combined - w$individual)
})

test_that("decompose_rule_impact combined equals relax_rule on the whole set", {
  f <- fixture()
  dc <- decompose_rule_impact(f$applied, f$final, c("M543", "M542"))
  rr <- relax_rule(f$applied, f$final, c("M543", "M542"))
  from_decomp <- dc[component == "combined"][order(coverage)]
  from_relax  <- rr[, .(n_flipped = sum(n_flipped)), by = coverage][order(coverage)]
  expect_equal(from_decomp$n_flipped, from_relax$n_flipped)
})

test_that("decompose_rule_impact needs at least two codes", {
  f <- fixture()
  expect_error(decompose_rule_impact(f$applied, f$final, "M543"), "at least two")
})
