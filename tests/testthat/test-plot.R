test_that("plot() dispatches for each result class and returns a ggplot", {
  skip_if_not_installed("ggplot2")
  f <- fixture()
  tab <- tabulate_decision(f$combined)
  expect_s3_class(plot(tab), "ggplot")                              # tabulated_decision
  li <- list_rule_impact(f$applied, f$combined)
  expect_s3_class(plot(li, coverage = "cov1"), "ggplot")            # rule_impact_list
  rr <- relax_rule(f$applied, f$combined, "M543")
  expect_s3_class(plot(rr), "ggplot")                               # relaxed_rule
})

test_that("plotting a multi-coverage ranking without picking a coverage errors", {
  skip_if_not_installed("ggplot2")
  f <- fixture()
  li <- list_rule_impact(f$applied, f$combined)
  expect_error(plot(li), "coverage")
})
