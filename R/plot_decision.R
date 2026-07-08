#' Plot the decision composition per coverage
#'
#' A stacked bar of each coverage's decision composition from
#' [tabulate_decision()] -- by default the auto-decided vs manual-review share --
#' with each segment's percentage labelled at its centre.
#'
#' @param final A wide final-decision table from [combine_decision()].
#' @param by The column to stack and colour by: `"auto"` (default) or
#'   `"category"`.
#' @param order Coverage order along the x-axis: `"config"` (default, the
#'   coverage order defined in the rule set, from `final`'s `decision_cols`
#'   attribute) or `"auto"` (highest auto-decided share first).
#' @param min_label Segments whose share is at or below this are left unlabelled,
#'   to keep thin slivers from cluttering (default `0.03`).
#' @return A `ggplot` object. When `by = "auto"`, auto-decided (`1`) is blue and
#'   manual review (`0`) is red.
#' @seealso [tabulate_decision()].
#' @export
plot_decision <- function(final, by = c("auto", "category"),
                          order = c("config", "auto"), min_label = 0.03) {
  by    <- match.arg(by)
  order <- match.arg(order)
  tab   <- tabulate_decision(final)

  coverage_levels <- if (order == "auto") {
    auto_rate <- tab[, .(rate = sum(ratio[auto == "1"])), by = coverage]
    setorder(auto_rate, -rate)$coverage
  } else {
    cols <- attr(final, "decision_cols")
    covs <- unique(tab$coverage)
    if (is.null(cols)) sort(covs) else c(intersect(cols, covs), setdiff(covs, cols))
  }

  d <- tab[, .(ratio = sum(ratio)), by = c("coverage", by)]
  d[, coverage := factor(coverage, levels = coverage_levels)]

  p <- ggplot2::ggplot(d, ggplot2::aes(x = coverage, y = ratio, fill = .data[[by]])) +
    ggplot2::geom_col() +
    ggplot2::geom_text(
      ggplot2::aes(label = ifelse(ratio > min_label, round(ratio * 100), NA_real_)),
      position = ggplot2::position_stack(vjust = 0.5), na.rm = TRUE) +
    ggplot2::labs(fill = by) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5))

  # auto-decided (1) blue, manual review (0) red
  if (by == "auto")
    p <- p + ggplot2::scale_fill_manual(values = c("0" = "firebrick", "1" = "steelblue"))
  p
}
