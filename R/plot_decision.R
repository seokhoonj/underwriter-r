#' Plot the decision composition per coverage
#'
#' A stacked bar of each coverage's decision composition from
#' [tabulate_decision()] -- by default the auto-decided vs manual-review share --
#' with each segment's percentage labelled at its centre.
#'
#' @param final A wide final-decision table from [combine_decision()].
#' @param group Column to stack and colour by: `"auto"` (default) or
#'   `"category"`.
#' @param order Coverage order along the x-axis: `"column"` (default, the
#'   coverage column order defined in the rule set, from `final`'s
#'   `decision_cols` attribute), `"auto_high"` (highest auto-decided share
#'   first), or `"auto_low"` (lowest first).
#' @param min_label Segments whose share is at or below this are left unlabelled,
#'   to keep thin slivers from cluttering (default `0.03`).
#' @param title Plot title (default `"decision composition per coverage"`).
#' @return A `ggplot` object. When `group = "auto"`, auto-decided (`1`) is blue
#'   and manual review (`0`) is red.
#' @seealso [tabulate_decision()].
#' @export
plot_decision <- function(final, group = c("auto", "category"),
                          order = c("column", "auto_high", "auto_low"), min_label = 0.03,
                          title = "decision composition per coverage") {
  group <- match.arg(group)
  order <- match.arg(order)
  tab   <- tabulate_decision(final)

  coverage_levels <- if (order == "column") {
    cols <- attr(final, "decision_cols")
    covs <- unique(tab$coverage)
    if (is.null(cols)) sort(covs) else c(intersect(cols, covs), setdiff(covs, cols))
  } else {
    auto_rate <- tab[, .(rate = sum(ratio[auto == "1"])), by = coverage]
    setorder(auto_rate, rate)
    if (order == "auto_high") rev(auto_rate$coverage) else auto_rate$coverage
  }

  d <- tab[, .(ratio = sum(ratio)), by = c("coverage", group)]
  d[, coverage := factor(coverage, levels = coverage_levels)]

  p <- ggplot2::ggplot(d, ggplot2::aes(x = coverage, y = ratio, fill = .data[[group]])) +
    ggplot2::geom_col() +
    ggplot2::geom_text(
      ggplot2::aes(label = ifelse(ratio > min_label, round(ratio * 100), NA_real_)),
      position = ggplot2::position_stack(vjust = 0.5), na.rm = TRUE) +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.25), labels = seq(0, 100, 25)) +
    ggplot2::labs(title = title, y = "percent", fill = group) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x        = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5),
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank())   # else the vertical gridlines peek
                                                       # above the bars and read as top ticks

  # auto-decided (1) blue, manual review (0) red
  if (group == "auto")
    p <- p + ggplot2::scale_fill_manual(values = c("0" = "#FB8072", "1" = "#80B1D3"))
  p
}
