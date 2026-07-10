#' Plot the decision composition per coverage
#'
#' `plot()` method for a [tabulate_decision()] result (class `tabulated_decision`):
#' a stacked bar of each coverage's decision composition -- by default the
#' auto-decided vs underwriter-referred share -- with each segment's percentage labelled
#' at its centre.
#'
#' @param x A [tabulate_decision()] result (class `tabulated_decision`).
#' @param group Column to stack and colour by: `"auto"` (default) or
#'   `"category"`.
#' @param order Coverage order along the x-axis: `"auto_high"` (default, highest
#'   auto-decided share first), `"auto_low"` (lowest first), or `"column"` (the
#'   coverage column order defined in the rule set, from the `decision_cols`
#'   attribute the tabulation carries).
#' @param min_label Segments whose share is at or below this are left unlabelled,
#'   to keep thin slivers from cluttering (default `0.03`).
#' @param title Plot title (default `"Decision composition per coverage"`).
#' @param ... Unused.
#' @return A `ggplot` object. When `group = "auto"`, auto-decided (`1`) is blue
#'   and underwriter-referred (`0`) is red.
#' @seealso [tabulate_decision()], [combine_decision()].
#' @method plot tabulated_decision
#' @export
plot.tabulated_decision <- function(x, ..., group = c("auto", "category"),
                                    order = c("auto_high", "auto_low", "column"),
                                    min_label = 0.03,
                                    title = "Decision composition per coverage") {
  tab   <- x
  group <- match.arg(group)
  order <- match.arg(order)

  coverage_levels <- if (order == "column") {
    cols <- attr(tab, "decision_cols")
    covs <- unique(tab$coverage)
    if (is.null(cols)) sort(covs) else c(intersect(cols, covs), setdiff(covs, cols))
  } else {
    auto_prop <- tab[, .(prop = sum(prop[auto == "1"])), by = coverage]
    setorder(auto_prop, prop)
    if (order == "auto_high") rev(auto_prop$coverage) else auto_prop$coverage
  }

  plot_data <- tab[, .(prop = sum(prop)), by = c("coverage", group)]
  plot_data[, coverage := factor(coverage, levels = coverage_levels)]

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = coverage, y = prop, fill = .data[[group]])) +
    ggplot2::geom_col() +
    ggplot2::geom_text(
      ggplot2::aes(label = ifelse(prop > min_label, round(prop * 100), NA_real_)),
      position = ggplot2::position_stack(vjust = 0.5), na.rm = TRUE) +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.25), labels = seq(0, 100, 25)) +
    ggplot2::labs(title = title, y = "percent", fill = group) +
    ggplot2::theme_bw() +   # white panel, black border, no gridlines
    ggplot2::theme(
      axis.text.x      = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border     = ggplot2::element_rect(colour = "black", fill = NA))

  # auto-decided (1) blue, underwriter-referred (0) red
  if (group == "auto")
    p <- p + ggplot2::scale_fill_manual(values = c("0" = "#FB8072", "1" = "#80B1D3"))
  p
}
