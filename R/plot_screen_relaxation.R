#' Plot the disease relaxation ranking
#'
#' Draws the ranking from [screen_relaxation()] as a horizontal bar chart: one bar per
#' representative disease, its length the automation-rate lift (percentage points)
#' from relaxing that disease on its own, labelled with the lift and the insured
#' count moved off manual review. Pass either the overall ranking
#' (`screen_relaxation(applied, final)`) or a single coverage's ranking (one coverage
#' sliced out of `screen_relaxation(..., by_coverage = TRUE)`); when a `coverage`
#' column is present its name is woven into the axis label and title.
#'
#' @param ranking A `data.table` from [screen_relaxation()]. If it carries a `coverage`
#'   column it must hold a single coverage -- slice one first, e.g.
#'   `ranking[coverage == "adb"]`.
#' @param top Number of top-ranked diseases to show (default `12`).
#' @param fill Bar fill colour (default `"#4E79A7"`).
#' @param title Plot title; by default a sentence naming the coverage for a
#'   single-coverage slice, or the overall gain otherwise.
#' @return A `ggplot` object.
#' @seealso [screen_relaxation()], [plot_decision()].
#' @export
plot_screen_relaxation <- function(ranking, top = 12L, fill = "#4E79A7", title = NULL) {
  ranking <- as.data.table(ranking)
  if (!nrow(ranking))
    stop("`ranking` has no rows to plot (the coverage slice has no relaxable diseases).")
  cov <- if ("coverage" %in% names(ranking)) {
    covs <- unique(ranking$coverage)
    if (length(covs) > 1L)
      stop("`ranking` holds several coverages; slice one first, e.g. ranking[coverage == \"adb\"].")
    covs
  } else NULL

  d <- head(ranking, top)
  if (is.null(title))
    title <- if (is.null(cov)) "Diseases to relax for the biggest automation-rate gain"
             else sprintf("Diseases that lift the %s coverage", cov)
  ylab <- if (is.null(cov)) "overall automation-rate lift (%p)"
          else sprintf("%s automation-rate lift (%%p)", cov)

  ggplot2::ggplot(d, ggplot2::aes(x = stats::reorder(.data$kcd_main, .data$auto_lift),
                                  y = .data$auto_lift * 100)) +
    ggplot2::geom_col(fill = fill) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.2f%%p (%s)", .data$auto_lift * 100,
                                   format(.data$n_id, big.mark = ","))),
      hjust = -0.05, size = 3) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.25))) +
    ggplot2::labs(x = "kcd_main", y = ylab, title = title) +
    ggplot2::theme_bw() +   # white panel, black border, no gridlines (like plot_decision)
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border     = ggplot2::element_rect(colour = "black", fill = NA))
}
