#' Plot one disease's per-coverage relaxation effect
#'
#' Draws the per-coverage before/after from [simulate_relaxation()] as a dumbbell chart:
#' one row per coverage the disease actually moves, a grey point at the baseline
#' automation rate and a coloured point at the relaxed rate, joined by a segment
#' whose length is the lift. The lift (percentage points) is labelled at the
#' relaxed end. Coverages the disease leaves unchanged are dropped.
#'
#' Complements [plot_screen_relaxation()]: that ranks every disease by its overall
#' lift, this shows where a single chosen disease's lift lands across coverages.
#'
#' @param relaxed A `data.table` from [simulate_relaxation()] (columns `coverage`,
#'   `auto_base`, `auto_relaxed`, `lift`, `n_flipped`).
#' @param disease Optional disease label (the `kcd_main` passed to
#'   [simulate_relaxation()]) woven into the title.
#' @param top Optional cap on the number of coverages shown, taken by the largest
#'   absolute lift; default `NULL` shows every coverage the disease moves.
#' @param title Plot title; by default a sentence naming the disease when
#'   `disease` is given, or a generic before/after line otherwise.
#' @return A `ggplot` object. Baseline points are grey, relaxed points blue.
#' @seealso [simulate_relaxation()], [plot_screen_relaxation()].
#' @export
plot_simulate_relaxation <- function(relaxed, disease = NULL, top = NULL, title = NULL) {
  d <- as.data.table(relaxed)[auto_relaxed != auto_base]   # only coverages it moves
  if (!is.null(top)) d <- utils::head(d[order(-abs(lift))], top)
  d[, coverage := factor(coverage, levels = d[order(auto_relaxed), coverage])]

  long <- melt(d, id.vars = "coverage", measure.vars = c("auto_base", "auto_relaxed"),
               variable.name = "state", value.name = "share")
  long[, state := factor(fifelse(state == "auto_base", "baseline", "relaxed"),
                         levels = c("baseline", "relaxed"))]

  if (is.null(title))
    title <- if (is.null(disease)) "Automation rate by coverage: before vs after relaxing"
             else sprintf("Relaxing %s: automation rate by coverage", disease)

  ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = d, ggplot2::aes(y = .data$coverage, yend = .data$coverage,
                             x = .data$auto_base * 100, xend = .data$auto_relaxed * 100),
      colour = "grey70", linewidth = 1) +
    ggplot2::geom_point(
      data = long, ggplot2::aes(y = .data$coverage, x = .data$share * 100, colour = .data$state),
      size = 2.8) +
    ggplot2::geom_text(
      data = d, ggplot2::aes(y = .data$coverage, x = .data$auto_relaxed * 100,
                             label = sprintf("%+.1f%%p (%s)", .data$lift * 100,
                                             format(.data$n_flipped, big.mark = ","))),
      hjust = -0.15, size = 3) +
    ggplot2::scale_colour_manual(values = c(baseline = "grey50", relaxed = "#4E79A7")) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.2))) +
    ggplot2::labs(x = "automation rate (percent)", y = "coverage", colour = NULL, title = title) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border     = ggplot2::element_rect(colour = "black", fill = NA),
      legend.position  = "top")
}
