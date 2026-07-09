#' Plot a rule-relaxation result
#'
#' `plot()` methods: a [list_rule_impact()] result (class `rule_impact_list`)
#' draws the per-rule marginal-impact ranking as a horizontal bar chart; a
#' [relax_rule()] result (class `relaxed_rule`) draws the chosen rule's
#' per-coverage before/after as a dumbbell chart.
#'
#' @param x A [list_rule_impact()] result (`rule_impact_list`) or a
#'   [relax_rule()] result (`relaxed_rule`).
#' @param coverage For a per-coverage ranking
#'   (`list_rule_impact(..., by_coverage = TRUE)`), the single coverage to plot,
#'   e.g. `"adb"`. Slice through this argument rather than subsetting the object
#'   first -- data.table's `[` drops the class that method dispatch relies on.
#' @param top Rows to show: the ranking keeps the top `12`, the relaxed-rule plot
#'   defaults to `NULL` (every coverage the rule moves).
#' @param fill Bar fill colour for the ranking plot.
#' @param disease Optional disease label woven into the relaxed-rule plot's title.
#' @param title Plot title.
#' @param ... Unused.
#' @return A `ggplot` object. In the ranking plot the bar length is the
#'   automation-rate lift; in the relaxed-rule plot baseline points are grey and
#'   relaxed points blue.
#' @seealso [list_rule_impact()], [relax_rule()], [combine_decision()].
#' @name plot.relaxation
NULL

#' @rdname plot.relaxation
#' @method plot rule_impact_list
#' @export
plot.rule_impact_list <- function(x, ..., coverage = NULL, top = 12L, fill = "#4E79A7",
                                      title = NULL) {
  x <- as.data.table(x)
  has_cov <- "coverage" %in% names(x)
  if (has_cov && !is.null(coverage)) {
    pick <- coverage
    x <- x[coverage == pick]
  }
  cov <- if (has_cov) {
    covs <- unique(x$coverage)
    if (length(covs) != 1L)
      stop("per-coverage ranking: pass coverage = \"<name>\" to pick one, e.g. plot(x, coverage = \"adb\").")
    covs
  } else NULL
  if (!nrow(x))
    stop("no rows to plot (the coverage slice has no relaxable diseases).")

  d <- head(x, top)
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
    ggplot2::theme_bw() +   # white panel, black border, no gridlines (like the decision plot)
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border     = ggplot2::element_rect(colour = "black", fill = NA))
}

#' @rdname plot.relaxation
#' @method plot relaxed_rule
#' @export
plot.relaxed_rule <- function(x, ..., disease = NULL, top = NULL, title = NULL) {
  d <- as.data.table(x)[auto_relaxed != auto_base]   # only coverages it moves
  if (!nrow(d)) stop("this relaxation moved no coverage; nothing to plot.")
  if (!is.null(top)) d <- head(d[order(-abs(lift))], top)
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
