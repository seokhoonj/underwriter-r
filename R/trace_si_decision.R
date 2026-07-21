#' Trace one insured's simplified-issue decision back to the three questions
#'
#' The simplified-issue counterpart of [trace_decision()]. For a single insured
#' it runs the engine over that insured's claims alone and shows, per coverage,
#' what each of the three questions answered and which one decided the coverage.
#' Use it to audit one case: what fed in, how it combined, and why the coverage
#' landed where it did.
#'
#' Like [trace_decision()], it reuses the engine ([match_si_rule()] /
#' [combine_si_decision()]) rather than reimplementing the logic, so a trace can
#' never silently disagree with a production run. An independent, spec-derived
#' recomputation used to cross-check the engine lives in the test suite, not here.
#'
#' @param mapped Mapped claim lines from [map_disease()].
#' @param id The single insured id to trace.
#' @param rulebook A rulebook from [load_si_rulebook()].
#' @param product Product configuration from [si_product()].
#' @param source `"icis"` or `"declaration"`, as in [match_si_rule()].
#' @return A `data.table`, one row per coverage: the final `dec` and `reason`, the
#'   `question` that decided it, the driving `kcd_main`, and `answers` -- every
#'   question's answer for that coverage as `"question:dec"`, `" | "`-separated --
#'   so the fold is visible.
#' @seealso [trace_decision()], [match_si_rule()], [combine_si_decision()].
#' @export
trace_si_decision <- function(mapped, id, rulebook, product,
                              source = c("icis", "declaration")) {
  coverage <- dec <- reason <- question <- kcd_main <- answers <- final <- NULL  # NSE
  source <- match.arg(source)

  one <- as.data.table(mapped)[mapped$id == id]
  if (!nrow(one)) stop("no claim lines for id ", id)

  matched <- match_si_rule(one, rulebook, product, source)
  final   <- combine_si_decision(matched, id, rulebook, product)

  # every answer this insured produced, per coverage, so the fold is legible
  ans <- matched[, .(answers = paste(sprintf("%s:%s", question, dec), collapse = " | ")),
                 by = coverage]
  out <- final[ans, on = "coverage"]
  out[is.na(answers), answers := ""]
  out[, .(coverage, dec, reason, question, kcd_main, answers)][order(coverage)]
}
