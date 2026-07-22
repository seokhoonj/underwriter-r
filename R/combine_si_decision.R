#' Combine the three questions' answers into one decision per coverage
#'
#' The simplified-issue counterpart of [combine_decision()]. Where the standard
#' path composes eight codes through per-code combiners, this takes the worst of
#' three by the `decision` sheet's `priority` (smaller is worse, so decline wins).
#' Every insured x coverage the product sells gets a row: [match_si_rule()] already
#' carries the whole roster, so the id universe is read off `matched` -- the same
#' no-insured-left-behind invariant the standard path holds through its sentinels,
#' with no separate id list to pass. Presence rows (no question, no decision) seed
#' the roster only; a grid cell left with no answer is the baseline standard, a
#' no-applicable-condition accept.
#'
#' @param matched Answers from [match_si_rule()], carrying every insured.
#' @param rulebook A rulebook from [load_si_rulebook()].
#' @param product Product configuration from [si_product()].
#' @return A `data.table`, one row per `(id, coverage)`: `dec`, the `reason` that
#'   produced it, the `question` it came from, and the driving `kcd_main`.
#' @seealso [match_si_rule()], [tabulate_si_decision()].
#' @export
combine_si_decision <- function(matched, rulebook, product) {
  id <- coverage <- dec <- .rank <- reason <- question <- kcd_main <- NULL  # NSE
  code <- rulebook$code
  matched_dt <- as.data.table(matched)
  if (!nrow(matched_dt))
    return(data.table(id = matched_dt$id[0L], coverage = character(), dec = character(),
                      reason = character(), question = character(),
                      kcd_main = character()))
  grid <- CJ(id = unique(matched_dt$id), coverage = product$coverages, unique = TRUE)

  # Presence rows (no question, no decision) only seed the roster; a real answer
  # with a missing decision is still an upstream bug. Guard the answers before the
  # fold: setorder puts NA first (na.last = FALSE), so an NA `.rank` would win the
  # worst-pick and then be filled to standard below -- laundering a broken answer.
  ans <- matched_dt[!is.na(question)]
  ans[, .rank := rulebook$rank[dec]]
  if (anyNA(ans$dec) || anyNA(ans$.rank))
    stop("`matched` carries ", sum(is.na(ans$dec) | is.na(ans$.rank)),
         " answer(s) with no decision; a band comparison returned NA.")

  setorder(ans, id, coverage, .rank)
  worst <- ans[, .(dec = dec[1L], reason = reason[1L], question = question[1L],
                   kcd_main = kcd_main[1L]), by = .(id, coverage)]

  # A grid cell with no answer is the baseline: an insured with usable, in-window
  # history that tripped no question is accepted (no applicable condition). This is
  # the single place the baseline standard is decided -- as on the standard path.
  out <- worst[grid, on = .(id, coverage)]
  out[is.na(dec), dec := code[["standard"]]]
  setorder(out, id, coverage)
  out[]
}
