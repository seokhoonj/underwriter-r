#' Combine the three questions' answers into one decision per coverage
#'
#' The simplified-issue counterpart of [combine_decision()]. Where the standard
#' path composes eight codes through per-code combiners, this takes the worst of
#' three by the `decision` sheet's `priority` (smaller is worse, so decline wins).
#' Every insured x coverage the product sells gets a row: [match_si_rule()] already
#' carries the whole roster (an insured that tripped no question arrives as an
#' explicit standard), so the id universe is read off `matched` -- the same
#' no-insured-left-behind invariant the standard path holds through its sentinels,
#' with no separate id list to pass.
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
  all  <- as.data.table(matched)
  if (!nrow(all))
    return(data.table(id = all$id[0L], coverage = character(), dec = character(),
                      reason = character(), question = character(),
                      kcd_main = character()))
  grid <- CJ(id = unique(all$id), coverage = product$coverages, unique = TRUE)

  all[, .rank := rulebook$rank[dec]]
  # An answer with no decision is an upstream bug, not an accept. Guard before the
  # fold: setorder puts NA first (na.last = FALSE), so an NA `.rank` would win the
  # worst-pick and then be filled to standard below -- laundering a broken answer
  # into an acceptance.
  if (anyNA(all$dec) || anyNA(all$.rank))
    stop("`matched` carries ", sum(is.na(all$dec) | is.na(all$.rank)),
         " answer(s) with no decision; a band comparison returned NA.")

  setorder(all, id, coverage, .rank)
  worst <- all[, .(dec = dec[1L], reason = reason[1L], question = question[1L],
                   kcd_main = kcd_main[1L]), by = .(id, coverage)]

  # Only a genuine grid miss -- an insured who raised no question -- is filled to
  # standard. `worst` never carries NA now, so a missing `dec` here can only come
  # from the right join, i.e. someone with no answer.
  out <- worst[grid, on = .(id, coverage)]
  out[is.na(dec), dec := code[["standard"]]]
  setorder(out, id, coverage)
  out[]
}
