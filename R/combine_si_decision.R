#' Combine the three questions' answers into one decision per coverage
#'
#' The simplified-issue counterpart of [combine_decision()]. Where the standard
#' path composes eight codes through per-code combiners, this takes the worst of
#' three by the `decision` sheet's `priority` (smaller is worse, so decline wins).
#' Every insured x coverage the product sells gets a row whether or not any
#' question raised anything: an insured with no claim history appears in no
#' answer, and dropping them would shrink the denominator silently -- the same
#' no-insured-left-behind invariant the standard path holds through its sentinels.
#'
#' @param matched Answers from [match_si_rule()].
#' @param ids Every insured id in the input, so none is lost.
#' @param rulebook A rulebook from [load_si_rulebook()].
#' @param product Product configuration from [si_product()].
#' @return A `data.table`, one row per `(id, coverage)`: `dec`, the `reason` that
#'   produced it, the `question` it came from, and the driving `kcd_main`.
#' @seealso [match_si_rule()], [tabulate_si_decision()].
#' @export
combine_si_decision <- function(matched, ids, rulebook, product) {
  id <- coverage <- dec <- .rank <- reason <- question <- kcd_main <- NULL  # NSE
  code <- rulebook$code
  grid <- CJ(id = unique(ids), coverage = product$coverages, unique = TRUE)
  all  <- as.data.table(matched)
  if (!nrow(all))
    return(grid[, .(id, coverage, dec = code[["standard"]], reason = NA_character_,
                    question = NA_character_, kcd_main = NA_character_)])

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
