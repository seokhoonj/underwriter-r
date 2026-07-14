#' Assemble the engine's rule set from disease rules and sentinel catch-alls
#'
#' [match_rule()] consumes one rule-set table, but the four sentinel catch-all
#' rows (`"VACANT"`, `"EXPIRED"`, `"IRREGULAR"`, `"UNMAPPED"`) are authored apart
#' from the disease rules -- kept in their own place so re-authoring the disease
#' rules cannot silently drop them. This appends `sentinel` below `ruleset` and
#' continues the rule number as `max(ruleset$no) + 1, 2, ...`, so a `no` is never
#' duplicated and the sentinels always sit after however large the disease rule
#' set has grown. `sentinel` carries no `no` of its own; it is assigned here (a
#' stale `no` it happens to carry is overwritten, not trusted).
#'
#' @param ruleset The disease rule table, one row per rule, carrying an integer
#'   `no`.
#' @param sentinel The sentinel catch-all rows, the same columns as `ruleset`
#'   except `no` (assigned here).
#' @return A single `data.table`: `ruleset` with `sentinel` appended below it and
#'   renumbered. Column order follows `ruleset` (so `no` stays first).
#' @seealso [match_rule()], [diagnose_ruleset()].
#' @export
combine_ruleset <- function(ruleset, sentinel) {
  no <- NULL  # data.table NSE; suppress R CMD check note
  ruleset  <- as.data.table(copy(ruleset))
  sentinel <- as.data.table(copy(sentinel))
  if (!nrow(ruleset))  stop("`ruleset` has no rows to number the sentinels after.")
  if (!nrow(sentinel)) return(ruleset[])
  if ("no" %in% names(sentinel)) sentinel[, no := NULL]
  sentinel[, no := max(ruleset$no) + .I]
  rbind(ruleset, sentinel, use.names = TRUE)[]
}
