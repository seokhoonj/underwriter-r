#' Combine per-disease decisions into a per-insured final decision
#'
#' Collapses the per-disease decisions from [match_rule()] to one decision per
#' `(id, coverage)`. Fully table-driven and code-letter agnostic: no decision code
#' (`R`/`E`/`L`/`S`/`D`/`U` ...) is hard-wired. `decision_table` supplies both the
#' class letters and their meaning, so a company that writes exclusion as `X`
#' instead of `R` only edits its table -- the code is unchanged. The four
#' combiners are: `priority` (worst code wins), `exclusion` (union sites, max
#' period per site, over `max_sites` sites declines), `loading` (sum indices, then
#' band), `reduction` (longest period).
#'
#' @param applied The per-disease decisions from [match_rule()] (`$applied`).
#' @param decision_table Decision-code table with columns `code`, `priority`
#'   (lower = worse), `combine` (`priority`/`exclusion`/`loading`/`reduction`),
#'   and `role` (marks the engine-emitted codes: `standard`, `decline`,
#'   `manual_review`).
#' @param exclusion_table,reduction_table Period-code tables with columns `mark`,
#'   `base_year`, `minus_elapsed`, `all_period`.
#' @param loading_table Columns `lower`, `decision`.
#' @param decision_cols Coverage decision columns (default: the `"decision_cols"`
#'   attribute set by [match_rule()]).
#' @param max_sites Maximum distinct exclusion sites before a coverage declines.
#' @return A wide `data.table`, one row per `id`, one column per coverage.
#' @export
combine_decision <- function(applied, decision_table, exclusion_table, reduction_table, loading_table,
                             decision_cols = attr(applied, "decision_cols"), max_sites = 4L) {
  priority <- setNames(as.integer(decision_table$priority), decision_table$code)
  combiner <- setNames(decision_table$combine, decision_table$code)
  letter   <- .decision_letters(decision_table, priority)

  long <- .melt_decisions(applied, decision_cols, combiner, letter$manual_review)

  results <- rbindlist(list(
    .combine_priority( long[method == "priority"] , priority),
    .combine_exclusion(long[method == "exclusion"], exclusion_table, max_sites, letter$exclusion, letter$decline),
    .combine_loading(  long[method == "loading"]  , loading_table  , letter$loading),
    .combine_reduction(long[method == "reduction"], reduction_table, letter$reduction)
  ), use.names = TRUE)

  .pick_worst(results, priority, unique(long[, .(id, coverage)]), letter$standard)
}

# Resolve the company's code letters from the table: class letters from the
# `combine` column, engine-emitted codes from the `role` column (falling back to
# best/worst priority for standard/decline when no role column is supplied).
.decision_letters <- function(decision_table, priority) {
  by_combine <- function(m) decision_table$code[decision_table$combine == m][1L]
  by_role    <- function(r) if ("role" %in% names(decision_table))
    decision_table$code[!is.na(decision_table$role) & decision_table$role == r][1L] else NA_character_
  standard <- by_role("standard"); if (is.na(standard)) standard <- names(which.max(priority))
  decline  <- by_role("decline");  if (is.na(decline))  decline  <- names(which.min(priority))
  list(exclusion     = by_combine("exclusion"),
       loading       = by_combine("loading"),
       reduction     = by_combine("reduction"),
       standard      = standard,
       decline       = decline,
       manual_review = by_role("manual_review"))
}

# Melt the per-disease decisions to one row per (id, coverage, disease), tagging
# each with the combiner its class uses. No-rule diseases route to manual review.
.melt_decisions <- function(applied, decision_cols, combiner, manual_review) {
  applied <- as.data.table(copy(applied))
  applied[matched == 0L, (decision_cols) := manual_review]
  long <- melt(applied, id.vars = c("id", "elp_day"), measure.vars = decision_cols,
               variable.name = "coverage", value.name = "code", variable.factor = FALSE)
  long <- long[!is.na(code) & nzchar(code)]
  long[, method := combiner[substr(code, 1L, 1L)]]
  long[is.na(method), method := "priority"]
  long[]
}

# priority: keep the worst (lowest-priority-number) code.
.combine_priority <- function(rows, priority) {
  if (!nrow(rows)) return(rows[, .(id, coverage, dec = code)])
  rows[, rank := priority[substr(code, 1L, 1L)]]
  rows[is.na(rank), rank := max(priority) + 1L]
  setorder(rows, id, coverage, rank)
  rows[, .(dec = code[1L]), by = .(id, coverage)]
}

# exclusion: split "R01(5i),R03(3i)" into sites, resolve each, keep the longest
# period per site, drop expired sites, and rebuild; too many sites -> decline.
# `letter` is the company's exclusion code letter (e.g. "R"); `decline` its
# decline code.
.combine_exclusion <- function(rows, period_table, max_sites, letter, decline) {
  if (!nrow(rows)) return(rows[, .(id, coverage, dec = code)])
  sites <- rows[, .(token = unlist(strsplit(code, ",", fixed = TRUE))),
                by = .(id, coverage, elp_day)]
  sites[, area   := sub(sprintf("^%s([0-9]+)\\(.*$", letter), "\\1", token)]
  sites[, mark   := sub(sprintf("^%s[0-9]+\\((.*)\\)$", letter), "\\1", token)]
  sites[, months := .resolve_months(mark, elp_day, period_table)]
  sites <- sites[!is.na(months) & months > 0L]
  per_site <- sites[, .(months = max(months)), by = .(id, coverage, area)]
  setorder(per_site, id, coverage, area)   # canonical, deterministic site order
  per_site[, .(dec = .exclusion_code(area, months, max_sites, letter, decline)), by = .(id, coverage)]
}
.exclusion_code <- function(area, months, max_sites, letter, decline) {
  if (length(area) > max_sites) return(decline)
  paste(sprintf("%s%s(%s)", letter, area, .months_str(months)), collapse = ",")
}

# loading: sum the indices, then the worst band the sum reaches. `letter` is the
# company's loading code letter (e.g. "E").
.combine_loading <- function(rows, loading_table, letter) {
  if (!nrow(rows)) return(rows[, .(id, coverage, dec = code)])
  rows[, index := as.integer(sub(sprintf("^%s\\(([0-9]+)\\).*$", letter), "\\1", code))]
  rows[is.na(index), index := 0L]
  bands  <- loading_table[order(lower)]
  totals <- rows[, .(total = sum(index)), by = .(id, coverage)]
  totals[, .(id, coverage, dec = bands$decision[findInterval(total, bands$lower)])]
}

# reduction: resolve the period, keep the longest. `letter` is the company's
# reduction code letter (e.g. "L").
.combine_reduction <- function(rows, period_table, letter) {
  if (!nrow(rows)) return(rows[, .(id, coverage, dec = code)])
  rows[, mark   := sub(sprintf("^%s\\((.*)\\)$", letter), "\\1", code)]
  rows[, months := .resolve_months(mark, elp_day, period_table)]
  rows <- rows[!is.na(months) & months > 0L]
  rows[, .(dec = sprintf("%s(%s)", letter, .months_str(max(months)))), by = .(id, coverage)]
}

# From every combiner's result, keep the worst per (id, coverage); a coverage that
# resolved to nothing (e.g. all exclusions expired) falls back to standard.
.pick_worst <- function(results, priority, all_pairs, standard) {
  results[, rank := priority[substr(dec, 1L, 1L)]]
  results[is.na(rank), rank := max(priority) + 1L]
  setorder(results, id, coverage, rank)
  worst <- results[, .(dec = dec[1L]), by = .(id, coverage)]
  worst <- worst[all_pairs, on = .(id, coverage)]
  worst[is.na(dec), dec := standard]
  dcast(worst, id ~ coverage, value.var = "dec")
}

# A period mark is "5i" (5 years minus elapsed), "3" (3 years fixed), or "99"
# (whole period). Resolve it to a month count given the elapsed days; a result
# <= 0 means the restriction has expired. 30 days = 1 month, truncated.
.resolve_months <- function(mark, elapsed_days, period_table) {
  row     <- match(mark, period_table$mark)
  elapsed <- as.integer(elapsed_days %/% 30L)
  years   <- period_table$base_year[row]
  months  <- fifelse(period_table$minus_elapsed[row], years * 12L - elapsed, years * 12L)
  months[period_table$all_period[row]] <- 9999L
  months[is.na(row)] <- NA_integer_
  months
}
.months_str <- function(months) fifelse(months >= 9999L, "99", as.character(months))
