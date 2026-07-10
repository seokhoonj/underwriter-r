#' Combine per-disease decisions into a per-insured final decision
#'
#' Collapses the per-disease decisions from [match_rule()] to one decision per
#' `(id, coverage)`. Fully table-driven and code-letter agnostic: no decision code
#' (`R`/`E`/`L`/`S`/`D`/`U` ...) is hard-wired. `decision_table` supplies both the
#' class letters and their meaning, so a company that writes exclusion as `X`
#' instead of `R` only edits its table -- the code is unchanged.
#'
#' Two columns of `decision_table` do two different jobs. `combiner` says how
#' several diseases' codes of *one* class merge into that class's single result:
#' `exclusion` unions the sites and keeps the longest period per site, `loading`
#' sums the indices, `reduction` keeps the longest period, and `priority` -- for a
#' class with no merge rule of its own -- keeps the worst code.
#'
#' Two of those merges leave behind an accumulated amount -- the exclusion's
#' distinct site count, the loading's summed index -- and `band_table` says what
#' each amount decides. Both are read the same way: the band the amount lands in
#' either keeps the class's own output, when its `decision` is the bare class
#' letter, or replaces it with another code. So `R` at `at_least = 1` keeps the
#' exclusion an insured earned, and `D` at `at_least = 5` declines a coverage once
#' the exclusion spans five sites; `E` at `at_least = 0` writes the loading
#' (`E(75)`), and `U` at `at_least = 50` refers it to the underwriter instead.
#'
#' `role` then says how the classes meet each other. The `decline` and
#' `underwriter` codes are terminal: either one stands alone on a cell, suppressing
#' every restriction, and the worse of the two wins. The `standard` code is the
#' identity and drops out. Everything else composes, so an insured can carry an
#' exclusion, a loading and a reduction on one coverage at once
#' (`"E(75),L(36),R03(24),R12(24)"`), written in the decision table's row order.
#' Terminality is judged on each combiner's *output*: a summed loading that reaches
#' a decline band, or an exclusion whose site count does, escalates the whole cell
#' even though neither input code was terminal.
#'
#' One consequence is worth knowing before writing the table. Every class whose
#' `combiner` is `priority` shares a single bucket, so those classes stay exclusive
#' of *each other* -- a cell holding both `C` and `M` keeps only the worse of the
#' two, and the loser never reaches the composition. That is the right reading when
#' the two are genuine alternatives. When two such restrictions must instead sit on
#' one cell together, give each its own `combiner`, so each contributes a result of
#' its own for the composition to pick up.
#'
#' `priority` therefore carries less weight than its name suggests. It orders the
#' two terminal codes against each other, and it ranks the codes that merge by
#' `priority` within their bucket. It does *not* order the composition -- that
#' follows the decision table's row order -- and it does not decide which classes
#' compose, which is `role`'s job. Codes that compose may share a priority number
#' freely; doing so states that none of them dominates the others.
#'
#' Every insured reaches here with at least one row in `applied` -- one with
#' nothing to underwrite arrives on the no-diagnosis code, whose decision the
#' rule set supplies -- so every id gets a decision and none has to be re-added
#' afterwards. `nrow(combined)` therefore equals the number of insured in the
#' claim feed.
#'
#' @param applied The per-disease decisions from [match_rule()] (`$applied`).
#' @param decision_table Decision-code table with columns `code`, `priority`
#'   (lower = worse; ranks the two terminal codes and the codes merging by
#'   `priority`, nothing else), `combiner`
#'   (`priority`/`exclusion`/`loading`/`reduction`, the within-class merge rule),
#'   `role` (marks the engine-emitted codes: `standard`, `decline`,
#'   `underwriter` -- an `underwriter` row is required, since unmatched
#'   diseases are referred there), and `auto` (`1`/`0`, read by [tabulate_decision()]
#'   and `plot()` to flag which codes count as automatic). Its row order is the
#'   order the composed codes are written in.
#' @param exclusion_table,reduction_table Period-code tables listing the valid
#'   `mark`s (`"5i"` = 5 years minus elapsed, `"3"` = 3 years, `"99"` = whole
#'   period); the period logic is parsed from the mark itself.
#' @param band_table Columns `class`, `at_least`, `decision`. One staircase of
#'   bands per accumulating class: within a `class`, a row claims every amount from
#'   its `at_least` up to the next row's, and the last row runs to infinity. Every
#'   class the engine accumulates for -- the exclusion and loading codes -- needs at
#'   least one band, and its first `at_least` must be low enough to catch the
#'   smallest amount that class can produce (`0` for a loading, `1` for an
#'   exclusion, which always has a site). A `decision` holding the bare class letter
#'   keeps that class's own output; any other value substitutes for it.
#' @param decision_cols Coverage decision columns (default: the `"decision_cols"`
#'   attribute set by [match_rule()]).
#' @return A wide `data.table`, one row per `id`, one column per coverage. The
#'   four supplied tables ride along as attributes (`decision_table`,
#'   `exclusion_table`, `reduction_table`, `band_table`), together with
#'   `decision_cols` (the rule-set coverage order, which the `id ~ coverage`
#'   reshape would otherwise sort away), so downstream summaries such as
#'   [tabulate_decision()] and `plot()`, and the functions that recombine
#'   ([trace_decision()], [relax_rule()]), can recover them without re-passing.
#' @export
combine_decision <- function(applied, decision_table, exclusion_table, reduction_table, band_table,
                             decision_cols = attr(applied, "decision_cols")) {
  priority <- setNames(as.integer(decision_table$priority), decision_table$code)
  combiner <- setNames(decision_table$combiner, decision_table$code)
  letter   <- .decision_letters(decision_table, priority)
  if (is.na(letter$underwriter))
    stop("`decision_table` needs a row with role == \"underwriter\"; unmatched diseases are referred there.")
  exclusion_band <- .band_for(band_table, letter$exclusion)
  loading_band   <- .band_for(band_table, letter$loading)

  long <- .melt_decisions(applied, decision_cols, combiner, letter$underwriter)

  results <- rbindlist(list(
    .combine_priority( long[method == "priority"] , priority),
    .combine_exclusion(long[method == "exclusion"], exclusion_table, exclusion_band, letter$exclusion),
    .combine_loading(  long[method == "loading"]  , loading_band   , letter$loading),
    .combine_reduction(long[method == "reduction"], reduction_table, letter$reduction)
  ), use.names = TRUE)

  combined <- .compose_decision(results, letter, priority, unique(long[, .(id, coverage)]))

  setattr(combined, "decision_table",  decision_table)
  setattr(combined, "exclusion_table", exclusion_table)
  setattr(combined, "reduction_table", reduction_table)
  setattr(combined, "band_table",      band_table)
  setattr(combined, "decision_cols",   decision_cols)   # rule-set coverage order, for plot()
  setattr(combined, "class", c("combined_decision", "data.table", "data.frame"))
  combined
}

# The bands of one class, ordered. A class the engine has no letter for (the rule
# set uses no loading, say) needs no bands and gets `NULL`; its combiner is never
# reached. Any class that does exist must be banded, or there is no way to say what
# a given accumulation decides -- and silently leaving it unbounded would let, for
# instance, an exclusion sprawl over every site and still auto-decide.
.band_for <- function(band_table, letter) {
  if (is.na(letter)) return(NULL)
  bands <- as.data.table(band_table)[class == letter]
  if (!nrow(bands))
    stop(sprintf("`band_table` has no rows for class \"%s\"; every class it accumulates for needs a band.",
                 letter))
  bands[order(at_least)]
}

# Map an accumulated amount -- the exclusion's distinct site count, the loading's
# summed index -- to a decision. The band it lands in either keeps `class_output`,
# what the class built for itself, when the band's `decision` is the bare class
# letter, or replaces it with another code, which is how a rule set escalates to the
# underwriter or declines outright. `pmax(., 1L)` clamps an amount below the first
# band, which would otherwise index 0, drop the element, and recycle a wrong band
# across the group.
.apply_band <- function(amount, class_output, bands, letter) {
  band <- bands$decision[pmax(findInterval(amount, bands$at_least), 1L)]
  fifelse(band == letter, class_output, band)
}

# Resolve the company's code letters from the table: class letters from the
# `combine` column, engine-emitted codes from the `role` column (falling back to
# best/worst priority for standard/decline when no role column is supplied).
#
# `terminal` are the two codes that stand alone on a cell -- decline and
# underwriter -- and `sheet_ord` is the table's own row order, which sets the
# order the composed codes are written in.
.decision_letters <- function(decision_table, priority) {
  by_combine <- function(m) decision_table$code[decision_table$combiner == m][1L]
  by_role    <- function(r) if ("role" %in% names(decision_table))
    decision_table$code[!is.na(decision_table$role) & decision_table$role == r][1L] else NA_character_
  standard <- by_role("standard"); if (is.na(standard)) standard <- names(which.max(priority))
  decline  <- by_role("decline");  if (is.na(decline))  decline  <- names(which.min(priority))
  underwriter <- by_role("underwriter")
  list(exclusion   = by_combine("exclusion"),
       loading     = by_combine("loading"),
       reduction   = by_combine("reduction"),
       standard    = standard,
       decline     = decline,
       underwriter = underwriter,
       terminal    = unique(c(decline, underwriter)),
       sheet_ord   = setNames(seq_len(nrow(decision_table)), decision_table$code))
}

# Melt the per-disease decisions to one row per (id, coverage, disease), tagging
# each with the combiner its class uses. No-rule diseases go to the underwriter.
.melt_decisions <- function(applied, decision_cols, combiner, underwriter) {
  applied <- as.data.table(copy(applied))
  applied[matched == 0L, (decision_cols) := underwriter]
  long <- melt(applied, id.vars = c("id", "elp_day"), measure.vars = decision_cols,
               variable.name = "coverage", value.name = "code", variable.factor = FALSE)
  long <- long[!is.na(code) & nzchar(code)]
  long[, method := combiner[substr(code, 1L, 1L)]]
  long[is.na(method), method := "priority"]
  long[]
}

# priority: keep the worst (lowest-priority-number) code. This is the merge rule
# for a class that has none of its own, so every such class lands in this one
# bucket and they come out exclusive of each other -- only the worst reaches the
# composition. Give a class its own combiner when it has to sit beside the others
# on one cell rather than displace them.
.combine_priority <- function(rows, priority) {
  if (!nrow(rows)) return(rows[, .(id, coverage, dec = code)])
  rows[, rank := priority[substr(code, 1L, 1L)]]
  rows[is.na(rank), rank := max(priority) + 1L]
  setorder(rows, id, coverage, rank)
  rows[, .(dec = code[1L]), by = .(id, coverage)]
}

# exclusion: split "R01(5i),R03(3i)" into sites, resolve each period, keep the
# longest per site, drop the expired ones, and rebuild. The distinct site count is
# what the exclusion bands on, so a rule set declines -- or refers -- an insured
# whose exclusions sprawl over too many sites. `letter` is the company's exclusion
# code letter (e.g. "R").
.combine_exclusion <- function(rows, period_table, bands, letter) {
  if (!nrow(rows)) return(rows[, .(id, coverage, dec = code)])
  sites <- rows[, .(token = unlist(strsplit(code, ",", fixed = TRUE))),
                by = .(id, coverage, elp_day)]
  sites[, site   := sub(sprintf("^%s([0-9]+)\\(.*$", letter), "\\1", token)]
  sites[, mark   := sub(sprintf("^%s[0-9]+\\((.*)\\)$", letter), "\\1", token)]
  sites[, months := .resolve_months(mark, elp_day, period_table)]
  sites <- sites[!is.na(months) & months > 0L]        # every site expired: nothing to exclude
  if (!nrow(sites)) return(rows[0L, .(id, coverage, dec = code)])
  per_site <- sites[, .(months = max(months)), by = .(id, coverage, site)]
  setorder(per_site, id, coverage, site)   # canonical, deterministic site order
  built <- per_site[, .(n_site       = .N,
                        class_output = paste(sprintf("%s%s(%s)", letter, site, .months_str(months)),
                                             collapse = ",")),
                    by = .(id, coverage)]
  built[, .(id, coverage, dec = .apply_band(n_site, class_output, bands, letter))]
}

# loading: sum the indices; the sum is what the loading bands on. `letter` is the
# company's loading code letter (e.g. "E").
.combine_loading <- function(rows, bands, letter) {
  if (!nrow(rows)) return(rows[, .(id, coverage, dec = code)])
  rows[, index := as.integer(sub(sprintf("^%s\\(([0-9]+)\\).*$", letter), "\\1", code))]
  rows[is.na(index), index := 0L]
  totals <- rows[, .(total = sum(index)), by = .(id, coverage)]
  totals[, .(id, coverage,
             dec = .apply_band(total, sprintf("%s(%d)", letter, total), bands, letter))]
}

# reduction: resolve the period, keep the longest. `letter` is the company's
# reduction code letter (e.g. "L").
.combine_reduction <- function(rows, period_table, letter) {
  if (!nrow(rows)) return(rows[, .(id, coverage, dec = code)])
  rows[, mark   := sub(sprintf("^%s\\((.*)\\)$", letter), "\\1", code)]
  rows[, months := .resolve_months(mark, elp_day, period_table)]
  kept <- rows[!is.na(months) & months > 0L]           # every reduction expired
  if (!nrow(kept)) return(rows[0L, .(id, coverage, dec = code)])
  kept[, .(dec = sprintf("%s(%s)", letter, .months_str(max(months)))), by = .(id, coverage)]
}

# Build one decision per (id, coverage) out of the combiners' results.
#
# The combiner says how several diseases' codes of one class merge; the role says
# how the classes meet each other. A terminal code -- decline or underwriter --
# stands alone, suppressing every restriction on the cell, and the worst of two
# terminals wins. Otherwise the standard code is the identity and drops out, and
# whatever is left composes into one comma-joined decision written in the decision
# table's own row order. A cell that resolved to nothing (every exclusion expired,
# say) falls back to standard.
#
# Terminality is read off the combiners' OUTPUT, not their input: a loading whose
# summed index reaches a decline band, and an exclusion whose site count does, both
# emit a terminal code from a non-terminal input.
#
# `priority` only ranks the terminals against each other here. The composed order
# is `sheet_ord`, the decision table's row order, so two composing codes may hold
# the same priority number without their output order becoming arbitrary.
.compose_decision <- function(results, letter, priority, all_pairs) {
  results[, class_letter := substr(dec, 1L, 1L)]
  results[, rank := priority[class_letter]]
  results[is.na(rank), rank := max(priority) + 1L]
  results[, has_terminal := any(class_letter %in% letter$terminal), by = .(id, coverage)]

  setorder(results, id, coverage, rank)
  alone <- results[class_letter %in% letter$terminal, .(dec = dec[1L]), by = .(id, coverage)]

  rest <- results[has_terminal == FALSE & class_letter != letter$standard]
  rest[, pos := letter$sheet_ord[class_letter]]
  rest[is.na(pos), pos := length(letter$sheet_ord) + 1L]   # a code the table omits sorts last
  setorder(rest, id, coverage, pos)
  composed <- rest[, .(dec = paste(dec, collapse = ",")), by = .(id, coverage)]

  final <- rbind(alone, composed)
  final <- final[all_pairs, on = .(id, coverage)]
  final[is.na(dec), dec := letter$standard]
  dcast(final, id ~ coverage, value.var = "dec")
}

# A period mark is "5i" (5 years minus elapsed), "3" (3 years fixed), or "99"
# (whole period). Resolve it to a month count given the elapsed days; a result
# <= 0 means the restriction has expired. 30 days = 1 month, truncated.
.resolve_months <- function(mark, elapsed_days, period_table) {
  elapsed   <- as.integer(elapsed_days %/% 30L)
  base_year <- suppressWarnings(as.integer(sub("i", "", mark)))   # "5i"/"3" -> 5/3
  months    <- fifelse(grepl("i$", mark), base_year * 12L - elapsed, base_year * 12L)
  months[mark == "99"] <- 9999L                       # "99" = whole period
  months[!mark %in% period_table$mark] <- NA_integer_ # mark not in the table = invalid
  months
}
.months_str <- function(months) fifelse(months >= 9999L, "99", as.character(months))
