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
#' Two of those merges leave behind an amount the rule set then bands. The
#' exclusion counts distinct sites and declines a coverage once they exceed the
#' `max_sites` its `decision_table` row carries. The loading sums its indices and
#' looks the sum up in `loading_table`: the band it lands in either keeps the
#' loading the class wrote for itself, when its `decision` is the bare loading
#' letter, or substitutes another code -- `E` at `at_least = 0` writes `E(75)`,
#' while `U` at `at_least = 50` refers that sum to the underwriter instead.
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
#' A decision code nothing can read -- a class letter absent from `decision_table`, a
#' malformed exclusion token, a period mark absent from its table, a loading with no
#' numeric index -- refers its coverage to the underwriter, and never quietly stands
#' the coverage instead. Expiry and unreadability look alike once a code has been
#' dropped, so the whole code vocabulary is judged before any merging: an exclusion
#' that ran out leaves the coverage standard, while one nobody can read escalates it.
#' Every such code is counted in the `"unresolved"` attribute and reported in a single
#' warning naming the rule rows that wrote it, so a rule-set typo surfaces instead of
#' turning into a silent automatic acceptance.
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
#'   diseases are referred there), `auto` (`1`/`0`, read by [tabulate_decision()]
#'   and `plot()` to flag which codes count as automatic), and `max_sites` (the
#'   distinct exclusion sites a coverage tolerates before it declines; required on
#'   the exclusion code's row, blank on every other, as `role` is). Its row order is
#'   the order the composed codes are written in.
#' @param exclusion_table,reduction_table Period-code tables listing the valid
#'   `mark`s (`"5i"` = 5 years minus elapsed, `"3"` = 3 years, `"99"` = whole
#'   period); the period logic is parsed from the mark itself.
#' @param loading_table Columns `at_least`, `decision`. A staircase over the summed
#'   loading index: a row claims every sum from its `at_least` (inclusive) up to the
#'   next row's, the last row runs to infinity, and the first must start at `0`. A
#'   `decision` holding the bare loading letter keeps the loading itself, so the band
#'   reads `E(75)`; any other value substitutes for it, so `"U"` or `"D"` escalates
#'   the coverage rather than loading it.
#' @param decision_cols Coverage decision columns (default: the `"decision_cols"`
#'   attribute set by [match_rule()]).
#' @return A wide `data.table`, one row per `id`, one column per coverage. The
#'   four supplied tables ride along as attributes (`decision_table`,
#'   `exclusion_table`, `reduction_table`, `loading_table`), together with
#'   `decision_cols` (the rule-set coverage order, which the `id ~ coverage`
#'   reshape would otherwise sort away), so downstream summaries such as
#'   [tabulate_decision()] and `plot()`, and the functions that recombine
#'   ([trace_decision()], [relax_rule()]), can recover them without re-passing.
#'   The `"unresolved"` attribute holds one row per unreadable decision code --
#'   `code`, `reason`, the `rule_no` and `kcd_main` that wrote it, and the `n_id` /
#'   `n_cell` it reached -- or `NULL` when every code read cleanly.
#' @export
combine_decision <- function(applied, decision_table, exclusion_table, reduction_table, loading_table,
                             decision_cols = attr(applied, "decision_cols")) {
  .check_decision_table(decision_table)
  priority <- setNames(as.integer(decision_table$priority), decision_table$code)
  combiner <- setNames(decision_table$combiner, decision_table$code)
  letter   <- .decision_letters(decision_table, priority)
  if (is.na(letter$underwriter))
    stop("`decision_table` needs a row with role == \"underwriter\"; unmatched diseases are referred there.")
  max_sites    <- .max_sites(decision_table, letter$exclusion)
  loading_band <- .check_loading_table(loading_table, letter$loading, decision_table,
                                       combiner, exclusion_table, reduction_table)

  melted <- .melt_decisions(applied, decision_cols, combiner, letter$underwriter)

  # judge the whole code vocabulary once, then keep the unreadable ones away from the
  # combiners: their cells are referred, and the underwriter code being terminal
  # suppresses whatever else those cells carried.
  unreadable <- .unresolvable(unique(melted$code), decision_table, combiner,
                              exclusion_table, reduction_table)
  melted[, reason := NA_character_]
  if (nrow(unreadable)) melted[unreadable, on = .(code), reason := i.reason]
  referred <- melted[!is.na(reason)]
  readable <- melted[ is.na(reason)]

  results <- rbindlist(list(
    unique(referred[, .(id, coverage)])[, dec := letter$underwriter],
    .combine_priority( readable[method == "priority"] , priority),
    .combine_exclusion(readable[method == "exclusion"], exclusion_table, max_sites,
                       letter$exclusion, letter$decline),
    .combine_loading(  readable[method == "loading"]  , loading_band   , letter$loading),
    .combine_reduction(readable[method == "reduction"], reduction_table, letter$reduction)
  ), use.names = TRUE)

  combined <- .compose_decision(results, letter, priority, unique(melted[, .(id, coverage)]))

  report <- if (nrow(referred)) .unresolved_report(referred) else NULL
  setattr(combined, "decision_table",  decision_table)
  setattr(combined, "exclusion_table", exclusion_table)
  setattr(combined, "reduction_table", reduction_table)
  setattr(combined, "loading_table",   loading_table)
  setattr(combined, "decision_cols",   decision_cols)   # rule-set coverage order, for plot()
  setattr(combined, "unresolved",      report)          # NULL when every code read cleanly
  setattr(combined, "class", c("combined_decision", "data.table", "data.frame"))
  if (!is.null(report)) .warn_unresolved(report)
  combined
}

# One row per unresolvable code: why it failed, which rules wrote it, which diseases
# carried it, and how far it reached. `no` and `kcd_main` are there only when
# `applied` came from match_rule(); without them the report still names the code and
# the reason, it just cannot point at the rule row to fix.
.unresolved_report <- function(referred) {
  report <- referred[, .(n_id = uniqueN(id), n_cell = .N), by = .(code, reason)]
  if ("no" %in% names(referred))
    report[referred[, .(rule_no = .abbreviate(unique(no))), by = code], on = .(code), rule_no := i.rule_no]
  if ("kcd_main" %in% names(referred))
    report[referred[, .(kcd_main = .abbreviate(unique(kcd_main))), by = code], on = .(code), kcd_main := i.kcd_main]
  setcolorder(report, intersect(c("code", "reason", "rule_no", "kcd_main", "n_id", "n_cell"), names(report)))
  setorder(report, -n_cell)
  report[]
}

# A comma-joined sample of the values, so one bad code spread over hundreds of rules
# still reports in one line.
.abbreviate <- function(x, max_shown = 5L) {
  x <- sort(x[!is.na(x)])
  if (length(x) > max_shown)
    sprintf("%s, +%d more", paste(head(x, max_shown), collapse = ","), length(x) - max_shown)
  else paste(x, collapse = ",")
}

# One warning for the whole call. Raised here rather than inside a combiner so that R
# names `combine_decision()` as the offending call, and once rather than per code so
# that the default `warn = 0` does not collapse them into "there were N warnings".
.warn_unresolved <- function(report) {
  shown  <- head(report, 5L)
  suffix <- if ("rule_no" %in% names(report)) sprintf("  (rule no: %s)", shown$rule_no) else ""
  lines  <- sprintf("    %-14s %s%s", shown$code, shown$reason, suffix)
  if (nrow(report) > nrow(shown))
    lines <- c(lines, sprintf("    ... and %d more", nrow(report) - nrow(shown)))
  plural <- function(n, word) sprintf("%s %s%s", format(n, big.mark = ","), word, if (n == 1L) "" else "s")
  warning(sprintf("%s could not be resolved; %s referred to the underwriter.\n%s\n  Full detail in attr(combined, \"unresolved\").",
                  plural(nrow(report), "decision code"),
                  plural(sum(report$n_cell), "cell"),
                  paste(lines, collapse = "\n")))
}

# The site cap the exclusion code's `decision_table` row carries. Blank on every
# other row, the way `role` is: a code that is not an exclusion has no sites to cap.
# The exclusion's own row must state it, though -- a blank there reads as either "no
# sites allowed" or "no cap", and guessing either way decides real coverages. Write a
# large number to mean no cap, as the period marks do.
.max_sites <- function(decision_table, letter) {
  if (is.na(letter)) return(NA_integer_)          # the rule set writes no exclusions
  if (!"max_sites" %in% names(decision_table))
    stop("`decision_table` needs a `max_sites` column; the exclusion code's row carries the site cap.")
  cap <- as.integer(decision_table$max_sites[decision_table$code == letter])
  if (!length(cap) || is.na(cap[1L]) || cap[1L] < 1L)
    stop(sprintf("`decision_table` needs a positive `max_sites` on the exclusion code \"%s\"; write a large number for no cap.",
                 letter))
  cap[1L]
}

# The loading bands, ordered and checked. The staircase has to start at 0, because a
# summed index below the first band would land `findInterval()` on 0 and index the
# decision vector out of existence. Every band's decision has to be a code the rest
# of the engine can read -- unless it is the bare loading letter, which is the
# sentinel for "keep the loading itself" rather than a code at all.
.check_loading_table <- function(loading_table, letter, decision_table, combiner,
                                 exclusion_table, reduction_table) {
  bands <- as.data.table(loading_table)
  if (is.na(letter)) return(bands)                # the rule set writes no loadings
  if (!all(c("at_least", "decision") %in% names(bands)))
    stop("`loading_table` needs `at_least` and `decision` columns.")
  if (!nrow(bands) || anyNA(bands$at_least))
    stop("`loading_table` needs at least one band, and no `at_least` may be missing.")
  if (anyDuplicated(bands$at_least))
    stop("`loading_table` has duplicate `at_least` bounds.")
  bands <- bands[order(at_least)]
  if (bands$at_least[1L] != 0L)
    stop(sprintf("`loading_table`'s first `at_least` must be 0, not %s; a summed index below the first band has nowhere to land.",
                 bands$at_least[1L]))
  substituted <- setdiff(bands$decision, letter)   # the bare letter is a sentinel
  unreadable  <- .unresolvable(substituted, decision_table, combiner, exclusion_table, reduction_table)
  if (nrow(unreadable))
    stop(sprintf("`loading_table` band decision \"%s\" cannot be read: %s.",
                 unreadable$code[1L], unreadable$reason[1L]))
  bands
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
#
# `kcd_main` and the rule row `no` come along when `applied` carries them, so a code
# that turns out to be unresolvable can be traced back to the rule that wrote it.
# A hand-built `applied` may have neither, and the rest of the pipeline does not
# depend on them.
.melt_decisions <- function(applied, decision_cols, combiner, underwriter) {
  applied <- as.data.table(copy(applied))
  applied[matched == 0L, (decision_cols) := underwriter]
  carry  <- intersect(c("id", "elp_day", "kcd_main", "no"), names(applied))
  melted <- melt(applied, id.vars = carry, measure.vars = decision_cols,
                 variable.name = "coverage", value.name = "code", variable.factor = FALSE)
  melted <- melted[!is.na(code) & nzchar(code)]
  melted[, method := combiner[substr(code, 1L, 1L)]]
  melted[is.na(method), method := "priority"]
  melted[]
}

# The decision table itself has to be interpretable before any decision can be. A
# class letter is read off a code's first character and spliced into a regular
# expression, so a two-character code would route to the wrong combiner without a
# word and a metacharacter would corrupt the pattern. Neither is a defect one insured
# can be referred for -- the whole table is unusable -- so this stops.
.REGEX_METACHARACTERS <- c(".", "^", "$", "*", "+", "?", "(", ")", "[", "]", "{", "}", "|", "\\")
.COMBINERS <- c("priority", "exclusion", "loading", "reduction")
.check_decision_table <- function(decision_table) {
  code <- decision_table$code
  quote_all <- function(x) paste0("\"", x, "\"", collapse = ", ")
  wide <- unique(code[nchar(code) != 1L])
  if (length(wide))
    stop(sprintf("`decision_table` codes must be one character; found %s.", quote_all(wide)))
  meta <- unique(code[code %in% .REGEX_METACHARACTERS])
  if (length(meta))
    stop(sprintf("`decision_table` codes must not be regular-expression metacharacters; found %s.",
                 quote_all(meta)))
  duplicated_code <- unique(code[duplicated(code)])
  if (length(duplicated_code))
    stop(sprintf("`decision_table` has duplicate codes: %s.", quote_all(duplicated_code)))
  unknown <- unique(decision_table$combiner[!decision_table$combiner %in% .COMBINERS])
  if (length(unknown))
    stop(sprintf("`decision_table` combiners must be one of %s; found %s.",
                 quote_all(.COMBINERS), quote_all(unknown)))
  invisible(TRUE)
}

# Which of these decision codes cannot be interpreted at all, and why?
#
# Resolvability depends only on the code text and the config tables. It does not
# depend on the elapsed days, which decide *expiry*, and it does not depend on how
# several diseases merge -- so the whole vocabulary is judged once, up front, and the
# combiners never see a code they cannot read. Expiry and unreadability then stay
# apart: an exclusion that ran out leaves the coverage standard, while one nobody can
# read refers it, which is the only safe direction to fail in.
#
# The payload of a `priority` class (a limit code, a diagnosis code) is free text with
# no syntax of its own, so only its class letter is checked.
.unresolvable <- function(codes, decision_table, combiner, exclusion_table, reduction_table) {
  reason <- vapply(codes, function(code) {
    letter <- substr(code, 1L, 1L)
    if (!letter %in% decision_table$code)
      return(sprintf("class letter \"%s\" is not in decision_table", letter))
    switch(combiner[[letter]],
      exclusion = {
        token  <- strsplit(code, ",", fixed = TRUE)[[1L]]
        shaped <- grepl(sprintf("^%s[0-9]+\\(.*\\)$", letter), token)
        if (!all(shaped))
          return(sprintf("\"%s\" is not of the form %s<site>(<mark>)", token[!shaped][1L], letter))
        mark    <- sub(sprintf("^%s[0-9]+\\((.*)\\)$", letter), "\\1", token)
        unknown <- setdiff(mark, exclusion_table$mark)
        if (length(unknown))
          return(sprintf("mark \"%s\" is not in exclusion_table", unknown[1L]))
        ""
      },
      reduction = {
        if (!grepl(sprintf("^%s\\(.*\\)$", letter), code))
          return(sprintf("\"%s\" is not of the form %s(<mark>)", code, letter))
        mark <- sub(sprintf("^%s\\((.*)\\)$", letter), "\\1", code)
        if (!mark %in% reduction_table$mark)
          return(sprintf("mark \"%s\" is not in reduction_table", mark))
        ""
      },
      loading = {
        if (!grepl(sprintf("^%s\\([0-9]+\\)$", letter), code))
          return(sprintf("\"%s\" does not carry a numeric index", code))
        ""
      },
      ""   # priority: the payload has no syntax to check
    )
  }, character(1L), USE.NAMES = FALSE)
  data.table(code = codes, reason = reason)[nzchar(reason)]
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
# longest per site, drop the expired ones, and rebuild. An insured whose exclusions
# sprawl over more than `max_sites` distinct sites is declined instead. `letter` is
# the company's exclusion code letter (e.g. "R"); `decline` its decline code.
.combine_exclusion <- function(rows, period_table, max_sites, letter, decline) {
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
  built <- per_site[, .(n_site = .N,
                        dec    = paste(sprintf("%s%s(%s)", letter, site, .months_str(months)),
                                       collapse = ",")),
                    by = .(id, coverage)]
  built[n_site > max_sites, dec := decline]
  built[, .(id, coverage, dec)]
}

# loading: sum the indices, then read the sum off the bands. A band whose `decision`
# is the bare loading letter carries the sum back into the code ("E" -> "E(75)");
# any other value substitutes for it outright ("U", "D", or a fixed "E(50)"). So one
# table expresses both a loading the engine writes itself and a threshold that
# escalates the whole coverage. `letter` is the company's loading code letter.
#
# `.check_loading_table()` has already made the first band start at 0, so a summed
# index -- never negative -- always lands on a band and `findInterval()` never
# returns 0.
.combine_loading <- function(rows, bands, letter) {
  if (!nrow(rows)) return(rows[, .(id, coverage, dec = code)])
  rows[, index := as.integer(sub(sprintf("^%s\\(([0-9]+)\\).*$", letter), "\\1", code))]
  rows[is.na(index), index := 0L]
  totals <- rows[, .(total = sum(index)), by = .(id, coverage)]
  band <- bands$decision[findInterval(totals$total, bands$at_least)]
  totals[, .(id, coverage,
             dec = fifelse(band == letter, sprintf("%s(%d)", letter, total), band))]
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
