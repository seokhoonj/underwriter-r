# Static defects in a simplified-issue rule set. The counterpart of
# diagnose_ruleset(), but it looks for different defects because the rule sets
# are shaped differently: the standard set can match one disease against several
# overlapping bands, so its worst failure is a latent conflict between them. The
# SI key (si_type, coverage, kcd_main) is unique, so conflicts cannot arise -- but
# a band that can never fire, a disease absent on one coverage, or a sentinel code
# the sheet forgot can. Every check answers one question: would a reader of the
# sheet expect this?

#' Diagnose authoring defects in a simplified-issue rule set
#'
#' The simplified-issue counterpart of [diagnose_ruleset()]. Runs a battery of
#' static checks over a rulebook -- bands that can never match, sentinel codes the
#' sheet omitted, rules for codes the mapping cannot emit, and so on -- and
#' returns them as one findings list, printing a report when `verbose`.
#'
#' @param rulebook A rulebook from [load_si_rulebook()].
#' @param disease_table The shared `disease` sheet, to confirm every rule's
#'   `kcd_main` is a code the pipeline can actually produce.
#' @param verbose Print a report as well as returning the findings.
#' @return Invisibly, a named list of findings mirroring [diagnose_ruleset()]'s
#'   shape.
#' @seealso [diagnose_ruleset()], [load_si_rulebook()].
#' @export
diagnose_si_ruleset <- function(rulebook, disease_table, verbose = TRUE) {
  si_type <- coverage <- kcd_main <- recover <- N <- NULL              # NSE
  age_min <- age_max <- elp_day_min <- n_cov <- has <- n_kcd <- NULL
  sur_cnt_min <- sur_cnt_max <- hos_day_min <- hos_day_max <- NULL
  critical_disease_set <- class <- NULL

  rs <- as.data.table(copy(rulebook$ruleset))
  cd <- as.data.table(copy(rulebook$critical_disease))
  if (!nrow(rs)) stop("`rulebook$ruleset` has no rows to diagnose.")

  band_lo <- c("age_min", "sur_cnt_min", "hos_day_min")
  band_hi <- c("age_max", "sur_cnt_max", "hos_day_max")

  # band_inversion: a floor above its ceiling can never match, so the disease is
  # silently declined on that coverage no matter the applicant's history.
  inverted <- rbindlist(lapply(seq_along(band_lo), function(i) {
    lo <- band_lo[i]; hi <- band_hi[i]
    rs[get(lo) > get(hi), .(si_type, coverage, kcd_main, band = sub("_min$", "", lo),
                            lo = get(lo), hi = get(hi))]
  }))

  # never_eligible: elp_day_min is one-sided, so a very large floor is how the
  # sheet spells "never acceptable". Not a defect, but it decides the outcome
  # outright and a reader scanning bands can miss it.
  never <- rs[elp_day_min >= 9999L, .(si_type, coverage, kcd_main, elp_day_min)]

  # coverage_reach: how many diseases each (si_type, coverage) carves out. NOT a
  # defect -- coverages deliberately carve out different disease sets -- but a
  # coverage's reach decides how much of the book it declines outright, which is
  # invisible from the rule rows alone.
  reach <- rs[, .(n_kcd = uniqueN(kcd_main)), by = .(si_type, coverage)]
  setorder(reach, si_type, -n_kcd)

  # unmapped_kcd: a rule for a code map_disease() can never emit is dead weight.
  known <- unique(as.data.table(disease_table)$kcd_main)
  unmapped_rule <- setdiff(unique(rs$kcd_main), known)
  unmapped_crit <- setdiff(unique(cd$kcd_main), known)

  # shadow_condition: `recover` (completed-recovery status) applies only with
  # declaration data; an ICIS run never reads it (rows 21, 143). Unlike the
  # standard sheet, where `recover` is text that narrows the rule, SI's is a 0/1
  # flag on an already-unique key, so ignoring it cannot widen a match. Only
  # whether the column is populated matters, so it is counted by value.
  shadow <- rs[, .N, by = recover]
  setorder(shadow, -N)

  # critical_no_rule: a critical disease with no carve-out band is declined the
  # moment it trips Q2. Expected for a genuinely critical code, reported so the
  # expectation is checked rather than assumed.
  crit_codes <- unique(cd[class > 0L, kcd_main])
  crit_no_rule <- setdiff(crit_codes, unique(rs$kcd_main))

  # critical_class_gap: every (set, coverage) should class the same disease list;
  # a code present on one coverage and missing on another is an omission, since a
  # missing row reads as "not critical" rather than as an open question.
  per_set  <- cd[, .(n_cov = uniqueN(coverage)), by = critical_disease_set]
  cd_count <- cd[, .(has = uniqueN(coverage)), by = .(critical_disease_set, kcd_main)]
  class_gap <- cd_count[per_set, on = "critical_disease_set"][has < n_cov]

  # missing_sentinel: map_disease() emits four structural codes; the engine
  # withholds them from the questions and settles them from the sentinel sheet --
  # but only for codes the sheet lists. A code missing from the sheet flows into
  # the questions instead, matches no band, and is declined for having no readable
  # diagnosis. Mirrors the standard path's own missing-sentinel check; a workbook
  # sheet rename is exactly how such a row goes missing.
  sent_codes <- as.data.table(rulebook$sentinel)$kcd_main
  missing_sentinel <- setdiff(.KCD_SENTINELS, sent_codes)

  out <- list(
    n_rule             = nrow(rs),
    n_kcd              = uniqueN(rs$kcd_main),
    n_product          = uniqueN(rs$si_type),
    band_inversion     = list(n = nrow(inverted), rows = inverted),
    never_eligible     = list(n = nrow(never),    rows = never),
    coverage_reach     = list(rows = reach),
    unmapped_kcd       = list(n_rule = length(unmapped_rule), rule = unmapped_rule,
                              n_critical = length(unmapped_crit), critical = unmapped_crit),
    shadow_condition   = list(n_row = nrow(rs),   by_value = shadow),
    critical_no_rule   = list(n = length(crit_no_rule), kcds = crit_no_rule),
    critical_class_gap = list(n = nrow(class_gap), rows = class_gap),
    missing_sentinel   = list(n = length(missing_sentinel), kcds = missing_sentinel)
  )
  if (verbose) .print_diagnose_si_ruleset(out)
  invisible(out)
}


.print_diagnose_si_ruleset <- function(out) {
  si_type <- coverage <- n_kcd <- N <- NULL  # NSE
  .comma  <- function(x) format(x, big.mark = ",")
  .line   <- function(label, value) cat(sprintf("  %-30s : %s\n", label, value))
  .header <- function(title) cat(sprintf("\n== %s ==\n", title))
  .cap    <- function(dt, n = 10L) if (nrow(dt) > n) head(dt, n) else dt

  cat(sprintf("n_rule=%s | n_kcd=%s | n_product=%s\n",
              .comma(out$n_rule), .comma(out$n_kcd), .comma(out$n_product)))

  .header("band_inversion (floor above ceiling -- the rule can never match)")
  .line("rows", .comma(out$band_inversion$n))
  if (out$band_inversion$n) print(.cap(out$band_inversion$rows))

  .header("never_eligible (elp_day_min >= 9999 -- declines outright)")
  .line("rows", .comma(out$never_eligible$n))
  if (out$never_eligible$n)
    .line("by si_type", paste(utils::capture.output(
      out$never_eligible$rows[, .N, si_type])[-1L], collapse = " | "))

  .header("coverage_reach (diseases carved out per coverage -- by design, not a defect)")
  print(dcast(out$coverage_reach$rows, si_type ~ coverage, value.var = "n_kcd", fill = 0))

  .header("unmapped_kcd (code no disease mapping can emit)")
  .line("in ruleset",  .comma(out$unmapped_kcd$n_rule))
  .line("in critical", .comma(out$unmapped_kcd$n_critical))

  .header("shadow_condition (recover carried on every row, never read by an ICIS run)")
  .line("rows", .comma(out$shadow_condition$n_row))
  for (i in seq_len(nrow(out$shadow_condition$by_value)))
    .line(paste0("recover = ", out$shadow_condition$by_value$recover[i]),
          .comma(out$shadow_condition$by_value$N[i]))

  .header("critical_no_rule (critical disease with no carve-out band)")
  .line("kcd_main", .comma(out$critical_no_rule$n))

  .header("critical_class_gap (classed on some coverages of a set, missing on others)")
  .line("rows", .comma(out$critical_class_gap$n))
  if (out$critical_class_gap$n) print(.cap(out$critical_class_gap$rows))

  .header("missing_sentinel (pipeline sentinel code with no ruleset_sentinel row)")
  .line("kcd_main", if (out$missing_sentinel$n)
    paste(out$missing_sentinel$kcds, collapse = ", ") else "0")
  invisible(NULL)
}
