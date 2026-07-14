#' Diagnose authoring defects in a rule set
#'
#' Statically validates a rule set -- no claim data required -- for the authoring
#' errors that [match_rule()] can only surface at run time, and only once some
#' input happens to land in the offending band. A rule whose bands are never hit
#' by the data at hand looks clean to [match_rule()] yet is still defective; this
#' catches it up front.
#'
#' Only `decl_yn == 0` rows are examined, since those are the rows [match_rule()]
#' applies. The band overlap and duplicate tests use the four bands
#' [match_rule()] joins on (`age`, `elp_day`, `sur_cnt`, `hos_day`); `out_day` is
#' carried but never joined on, so a non-trivial `out_day` band counts as a
#' shadow condition rather than a real constraint.
#'
#' Sections:
#' \describe{
#'   \item{`shadow_condition`}{`decl_yn == 0` rows conditioned on a fact
#'     [match_rule()] cannot see -- a non-`*` declaration/exam attribute
#'     (`recover`, `recur`, `treat`, `severe`, `cause`, `medical_checkup`) or a
#'     constrained `out_day` band. The row is silently broader than the author
#'     intended: the engine applies it to inputs the condition was meant to
#'     exclude. Such a row belongs on `decl_yn == 1` (or the distinction must move
#'     into a band the feed carries).}
#'   \item{`latent_conflict`}{pairs of `decl_yn == 0` rules within one `kcd_main`
#'     whose bands overlap and whose decisions disagree -- the static form of
#'     [match_rule()]'s run-time `conflict`, found without needing an input to hit
#'     the overlap. A pair that also differs in a shadow condition is flagged
#'     `shadow_explained`: the disagreement is legitimate but unresolvable by the
#'     engine (fix via `shadow_condition`). A pair that does not is a genuine
#'     structural conflict -- same observable and unobservable conditions, two
#'     answers.}
#'   \item{`exact_duplicate`}{`decl_yn == 0` rows identical in their bands and
#'     every decision -- redundant rows that inflate [match_rule()]'s
#'     `multi_matched` without changing any outcome.}
#'   \item{`no_auto_rule`}{`kcd_main` present in the rule set but with no
#'     `decl_yn == 0` row, so every input for that disease is `unmatched` and
#'     forced to the underwriter -- the static form of [match_rule()]'s
#'     `unmatched`, for diseases the rule set does carry (a disease absent from
#'     the rule set entirely is a mapping gap, not this).}
#'   \item{`missing_sentinel`}{the pipeline's sentinel codes (`VACANT`,
#'     `IRREGULAR`, `UNMAPPED`, `EXPIRED`) that have no `decl_yn == 0` rule.
#'     Every insured resolves onto one of these when they have no reviewable
#'     diagnosis, so a missing sentinel row sends all such insured to
#'     `unmatched`. Unlike `no_auto_rule` this checks the fixed sentinel set
#'     directly, catching the case where the rule set omits a sentinel row
#'     entirely -- which `no_auto_rule` cannot see, as it only scans codes the
#'     sheet already lists.}
#' }
#'
#' @param ruleset A rule-set `data.table` (or coercible), the same table
#'   [match_rule()] consumes.
#' @param decision_cols The coverage decision columns; by default every column
#'   not in the fixed non-decision set (`.NON_DECISION_COLS`). Pass explicitly
#'   when the rule set carries extra attribute columns, exactly as for
#'   [match_rule()].
#' @param verbose If `TRUE` (default) print a report; the list is always returned
#'   invisibly.
#' @return Invisibly, a named list with `n_rule`, `n_kcd`, `n_auto` (the
#'   `decl_yn == 0` row count) and the five sections above.
#' @seealso [match_rule()], [diagnose_icis()].
#' @export
diagnose_ruleset <- function(ruleset,
                             decision_cols = setdiff(names(ruleset), .NON_DECISION_COLS),
                             verbose = TRUE) {
  rs <- as.data.table(copy(ruleset))
  if (!nrow(rs)) stop("`ruleset` has no rows to diagnose.")
  decision_cols <- intersect(decision_cols, names(rs))
  if (!length(decision_cols)) stop("no decision columns found in `ruleset`.")

  n_rule <- nrow(rs)
  n_kcd  <- uniqueN(rs$kcd_main)
  auto   <- rs[decl_yn == 0L]

  band_lo <- c("age_min", "elp_day_min", "sur_cnt_min", "hos_day_min")
  band_hi <- c("age_max", "elp_day_max", "sur_cnt_max", "hos_day_max")

  # --- shadow_condition -----------------------------------------------------
  # a decl_yn==0 row that constrains a fact match_rule() does not join on.
  sh_cols <- intersect(.SHADOW_COND_COLS, names(auto))
  # per shadow column, the name where the cell is a real (non-wildcard) condition
  flags <- lapply(sh_cols, function(col) fifelse(.is_condition(auto[[col]]), col, NA_character_))
  names(flags) <- sh_cols
  # out_day is a band match_rule() carries but never joins on; a non-full range
  # is therefore just as invisible to the engine as a declaration attribute.
  if (all(c("out_day_min", "out_day_max") %in% names(auto))) {
    outday <- (auto$out_day_min > 0L) | (auto$out_day_max < 9999L)
    outday[is.na(outday)] <- FALSE
    flags[["out_day"]] <- fifelse(outday, "out_day", NA_character_)
  }
  flag_dt  <- as.data.table(flags)
  row_cols <- apply(flag_dt, 1L, function(v) paste(v[!is.na(v)], collapse = ";"))
  sh_rows  <- data.table(kcd_main = auto$kcd_main, no = auto$no, cols = row_cols)[nzchar(cols)]
  by_col   <- vapply(flag_dt, function(v) sum(!is.na(v)), integer(1L))
  by_col   <- by_col[by_col > 0L]
  sh_by_kcd <- sh_rows[, .(
    no   = paste(no, collapse = ","),
    cols = paste(sort(unique(unlist(strsplit(cols, ";", fixed = TRUE)))), collapse = ";")
  ), by = kcd_main]
  shadow_condition <- list(
    n_row  = nrow(sh_rows),
    n_kcd  = uniqueN(sh_rows$kcd_main),
    by_col = by_col,
    by_kcd = sh_by_kcd,
    rows   = sh_rows
  )

  # --- latent_conflict ------------------------------------------------------
  # decl_yn==0 rows co-hit any input whose bands lie in the overlap; if their
  # decisions differ that input has no single answer. Pairwise within kcd_main
  # (groups are small); overlap in every joined band = the four min<=max tests.
  as_char <- function(dt, cols) as.matrix(dt[, lapply(.SD, function(v) {
    v <- as.character(v); v[is.na(v)] <- ""; v
  }), .SDcols = cols])
  pairs <- list()
  for (k in auto[, unique(kcd_main)]) {
    g <- auto[kcd_main == k]
    m <- nrow(g)
    if (m < 2L) next
    lo   <- as.matrix(g[, ..band_lo]); hi <- as.matrix(g[, ..band_hi])
    decm <- as_char(g, decision_cols)
    shm  <- if (length(sh_cols)) as_char(g, sh_cols) else NULL
    for (i in seq_len(m - 1L)) for (j in (i + 1L):m) {
      if (all(lo[i, ] <= hi[j, ] & lo[j, ] <= hi[i, ]) && any(decm[i, ] != decm[j, ])) {
        shadow_expl <- !is.null(shm) && any(shm[i, ] != shm[j, ])
        pairs[[length(pairs) + 1L]] <- list(kcd_main = k, no_a = g$no[i], no_b = g$no[j],
                                             shadow_explained = shadow_expl)
      }
    }
  }
  lc <- if (length(pairs)) rbindlist(pairs)
        else data.table(kcd_main = character(), no_a = numeric(), no_b = numeric(),
                        shadow_explained = logical())
  lc_by_kcd <- lc[, .(pairs = .N, genuine = sum(!shadow_explained)), by = kcd_main]
  latent_conflict <- list(
    n_pair    = nrow(lc),
    n_kcd     = uniqueN(lc$kcd_main),
    n_genuine = sum(!lc$shadow_explained),
    by_kcd    = lc_by_kcd,
    pairs     = lc
  )

  # --- exact_duplicate ------------------------------------------------------
  # identical in every joined band and every decision -> engine-equivalent rows.
  dup_key <- c("kcd_main", band_lo, band_hi, decision_cols)
  grp     <- auto[, .N, by = dup_key][N > 1L]
  dup_by_kcd <- grp[, .(rows = sum(N), groups = .N), by = kcd_main]
  exact_duplicate <- list(
    n_group = nrow(grp),
    n_extra = if (nrow(grp)) sum(grp$N - 1L) else 0L,
    by_kcd  = dup_by_kcd
  )

  # --- no_auto_rule ---------------------------------------------------------
  no_auto <- setdiff(rs[, unique(kcd_main)], auto[, unique(kcd_main)])
  no_auto_rule <- list(n_kcd = length(no_auto), kcds = no_auto)

  # --- missing_sentinel -----------------------------------------------------
  # the four sentinel codes the pipeline emits must each carry a decl_yn==0 rule.
  # Unlike no_auto_rule they may be absent from the sheet entirely, so checking
  # "present but no auto rule" misses them; check the fixed set directly. This is
  # the blind spot that let a rule set drop its sentinel catch-all rows silently.
  missing <- setdiff(.KCD_SENTINELS, auto[, unique(kcd_main)])
  missing_sentinel <- list(n_kcd = length(missing), kcds = missing)

  out <- list(
    n_rule           = n_rule,
    n_kcd            = n_kcd,
    n_auto           = nrow(auto),
    shadow_condition = shadow_condition,
    latent_conflict  = latent_conflict,
    exact_duplicate  = exact_duplicate,
    no_auto_rule     = no_auto_rule,
    missing_sentinel = missing_sentinel
  )
  if (verbose) .print_diagnose_ruleset(out)
  invisible(out)
}

# columns that condition which inputs a rule applies to but that match_rule()
# does NOT join on: the declaration/exam attributes (a claim feed does not carry
# them). A decl_yn == 0 row that sets any to a non-`*` value is silently broader
# in match_rule() than intended. (out_day, also unjoined, is handled inline.)
.SHADOW_COND_COLS <- c("recover", "recur", "treat", "severe", "cause", "medical_checkup")

# TRUE where a cell is a real condition, not the wildcard: for character columns
# non-empty and not "*"; for anything else (e.g. logical medical_checkup) simply
# present.
.is_condition <- function(x) {
  if (is.character(x)) !is.na(x) & nzchar(trimws(x)) & x != "*" else !is.na(x)
}

# Print the diagnose_ruleset report in the diagnose_icis() house style: an
# "== section ==" header, then aligned "  <label> : <value>" lines.
.print_diagnose_ruleset <- function(out) {
  .comma  <- function(x) format(x, big.mark = ",")
  .line   <- function(label, value) cat(sprintf("  %-28s : %s\n", label, value))
  .header <- function(title) cat(sprintf("\n== %s ==\n", title))
  .cap    <- function(dt, n = 20L) if (nrow(dt) > n) head(dt, n) else dt
  .more   <- function(dt, n = 20L) if (nrow(dt) > n) cat(sprintf("  ... %s more kcd_main\n", .comma(nrow(dt) - n)))

  cat(sprintf("n_rule=%s | n_kcd=%s | decl_yn==0 rows=%s\n",
              .comma(out$n_rule), .comma(out$n_kcd), .comma(out$n_auto)))

  sc <- out$shadow_condition
  .header("shadow_condition (decl_yn==0 rows conditioned on a fact match_rule ignores)")
  .line("rows", sprintf("%s across %s kcd_main", .comma(sc$n_row), .comma(sc$n_kcd)))
  for (nm in names(sc$by_col)) .line(paste0("by ", nm), .comma(sc$by_col[[nm]]))
  for (i in seq_len(nrow(.cap(sc$by_kcd)))) .line(sc$by_kcd$kcd_main[i],
        sprintf("rule no %s (%s)", sc$by_kcd$no[i], sc$by_kcd$cols[i]))
  .more(sc$by_kcd)

  lc <- out$latent_conflict
  .header("latent_conflict (decl_yn==0 rules whose bands overlap and decisions disagree)")
  .line("conflicting pairs", sprintf("%s across %s kcd_main", .comma(lc$n_pair), .comma(lc$n_kcd)))
  .line("genuine (not shadow-driven)", .comma(lc$n_genuine))
  for (i in seq_len(nrow(.cap(lc$by_kcd)))) .line(lc$by_kcd$kcd_main[i],
        sprintf("%s pairs (%s genuine)", lc$by_kcd$pairs[i], lc$by_kcd$genuine[i]))
  .more(lc$by_kcd)

  ed <- out$exact_duplicate
  .header("exact_duplicate (decl_yn==0 rows identical in bands and decisions)")
  .line("duplicate groups", sprintf("%s (%s redundant rows)", .comma(ed$n_group), .comma(ed$n_extra)))
  for (i in seq_len(nrow(.cap(ed$by_kcd)))) .line(ed$by_kcd$kcd_main[i],
        sprintf("%s rows in %s group(s)", ed$by_kcd$rows[i], ed$by_kcd$groups[i]))
  .more(ed$by_kcd)

  na <- out$no_auto_rule
  .header("no_auto_rule (kcd_main with no decl_yn==0 rule -> every input unmatched)")
  .line("kcd_main", .comma(na$n_kcd))
  if (na$n_kcd) .line("e.g.", paste(head(na$kcds, 15L), collapse = ", "))

  ms <- out$missing_sentinel
  .header("missing_sentinel (pipeline sentinel codes with no decl_yn==0 rule)")
  .line("kcd_main", .comma(ms$n_kcd))
  if (ms$n_kcd) .line("missing", paste(ms$kcds, collapse = ", "))
}
