# Regression tests for the simplified-issue engine, on inline synthetic fixtures
# (real claim data is gitignored and cannot be used). The independent, spec-derived
# recomputation that cross-checks the engine on real data lives in dev/verify_si.R;
# these pin the branch behavior a future edit must not silently break.

library(data.table)

test_that("an insured with no triggering history is accepted on every coverage", {
  rb  <- si_fixture_rulebook()
  pr  <- si_product("325", rb)
  # D1 outpatient 400 days ago only: no recent visit, no admission -> no question
  mapped <- si_line("A", "D1", "2025-06-01", elapsed = 400L)
  cb <- combine_si_decision(match_si_rule(mapped, rb, pr), rb, pr)

  expect_setequal(cb$coverage, pr$coverages)
  expect_true(all(cb$dec == "S"))
})

test_that("no insured is left behind: one who trips no question still gets standard per coverage", {
  rb <- si_fixture_rulebook()
  pr <- si_product("325", rb)
  # A trips Q2 (admission over the band); B has only an old outpatient visit that
  # trips nothing -- match_si_rule must carry B through as an explicit standard, so
  # B still appears without any external id list.
  mapped <- rbind(si_line("A", "D1", "2025-06-01", elapsed = 100L, hos = 40L),
                  si_line("B", "D1", "2025-06-01", elapsed = 400L))
  cb <- combine_si_decision(match_si_rule(mapped, rb, pr), rb, pr)

  expect_setequal(unique(cb$id), c("A", "B"))
  expect_true(all(cb[id == "B", dec] == "S"))          # nothing tripped -> standard
  expect_true(all(cb[id == "A", dec] == "D"))          # admission over band -> decline
  expect_equal(nrow(cb), 2L * length(pr$coverages))
})

test_that("Q1 declines a disease seen only within the recent window", {
  rb <- si_fixture_rulebook()
  pr <- si_product("325", rb)
  # two outpatient visits, both inside 3 months -> treatment in progress -> decline
  mapped <- rbind(si_line("A", "D1", "2025-06-01", elapsed = 10L),
                  si_line("A", "D1", "2025-06-01", elapsed = 40L))
  m <- match_si_rule(mapped, rb, pr)

  expect_true(all(m[question == "Q1", dec] == "D"))
  expect_true(all(m[question == "Q1", reason] == "recent_treatment"))
})

test_that("Q1 recent history refers instead of declines on declaration data", {
  rb <- si_fixture_rulebook()
  pr <- si_product("325", rb)
  mapped <- rbind(si_line("A", "D1", "2025-06-01", elapsed = 10L),
                  si_line("A", "D1", "2025-06-01", elapsed = 40L))
  m <- match_si_rule(mapped, rb, pr, source = "declaration")

  expect_true(all(m[question == "Q1", dec] == "U"))
})

test_that("Q2 declines a carved-out disease whose admission overruns the band", {
  rb <- si_fixture_rulebook()
  pr <- si_product("325", rb)
  # D1 admission of 40 days, over hos_day_max = 30 -> hos_day_over decline
  mapped <- si_line("A", "D1", "2025-06-01", elapsed = 100L, hos = 40L)
  m <- match_si_rule(mapped, rb, pr)

  q2 <- m[question == "Q2"]
  expect_true(nrow(q2) > 0L)
  expect_true(all(q2$dec == "D"))
  expect_true(all(q2$reason == "hos_day_over"))
})

test_that("Q2 accepts a carved-out disease within every band", {
  rb <- si_fixture_rulebook()
  pr <- si_product("325", rb)
  # 5-day admission, elapsed 100 (>= 30 floor), age in range -> accept
  mapped <- si_line("A", "D1", "2025-06-01", elapsed = 100L, hos = 5L)
  m <- match_si_rule(mapped, rb, pr)

  expect_true(all(m[question == "Q2", dec] == "S"))
})

test_that("Q2 declines an admission whose elapsed is under the floor", {
  rb <- si_fixture_rulebook()
  pr <- si_product("325", rb)
  # admission only 10 days ago, under elp_day_min = 30 -> elp_day_short
  mapped <- si_line("A", "D1", "2025-06-01", elapsed = 10L, hos = 5L)
  m <- match_si_rule(mapped, rb, pr)

  expect_true(all(m[question == "Q2", reason] == "elp_day_short"))
})

test_that("Q3 maps critical-disease class to S/U/D by treatment type", {
  rb <- si_fixture_rulebook()
  pr <- si_product("325", rb)
  # C1 is class 2 on life, class 3 on care.
  # outpatient-only: class 2 -> refer on life, class 3 -> decline on care
  out_only <- si_line("A", "C1", "2025-06-01", elapsed = 100L)
  m <- match_si_rule(out_only, rb, pr)[question == "Q3"]
  expect_equal(m[coverage == "life", dec], "U")
  expect_equal(m[coverage == "care", dec], "D")
})

test_that("sentinel codes are settled from the sheet, not put to the questions", {
  rb <- si_fixture_rulebook()
  pr <- si_product("325", rb)
  # VACANT -> standard (role), UNMAPPED -> underwriter (role); neither is declined
  mapped <- rbind(si_line("A", "VACANT",   "2025-06-01", elapsed = 10L, hos = 40L),
                  si_line("B", "UNMAPPED", "2025-06-01", elapsed = 10L, hos = 40L))
  m <- match_si_rule(mapped, rb, pr)

  expect_true(all(m$question == "sentinel"))       # no Q1/Q2/Q3 answer
  expect_false(any(m$reason == "no_rule", na.rm = TRUE))
  cb <- combine_si_decision(m, rb, pr)
  expect_true(all(cb[id == "A", dec] == "S"))       # VACANT -> standard
  expect_true(all(cb[id == "B", dec] == "U"))       # UNMAPPED -> underwriter
})

test_that("an insured whose every line is aged out is settled as EXPIRED", {
  rb <- si_fixture_rulebook()
  pr <- si_product("325", rb)
  # every line outside the 5-year window (in_5yr = 0): no reviewable history, so the
  # insured is settled as the EXPIRED sentinel, distinct from an in-window no-trigger
  mapped <- si_line("A", "D1", "2025-06-01", elapsed = 2200L, in_5yr = 0L)
  m <- match_si_rule(mapped, rb, pr)

  expect_true(all(m$kcd_main == "EXPIRED"))          # aged-out sentinel, not a question
  expect_true(all(m$question == "sentinel"))
  cb <- combine_si_decision(m, rb, pr)
  expect_true(all(cb$dec == "S"))                    # EXPIRED role = standard
})

test_that("aged-out (EXPIRED) and in-window no-trigger are distinguishable, both standard", {
  rb <- si_fixture_rulebook()
  pr <- si_product("325", rb)
  # A: all history aged out -> EXPIRED;  B: recent-ish benign visit -> no-trigger presence
  mapped <- rbind(si_line("A", "D1", "2025-06-01", elapsed = 2200L, in_5yr = 0L),
                  si_line("B", "D1", "2025-06-01", elapsed = 200L,  in_5yr = 1L))
  m <- match_si_rule(mapped, rb, pr)

  expect_true(all(m[id == "A", kcd_main] == "EXPIRED"))   # aged out
  expect_true(all(is.na(m[id == "B", question])))          # in-window, no question
  cb <- combine_si_decision(m, rb, pr)
  expect_true(all(cb$dec == "S"))                          # both accept, different reasons
})

test_that("the aged-out (EXPIRED) decision honors the workbook role, not a hardcoded standard", {
  rb <- si_fixture_rulebook()
  rb$sentinel[kcd_main == "EXPIRED", role := "underwriter"]  # route aged-out to review
  pr <- si_product("325", rb)
  mapped <- si_line("A", "D1", "2025-06-01", elapsed = 2200L, in_5yr = 0L)
  cb <- combine_si_decision(match_si_rule(mapped, rb, pr), rb, pr)

  expect_true(all(cb$dec == "U"))                    # follows EXPIRED role = underwriter
})

test_that("an insured with any in-window line is not aged out (EXPIRED needs every line aged)", {
  rb <- si_fixture_rulebook()
  pr <- si_product("325", rb)
  # one aged-out line and one in-window benign line -> max(in_5yr) = 1 -> not EXPIRED
  mapped <- rbind(si_line("A", "D1", "2025-06-01", elapsed = 2200L, in_5yr = 0L),
                  si_line("A", "D1", "2025-06-01", elapsed = 200L,  in_5yr = 1L))
  m <- match_si_rule(mapped, rb, pr)

  expect_false(any(m$kcd_main == "EXPIRED", na.rm = TRUE))  # not aged out
  expect_true(all(is.na(m[id == "A", question])))            # in-window presence
  expect_true(all(combine_si_decision(m, rb, pr)$dec == "S"))
})

test_that("combine_si_decision folds decline over underwriter over standard", {
  rb <- si_fixture_rulebook()
  pr <- si_product("325", rb)
  # one cell carrying all three outcomes at once -> worst (D) wins, with its provenance
  matched <- data.table(
    question = c("Q1", "Q2", "Q3"), id = "A", coverage = "life",
    kcd_main = c("D1", "D1", "C1"),
    dec      = c("S", "U", "D"),
    reason   = c(NA_character_, NA_character_, "critical_disease"))
  cb <- combine_si_decision(matched, rb, pr)

  expect_equal(cb[coverage == "life", dec], "D")            # decline wins the fold
  expect_equal(cb[coverage == "life", question], "Q3")      # provenance follows the winner
  expect_equal(cb[coverage == "life", reason], "critical_disease")
  expect_equal(cb[coverage == "care", dec], "S")            # other coverage grid-fills
})

test_that("Q3 declines an inpatient critical disease", {
  rb <- si_fixture_rulebook()
  pr <- si_product("325", rb)
  # C1 is critical (class 2 on life, 3 on care); a hospitalization declines on both
  mapped <- si_line("A", "C1", "2025-06-01", elapsed = 100L, hos = 10L)
  q3 <- match_si_rule(mapped, rb, pr)[question == "Q3"]

  expect_setequal(q3$coverage, pr$coverages)
  expect_true(all(q3$dec == "D"))
  expect_true(all(q3$reason == "critical_disease"))
})

test_that("tabulate_si_decision and auto_rate summarize the decision distribution", {
  rb <- si_fixture_rulebook()
  pr <- si_product("325", rb)
  mapped <- rbind(si_line("A", "D1", "2025-06-01", elapsed = 100L, hos = 40L),  # decline
                  si_line("B", "D1", "2025-06-01", elapsed = 400L))              # standard
  cb  <- combine_si_decision(match_si_rule(mapped, rb, pr), rb, pr)
  tab <- tabulate_si_decision(cb)

  expect_setequal(names(tab), c("coverage", "decision", "name", "auto", "n", "prop"))
  expect_equal(sum(tab$n), nrow(cb))
  expect_equal(tab[, sum(prop), by = coverage]$V1,
               rep(1, uniqueN(cb$coverage)), tolerance = 1e-9)
  expect_equal(auto_rate(tab)$auto_rate, 100, tolerance = 1e-9)   # D and S both auto
  expect_error(tabulate_si_decision(cb[0L]), "no rows")
})

test_that("list_si_rule_impact and list_si_decline_disease attribute declines", {
  rb <- si_fixture_rulebook()
  pr <- si_product("325", rb)
  mapped <- si_line("A", "D1", "2025-06-01", elapsed = 100L, hos = 40L)  # hos_day_over
  cb  <- combine_si_decision(match_si_rule(mapped, rb, pr), rb, pr)

  imp <- list_si_rule_impact(cb)
  expect_true(all(imp$reason == "hos_day_over"))
  expect_equal(sum(imp$n), nrow(cb[dec == "D"]))
  dd <- list_si_decline_disease(cb)
  expect_equal(dd$kcd_main, "D1")
  expect_equal(sum(dd$n), nrow(cb[dec == "D"]))
})

test_that("trace_si_decision reruns the engine for one insured, one row per coverage", {
  rb <- si_fixture_rulebook()
  pr <- si_product("325", rb)
  mapped <- si_line("A", "D1", "2025-06-01", elapsed = 100L, hos = 40L)
  tr <- trace_si_decision(mapped, "A", rb, pr)

  expect_setequal(tr$coverage, pr$coverages)
  expect_true(all(tr$dec == "D"))
  expect_true("answers" %in% names(tr))
  expect_error(trace_si_decision(mapped, "NOPE", rb, pr), "no claim lines")
})

test_that("a missing band input declines rather than being laundered into accept", {
  rb <- si_fixture_rulebook()
  pr <- si_product("325", rb)
  agg <- data.table(id = "A", kcd_main = "D1", coverage = "life",
                    age = NA_real_, hos_day = 5L, sur_cnt = 0L, elp = 100L, kcd_n = 1L)
  res <- underwriter:::.match_band(agg, rb$ruleset, pr, rb$code)
  expect_equal(res$dec, "D")
  expect_equal(res$reason, "age_out_of_band")
})

test_that("combine_si_decision refuses an answer with no decision", {
  rb <- si_fixture_rulebook()
  pr <- si_product("325", rb)
  bad <- data.table(question = "Q2", id = "A", coverage = "life",
                    kcd_main = "D1", dec = NA_character_, reason = NA_character_)
  expect_error(combine_si_decision(bad, rb, pr), "no decision")
})

test_that("the month window uses calendar arithmetic, not a 30-day approximation", {
  rb <- si_fixture_rulebook()
  pr <- si_product("325", rb)
  # inquiry on a month end; a visit exactly 3 calendar months back is the boundary.
  # 2025-05-31 minus 3 months clamps to 2025-02-28, so a 2025-02-28 visit is recent
  # and a 2025-02-27 visit is not.
  inq <- as.Date("2025-05-31")
  recent  <- si_line("A", "D1", inq, elapsed = as.integer(inq - as.Date("2025-02-28")))
  older_a <- si_line("A", "D1", inq, elapsed = as.integer(inq - as.Date("2025-02-27")))
  m <- match_si_rule(rbind(recent, older_a), rb, pr)
  # one recent (02-28) + one older (02-27): seen both recently and long ago -> defer,
  # not the outright "all recent" decline. So Q1 does not fire recent_treatment.
  expect_false(any(m[question == "Q1", reason] == "recent_treatment", na.rm = TRUE))
  # and it MUST still raise Q1: calendar arithmetic makes the 02-28 visit recent, so
  # the disease defers to the band. A 30-day approximation (05-31 minus ~90 days lands
  # in early March) would leave BOTH visits old, raise nothing, and drop the Q1 rows --
  # so their presence is what distinguishes the two implementations.
  expect_true(any(m$question == "Q1"))
})

test_that("Q2 declines a hospitalized disease that has no carve-out band", {
  rb <- si_fixture_rulebook()
  pr <- si_product("325", rb)
  # K1 is not in the ruleset (only D1 is carved out); a hospitalization on it is
  # not carved out at all -> decline for having no rule.
  mapped <- si_line("A", "K1", "2025-06-01", elapsed = 100L, hos = 5L)
  q2 <- match_si_rule(mapped, rb, pr)[question == "Q2"]

  expect_true(nrow(q2) > 0L)
  expect_true(all(q2$dec == "D"))
  expect_true(all(q2$reason == "no_rule"))
})

test_that("Q2 declines a surgery count over the band ceiling", {
  rb <- si_fixture_rulebook()
  pr <- si_product("325", rb)
  # six distinct surgery dates on D1 -> sur_cnt = 6, over sur_cnt_max = 5
  mapped <- rbindlist(lapply(seq_len(6L), function(k)
    si_line("A", "D1", "2025-06-01", elapsed = 100L + k, sur = 1L)))
  q2 <- match_si_rule(mapped, rb, pr)[question == "Q2"]

  expect_true(all(q2$dec == "D"))
  expect_true(all(q2$reason == "sur_cnt_over"))
})

test_that("Q2 declines when the disease count exceeds the coverage ceiling", {
  rb <- si_fixture_rulebook()
  rb$ruleset[, kcd_max := 1L]                        # ceiling of one distinct disease
  pr <- si_product("325", rb)
  # D1 hospitalized (carved) plus a second distinct disease in the window -> kcd_n = 2
  mapped <- rbind(si_line("A", "D1", "2025-06-01", elapsed = 100L, hos = 5L),
                  si_line("A", "C1", "2025-06-01", elapsed = 100L))
  q2 <- match_si_rule(mapped, rb, pr)[question == "Q2"]

  expect_true(any(q2$reason == "kcd_n_over"))
})

test_that("a product with no inpatient-surgery window (305 family) asks no Q2", {
  rb <- si_fixture_rulebook()
  pr <- si_product("325", rb)
  pr$inpatient_surgery_mon <- NA_integer_           # the 305-family shape
  # a hospitalization that would otherwise decline on Q2
  mapped <- si_line("A", "D1", "2025-06-01", elapsed = 100L, hos = 40L)
  m <- match_si_rule(mapped, rb, pr)

  expect_false("Q2" %in% m$question)                # Q2 is simply not asked
})

test_that("Q1 mixed recent-and-old outpatient history defers to the carve-out band", {
  rb <- si_fixture_rulebook()
  pr <- si_product("325", rb)
  # one visit recent (40 days, inside the 3-month window) and one old (400 days):
  # seen both recently and long ago -> defer. elp_all = 40 >= elp_day_min = 30 and
  # D1 is carved out, so the band accepts.
  mapped <- rbind(si_line("A", "D1", "2025-06-01", elapsed = 40L),
                  si_line("A", "D1", "2025-06-01", elapsed = 400L))
  q1 <- match_si_rule(mapped, rb, pr)[question == "Q1"]

  expect_setequal(q1$coverage, pr$coverages)
  expect_true(all(q1$dec == "S"))                   # deferred, then accepted by the band
})

test_that("underwrite_si carries every raw id through the full pipeline", {
  rb  <- si_fixture_rulebook()
  pr  <- si_product("325", rb)
  # raw claims carry real KCD codes (clean_icis rejects malformed ones as IRREGULAR);
  # the shared disease table maps M511 to the carved-out kcd_main "D1".
  dis <- si_disease_table("M511", "D1")
  # A: a 40-day D1 hospitalization -> Q2 hos_day_over decline.
  # B: only an old outpatient visit -> no question -> standard.
  # C: no claim line reaches the engine with a decision, but is in the raw universe.
  raw <- rbind(si_raw("A", "M511", "2025-06-01", elapsed = 100L, hos = 40L),
               si_raw("B", "M511", "2025-06-01", elapsed = 400L),
               si_raw("C", "M511", "2025-06-01", elapsed = 500L))
  out <- underwrite_si(raw, dis, rb, pr)

  expect_setequal(unique(out$id), c("A", "B", "C"))          # no insured left behind
  expect_equal(nrow(out), 3L * length(pr$coverages))         # one row per id x coverage
  expect_true(all(out[id == "A", dec] == "D"))               # hospitalization declines
  expect_true(all(out[id == "B", dec] == "S"))               # benign history accepts
})
