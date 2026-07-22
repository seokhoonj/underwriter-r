# underwriter

<!-- badges: start -->
[![R-CMD-check](https://github.com/seokhoonj/underwriter-r/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/seokhoonj/underwriter-r/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Automated underwriting simulation from insurance claim history. A data.table
pipeline that cleanses claim data, maps diagnosis codes to representative
diseases, and decides each insured -- for standard products by matching a rule set
of per-disease bands, and for simplified-issue products by evaluating the three
application-form questions. The front end is shared; the two paths fork at matching.

The engine is data-agnostic: the rule set, decision codes, period bands, and
simplified-issue workbook are supplied at run time, so it is not tied to any one
insurer.

## Install

```r
# install.packages("remotes")
remotes::install_github("seokhoonj/underwriter-r")
```

## Standard pipeline

```r
library(underwriter)

cleaned    <- filter_latest_inquiry(clean_icis(raw))  # cleanse, keep latest inquiry
melted     <- melt_kcd(cleaned)                       # one row per diagnosis code
mapped     <- map_disease(melted, disease)            # code -> representative disease
aggregated <- aggregate_disease(mapped)               # aggregate per-insured inputs
matched    <- match_rule(aggregated, ruleset)         # band-match the rule set
applied    <- matched$applied
combined   <- combine_decision(applied, rulebook)     # rulebook = decision/exclusion/reduction/loading
```

`combined` is one row per insured, one column per coverage, holding the final
decision code; the config tables ride along as attributes. Every insured in the
claim feed gets a row: one with nothing to underwrite carries a reserved sentinel
code -- `VACANT` (no diagnosis), `IRREGULAR` (unreadable), `UNMAPPED` (not in the
mapping), or `EXPIRED` (every diagnosis aged out of its window) -- and the rule set
decides them like any other, so no insured is lost.

## Simplified-issue (간편)

Simplified-issue products underwrite from three application-form questions rather
than a per-disease rule set, so the fork happens at matching. Everything is driven
by one workbook.

```r
rulebook <- load_si_rulebook("rulebook_si.xlsx")           # seven sheets, validated
product  <- si_product("325", rulebook)                    # one product's windows + coverages
combined <- underwrite_si(raw, disease, rulebook, product) # raw claims -> decision
```

Each insured x coverage is decided by the worst of the three questions
(decline > underwriter > standard). `underwrite_si` runs the whole chain;
`match_si_rule` and `combine_si_decision` are the pieces. Every accept carries its
reason -- a carve-out, aged-out history (`EXPIRED`), no diagnosis (`VACANT`), or no
applicable condition. Summaries mirror the standard path: `tabulate_si_decision` /
`auto_rate`, `list_si_rule_impact`, `list_si_decline_disease`,
`diagnose_si_ruleset`, and `trace_si_decision`.

## Summaries

```r
tab <- tabulate_decision(combined)    # decision distribution per coverage
plot(tab)                             # stacked bar of the composition
diagnose_icis(raw)                    # data-quality report
diagnose_icis(mapped)                 # ... plus which diagnoses the lookback windows admit
trace_decision(applied, combined, id) # audit one insured's decision
```

## Relaxation analysis

Which rules to relax to lift the automation rate (share of auto-decided cells,
i.e. those not referred to the underwriter) -- all per coverage:

```r
list_rule_impact(applied, combined)                         # each rule's marginal impact
relax_rule(applied, combined, "M543")                       # one rule, per-coverage before/after
decompose_rule_impact(applied, combined, c("M543", "M542")) # marginal / joint / synergy

plot(list_rule_impact(applied, combined), coverage = "adb") # ranking bar for a coverage
plot(relax_rule(applied, combined, "M543"))                 # before/after dumbbell
```

Pass `coverage = "adb"` (or a vector) to restrict any of these to specific
coverages; the default is every coverage.
