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

## Simplified-issue

Simplified-issue products underwrite from three application-form questions rather
than a per-disease rule set. The front end (cleanse / melt / map) is the same; the
fork starts at matching. There is no `aggregate_disease()` step -- each question
windows and counts differently, so there is no shared aggregation.

```r
rulebook <- load_si_rulebook("rulebook_si.xlsx")     # SI workbook, validated
product  <- si_product("325", rulebook)              # one product's windows + coverages

cleaned  <- filter_latest_inquiry(clean_icis(raw))   # front end: shared with standard
mapped   <- map_disease(melt_kcd(cleaned), disease)  #   (disease is the standard sheet)
matched  <- match_si_rule(mapped, rulebook, product) # evaluate the three questions
combined <- combine_si_decision(matched, rulebook, product)  # fold worst per coverage
```

Each insured x coverage is decided by the worst of the three questions
(decline > underwriter > standard). Every accept carries its reason -- a carve-out,
aged-out history (`EXPIRED`), no diagnosis (`VACANT`), or no applicable condition.
`underwrite_si(raw, disease, rulebook, product)` wraps these steps in one call.

## Standard vs simplified-issue

The two paths share the front end and diverge at matching. Function by function:

| Step | Standard | Simplified-issue |
|---|---|---|
| Assemble the rules | read sheets into a list | `load_si_rulebook()` |
| Select a product | -- | `si_product()` |
| Cleanse | `clean_icis()` -> `filter_latest_inquiry()` | *(shared)* |
| Melt + map | `melt_kcd()` -> `map_disease()` | *(shared)* |
| Aggregate inputs | `aggregate_disease()` | -- *(each question windows differently)* |
| Match | `match_rule()` | `match_si_rule()` |
| Combine | `combine_decision()` | `combine_si_decision()` |
| Whole chain | *(run the steps)* | `underwrite_si()` |
| Tabulate | `tabulate_decision()` | `tabulate_si_decision()` |
| Automation rate | *(inline)* | `auto_rate()` |
| Rule impact | `list_rule_impact()` | `list_si_rule_impact()` |
| Decline drivers | `list_decline_disease()` | `list_si_decline_disease()` |
| Diagnose the rules | `diagnose_ruleset()` | `diagnose_si_ruleset()` |
| Trace one insured | `trace_decision()` | `trace_si_decision()` |
| Relaxation study | `relax_rule()`, `decompose_rule_impact()` | -- *(declines carry their own reason)* |

Names match, with `si` added only where a standard counterpart already owns the
name; `si_product` and `auto_rate` have none, and `load_si_rulebook` /
`underwrite_si` mark operations the standard path does not have as functions.

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
