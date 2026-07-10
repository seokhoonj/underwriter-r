# underwriter

<!-- badges: start -->
[![R-CMD-check](https://github.com/seokhoonj/underwriter-r/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/seokhoonj/underwriter-r/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Automated underwriting simulation from insurance claim history. A data.table
pipeline that cleanses claim data, maps diagnosis codes to representative
diseases, aggregates per-insured underwriting inputs, matches them against a rule
set, and combines the per-disease decisions into a final per-insured decision.

The engine is data-agnostic: the rule set, decision-code table, and period bands
are supplied at run time, so it is not tied to any one insurer.

## Install

```r
# install.packages("remotes")
remotes::install_github("seokhoonj/underwriter-r")
```

## Pipeline

```r
library(underwriter)

cleaned    <- filter_latest_inquiry(clean_icis(raw))  # cleanse, keep latest inquiry
melted     <- melt_kcd(cleaned)                       # one row per diagnosis code
mapped     <- map_disease(melted, disease)            # code -> representative disease
aggregated <- aggregate_disease(mapped)               # aggregate per-insured inputs
matched    <- match_rule(aggregated, ruleset)         # band-match the rule set
applied    <- matched$applied
combined   <- combine_decision(applied, decision_table, exclusion_table, reduction_table, loading_table)
```

`combined` is one row per insured, one column per coverage, holding the final
decision code; the config tables ride along as attributes.

## Summaries

```r
tab <- tabulate_decision(combined)    # decision distribution per coverage
plot(tab)                             # stacked bar of the composition
diagnose_icis(raw)                    # data-quality report
trace_decision(applied, combined, id) # audit one insured's decision
```

## Relaxation analysis

Which rules to relax to lift the automation rate (share of auto-decided,
non-manual-review cells) -- all per coverage:

```r
list_rule_impact(applied, combined)                         # each rule's marginal impact
relax_rule(applied, combined, "M543")                       # one rule, per-coverage before/after
decompose_rule_impact(applied, combined, c("M543", "M542")) # marginal / joint / synergy

plot(list_rule_impact(applied, combined), coverage = "adb") # ranking bar for a coverage
plot(relax_rule(applied, combined, "M543"))                 # before/after dumbbell
```

Pass `coverage = "adb"` (or a vector) to restrict any of these to specific
coverages; the default is every coverage.
