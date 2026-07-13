# Search strategy and reproducibility notes

## Data source

ClinicalTrials.gov records were retrieved through the public API v2. No private account, password, or institutional database credential is required. The analytic snapshot was extracted on May 11, 2026.

## Date range

Records first submitted from January 1, 2015, through May 11, 2026, were eligible. The year 2026 was incomplete at extraction.

## Queries

Sixteen prespecified free-text searches combined AI terminology with cancer and imaging concepts. The exact strings are in `data/clinicaltrials_search_queries.csv`.

## Deduplication

Results from all searches were combined and deduplicated by NCT identifier before screening. The locked screening file contains 846 candidate records.

## Eligibility

Core eligibility required an oncologic context, radiological imaging or radiology-derived data, and AI imaging as a principal study objective, intervention, comparator, or primary evaluation. Pathology-only, endoscopy-only, EHR/genomics-only, breath- or biomarker-only studies, ordinary imaging-device studies without AI, and therapeutic studies in which imaging served only as an outcome assessment were excluded. Exploratory or correlative AI imaging within a therapeutic or interventional study was classified as broad eligibility rather than core eligibility.

## Re-running the search

`scripts/01_retrieve_candidate_ids.R` repeats the searches against the live ClinicalTrials.gov API and preserves each returned JSON page. Registry records are updated over time, so a live rerun may not reproduce the historical candidate count. The locked candidate and analysis datasets are the source of truth for manuscript reproduction.
