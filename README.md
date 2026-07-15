# AI-specific evaluation of registered oncologic imaging studies

This repository accompanies the manuscript **"Registry labels obscure prospective and randomized evaluation of oncologic imaging artificial intelligence."** It contains locked ClinicalTrials.gov-derived datasets, the prespecified codebook and search implementation, publication-linkage status, analysis code, source tables, and publication-quality figure files.

## Version

Version 0.9.0 reproduces the reported analyses from the locked May 11, 2026 registry snapshot. A versioned archival DOI will be added when the public release is deposited.

## Main findings

- 846 candidate ClinicalTrials.gov records were screened; 263 met strict core eligibility criteria.
- The registration rate per complete calendar year was 4.41-fold higher in 2021-2025 than in 2015-2020.
- Study-level fields classified 170 studies as prospective; AI-specific review confirmed prospective use of a fixed system in 103, leaving 67 of 170 (39.4%) unconfirmed.
- Study-level fields classified 32 studies as randomized; 28 clearly changed AI exposure between allocated groups.
- 14 of 263 (5.3%) designated decision, patient, or economic consequences as the primary AI evaluation level; only 3 of those studies were completed.
- Among 60 studies completed at least 12 months before extraction, 14 (23.3%) had registry-posted results or an empirical AI-results PubMed article linked by exact NCT identifier.

## Repository structure

```text
data/                    Locked candidate, primary, sensitivity, search, codebook, and linkage files
docs/                    Search, adjudication, and reproducibility documentation
scripts/                 Live candidate retrieval and locked-data analysis scripts
outputs/figures/         Main figure exports
outputs/tables/          Source and supplementary analysis tables
outputs/supplementary/   Submission-ready detailed audit appendix and record-level workbook
```

## Reproduce the analysis

Install R 4.5.1 or later and the packages listed in `requirements.txt`, then run from the repository root:

```bash
Rscript scripts/02_reproduce_analysis.R .
```

This command rebuilds the main table, figure source tables, supplementary analysis tables, and Figure 1 from the locked primary dataset.

To regenerate the SHA-256 release manifest after reproducing the outputs:

```bash
Rscript scripts/03_create_manifest.R .
```

To verify every file against the checked-in manifest:

```bash
Rscript scripts/04_verify_manifest.R .
```

To repeat the search against the current ClinicalTrials.gov API:

```bash
Rscript scripts/01_retrieve_candidate_ids.R .
```

Live registry results can differ from the locked May 11, 2026 snapshot because records are added and updated. The locked files are the source of truth for the manuscript.

## Cohort definition

The primary cohort required `eligibility == include_core` and `ai_as_primary_focus == yes`. Core records explicitly evaluated AI analysis of radiological oncologic imaging as a primary or major objective. Pathology-only, endoscopy-only, genomics/EHR-only, ordinary imaging studies without AI, and therapeutic studies with only ancillary AI imaging were not included in the primary analysis.

## Important definitions

`prospective_ai_validation` refers to prospective application of a fixed AI tool or workflow, not merely prospective participant enrollment. `randomized_ai_workflow_comparison` requires allocation that changes AI exposure, not treatment randomization alone. `true_patient_outcome` evaluates a patient health or patient-reported consequence of AI use rather than prediction of that outcome.

## Data provenance and privacy

All source information is public study-level ClinicalTrials.gov metadata. The repository contains no participant-level data, protected health information, credentials, or medical images. No AACT account is required.

## Publication-linkage limitation

PubMed linkage used exact NCT identifiers and is therefore specific but incomplete: it can miss publications that do not report the registration number. The combined public-results estimate should be interpreted as identifiable dissemination, not a definitive publication rate.

## Citation

Please use the metadata in `CITATION.cff`. A DOI and final journal citation will be added after author verification, archiving, and publication.
