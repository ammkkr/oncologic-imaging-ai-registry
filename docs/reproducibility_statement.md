# Reproducibility statement

The repository contains the locked candidate records, final analysis cohort, prespecified codebook, exact search strings, analysis scripts, source tables, and publication-quality figure exports. All source records are public ClinicalTrials.gov data; no patient-level data, protected health information, credentials, or medical images are included.

The locked May 11, 2026 datasets reproduce the manuscript. The retrieval script queries the live registry and is provided to document the search implementation, but live output may change as ClinicalTrials.gov records are added or updated.

The summary values in the manuscript can be checked against `analysis_summary.json`, and the SHA-256 manifest can be verified with `scripts/04_verify_manifest.R`.
