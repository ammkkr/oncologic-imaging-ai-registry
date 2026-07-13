# Final analysis codebook

The machine-readable codebook is `data/codebook_final.csv`.

## Eligibility

- `include_core`: oncologic radiological imaging with AI imaging as a primary or major study object.
- `include_broad`: AI imaging is ancillary, exploratory, correlative, or embedded in another intervention.
- `exclude`: nononcologic, nonradiological, pathology/endoscopy/genomics/EHR-only, ordinary imaging without AI, or therapeutic imaging outcome without AI evaluation.

Radiomics-only records require an image-derived predictive or classification model, not descriptive feature extraction alone. Radiology-report-only records require reports to be the imaging-derived input or workflow under evaluation; generic clinical-text NLP is excluded.

## AI focus and locked analysis fields

- `ai_as_primary_focus = yes`: the title, stated objective, intervention, comparator, or prespecified primary evaluation makes AI analysis of radiological imaging an explicit primary or major study focus.
- `ai_as_primary_focus = no`: AI imaging is ancillary, exploratory, correlative, or only one minor component of another therapeutic or procedural study.
- `ai_as_primary_focus = unclear`: the registry text is insufficient to determine whether AI imaging is a major objective.

The primary cohort requires `eligibility = include_core` and `ai_as_primary_focus = yes`. Public fields named `eligibility`, `prospective_ai_validation`, `randomized_ai_workflow_comparison`, and `primary_ai_evaluation_level` are the final locked analysis values after adjudication; the public export does not mix provisional rules with final classifications. `adjudication_note` records the study-specific rationale. An `unclear` value is a final uncertainty classification when the registry text cannot support yes or no.

## Paired design definitions

- `study_level_prospective_design = yes`: an interventional study or an observational study registered with prospective time perspective.
- `prospective_ai_validation = yes`: a fixed AI tool or workflow is applied to newly accrued cases. Prospective collection followed by model development or post hoc analysis is no.
- `study_level_randomized_design = yes`: the registry allocation field indicates randomization.
- `randomized_ai_workflow_comparison = yes`: allocation changes exposure to AI-assisted versus non-AI imaging or workflow.
- `unclear`: registry information is insufficient; this is an uncertainty category, not an unresolved review task.

## Primary evaluation hierarchy

One primary AI evaluation level is assigned from registry-designated primary outcomes and the explicit main AI objective. Secondary endpoints do not promote a record to a more downstream level. If co-primary objectives span levels, the furthest downstream explicit primary level is assigned.

1. `technical_performance`: segmentation, reconstruction, image quality, feasibility, or algorithm execution.
2. `diagnostic_accuracy`: sensitivity, specificity, area under the curve, yield, or classification accuracy.
3. `reader_performance`: reader accuracy with versus without AI.
4. `workflow_efficiency`: time, workload, triage, throughput, or reporting efficiency.
5. `clinical_decision_impact`: recall, biopsy, referral, treatment, or management decisions affected by AI.
6. `prognostic_prediction`: prediction of survival, recurrence, or progression without testing the consequence of AI use.
7. `treatment_response_prediction`: prediction of treatment response without testing the consequence of AI use.
8. `true_patient_outcome`: health, morbidity, mortality, quality of life, harms, or patient-reported outcomes attributable to AI use.
9. `health_economic`: cost, resource use, cost-effectiveness, or budget impact attributable to AI use.
10. `other_unclear`: no other primary AI-specific level can be assigned.

`human_or_downstream_evaluation` includes reader, workflow, decision, patient, and economic levels. `downstream_clinical_impact_endpoint` includes decision, patient, and economic levels. These labels describe endpoint position; they do not assert demonstrated clinical utility.
