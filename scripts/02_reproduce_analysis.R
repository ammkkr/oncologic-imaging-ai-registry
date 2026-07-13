#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(jsonlite)
  library(lubridate)
  library(patchwork)
  library(purrr)
  library(readr)
  library(scales)
  library(stringr)
  library(tidyr)
})

options(stringsAsFactors = FALSE, scipen = 999)

args <- commandArgs(trailingOnly = TRUE)
repo_root <- if (length(args) >= 1) args[[1]] else "."
repo_root <- normalizePath(repo_root, winslash = "/", mustWork = TRUE)
extraction_date <- as.Date("2026-05-11")

input_file <- file.path(
  repo_root,
  "data",
  "AI_oncologic_imaging_registered_studies_primary_locked.csv"
)
table_dir <- file.path(repo_root, "outputs", "tables")
figure_dir <- file.path(repo_root, "outputs", "figures")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

obsolete_table_files <- c(
  "Figure_1b_period_validation.csv",
  "Table_S2_result_reporting_denominators.csv",
  "Table_S9_radiomics_only_sensitivity.csv"
)
obsolete_paths <- file.path(table_dir, obsolete_table_files)
if (any(file.exists(obsolete_paths))) file.remove(obsolete_paths[file.exists(obsolete_paths)])

if (!file.exists(input_file)) stop("Locked primary dataset not found: ", input_file)

clean_chr <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  str_squish(x)
}

as_yes <- function(x) str_to_lower(clean_chr(x)) == "yes"

as_flag <- function(x) str_to_lower(clean_chr(x)) %in% c("true", "t", "yes", "y", "1")

parse_partial_date_end <- function(x) {
  x <- clean_chr(x)
  out <- rep(as.Date(NA), length(x))
  full <- str_detect(x, "^\\d{4}-\\d{2}-\\d{2}$")
  month <- str_detect(x, "^\\d{4}-\\d{2}$")
  year <- str_detect(x, "^\\d{4}$")
  out[full] <- suppressWarnings(as.Date(x[full]))
  if (any(month)) {
    first_day <- suppressWarnings(as.Date(paste0(x[month], "-01")))
    out[month] <- ceiling_date(first_day, unit = "month") - days(1)
  }
  if (any(year)) out[year] <- suppressWarnings(as.Date(paste0(x[year], "-12-31")))
  out
}

wilson_interval <- function(k, n, conf = 0.95) {
  if (n == 0) return(c(NA_real_, NA_real_))
  z <- qnorm(1 - (1 - conf) / 2)
  p <- k / n
  center <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
  half <- z * sqrt((p * (1 - p) + z^2 / (4 * n)) / n) / (1 + z^2 / n)
  c(max(0, center - half), min(1, center + half))
}

fmt_n_pct <- function(k, n) sprintf("%d (%.1f)", k, 100 * k / n)

fmt_p <- function(p) ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))

has_tag <- function(x, tags) {
  pattern <- paste(tags, collapse = "|")
  str_detect(str_to_lower(clean_chr(x)), paste0("(^|;\\s*)(", pattern, ")(\\s*;|$)"))
}

split_multilabel <- function(data, field, new_name) {
  data %>%
    select(nct_id, all_of(field)) %>%
    mutate(value = clean_chr(.data[[field]])) %>%
    filter(value != "") %>%
    separate_rows(value, sep = ";") %>%
    mutate(value = str_to_lower(str_squish(value))) %>%
    filter(value != "") %>%
    distinct(nct_id, value) %>%
    rename(!!new_name := value)
}

primary <- read_csv(input_file, show_col_types = FALSE, progress = FALSE) %>%
  mutate(
    submitted_year = as.integer(submitted_year),
    registration_period = factor(registration_period, levels = c("2015-2020", "2021-2026")),
    study_level_prospective_confirmed = as_yes(study_level_prospective_design),
    prospective_confirmed = as_yes(prospective_ai_validation),
    study_level_randomized_confirmed = as_yes(study_level_randomized_design),
    randomized_confirmed = as_yes(randomized_ai_workflow_comparison),
    downstream_impact_confirmed = as_yes(downstream_clinical_impact_endpoint),
    true_outcome_confirmed = as_yes(true_patient_outcome),
    registry_results_posted = as_flag(registry_results_posted),
    empirical_results_article_linked_by_exact_nct = as_flag(empirical_results_article_linked_by_exact_nct),
    completed = str_to_upper(clean_chr(overall_status)) == "COMPLETED",
    completion_date_parsed = parse_partial_date_end(completion_date),
    completed_at_least_12_months = completed & !is.na(completion_date_parsed) &
      completion_date_parsed <= extraction_date - days(365)
  ) %>%
  arrange(submitted_year, nct_id)

if (nrow(primary) != 263) warning("Expected 263 primary-cohort records; found ", nrow(primary))
if (anyDuplicated(primary$nct_id)) stop("Duplicate NCT identifiers in locked primary dataset")

prepare_sensitivity_cohort <- function(path) {
  read_csv(path, show_col_types = FALSE, progress = FALSE) %>%
    mutate(
      prospective_confirmed = as_yes(prospective_ai_validation),
      randomized_confirmed = as_yes(randomized_ai_workflow_comparison),
      downstream_impact_confirmed = as_yes(downstream_clinical_impact_endpoint),
      true_outcome_confirmed = as_yes(true_patient_outcome),
      registry_results_posted = as_flag(registry_results_posted)
    )
}

core_all <- prepare_sensitivity_cohort(file.path(
  repo_root, "data", "AI_oncologic_imaging_registered_studies_core_sensitivity_locked.csv"
))
broad_sensitivity <- prepare_sensitivity_cohort(file.path(
  repo_root, "data", "AI_oncologic_imaging_registered_studies_broad_sensitivity_locked.csv"
))
candidate_records <- read_csv(
  file.path(repo_root, "data", "AI_oncologic_imaging_candidate_records_adjudicated.csv"),
  show_col_types = FALSE,
  progress = FALSE
)

outcomes <- tribble(
  ~outcome_key, ~outcome_label,
  "prospective", "AI-specific prospective design",
  "randomized", "AI-workflow randomization",
  "downstream_impact", "Decision, patient, or economic primary evaluation",
  "true_outcome", "True patient outcome endpoint",
  "posted", "Registry results posted"
)

outcome_vector <- function(data, key) {
  switch(
    key,
    prospective = data$prospective_confirmed,
    randomized = data$randomized_confirmed,
    downstream_impact = data$downstream_impact_confirmed,
    true_outcome = data$true_outcome_confirmed,
    posted = data$registry_results_posted,
    stop("Unknown outcome: ", key)
  )
}

period_validation <- bind_rows(lapply(seq_len(nrow(outcomes)), function(i) {
  key <- outcomes$outcome_key[[i]]
  bind_rows(lapply(levels(primary$registration_period), function(period) {
    data <- primary %>% filter(registration_period == period)
    y <- outcome_vector(data, key)
    k <- sum(y)
    ci <- wilson_interval(k, length(y))
    tibble(
      outcome_key = key,
      outcome_label = outcomes$outcome_label[[i]],
      period = period,
      numerator = k,
      denominator = length(y),
      proportion = k / length(y),
      ci_low = ci[[1]],
      ci_high = ci[[2]]
    )
  }))
}))

trend_results <- bind_rows(lapply(c("prospective", "randomized", "downstream_impact"), function(key) {
  model_data <- tibble(
    y = as.integer(outcome_vector(primary, key)),
    year = primary$submitted_year,
    period = if_else(primary$submitted_year <= 2020, "2015-2020", "2021-2025")
  ) %>% filter(!is.na(year), year <= 2025, !is.na(y), !is.na(period))
  fit <- glm(y ~ year, family = binomial(), data = model_data)
  coefficient <- summary(fit)$coefficients["year", ]
  estimate <- unname(coefficient[["Estimate"]])
  se <- unname(coefficient[["Std. Error"]])
  early <- model_data$y[model_data$period == "2015-2020"]
  late <- model_data$y[model_data$period == "2021-2025"]
  contingency <- matrix(
    c(sum(late == 1), sum(late == 0), sum(early == 1), sum(early == 0)),
    nrow = 2,
    byrow = TRUE
  )
  fisher <- fisher.test(contingency)
  tibble(
    outcome_key = key,
    outcome_label = outcomes$outcome_label[outcomes$outcome_key == key],
    annual_log_odds_coefficient = estimate,
    annual_standard_error = se,
    annual_odds_ratio = exp(estimate),
    annual_or_ci_low = exp(estimate - 1.96 * se),
    annual_or_ci_high = exp(estimate + 1.96 * se),
    annual_z = unname(coefficient[["z value"]]),
    annual_p_value = unname(coefficient[["Pr(>|z|)"]]),
    late_vs_early_fisher_odds_ratio = unname(fisher$estimate),
    late_vs_early_fisher_ci_low = fisher$conf.int[[1]],
    late_vs_early_fisher_ci_high = fisher$conf.int[[2]],
    late_vs_early_fisher_p_value = fisher$p.value
  )
}))

annual_counts <- primary %>%
  mutate(
    prospective_status = if_else(
      prospective_confirmed,
      "AI-specific prospective design",
      "No or unclear"
    )
  ) %>%
  count(submitted_year, prospective_status, name = "n") %>%
  complete(
    submitted_year = 2015:2026,
    prospective_status = c("No or unclear", "AI-specific prospective design"),
    fill = list(n = 0)
  )

annual_totals <- annual_counts %>% group_by(submitted_year) %>% summarise(n = sum(n), .groups = "drop")
early_count <- sum(annual_totals$n[annual_totals$submitted_year %in% 2015:2020])
late_complete_count <- sum(annual_totals$n[annual_totals$submitted_year %in% 2021:2025])
registration_rate <- poisson.test(c(late_complete_count, early_count), T = c(5, 6))

cancer_long <- split_multilabel(primary, "cancer_types", "cancer_type")
cancer_counts <- cancer_long %>% count(cancer_type, sort = TRUE, name = "n")
top_cancers <- cancer_counts %>% slice_head(n = 6) %>% pull(cancer_type)

collapse_evaluation <- function(x) {
  case_when(
    x == "diagnostic_accuracy" ~ "Diagnostic accuracy",
    x %in% c("prognostic_prediction", "treatment_response_prediction") ~ "Prognostic/response prediction",
    x %in% c("reader_performance", "workflow_efficiency") ~ "Reader/workflow",
    x %in% c("clinical_decision_impact", "true_patient_outcome", "health_economic") ~ "Clinical decision/patient outcome",
    TRUE ~ "Model performance/other"
  )
}

evaluation_order <- c(
  "Diagnostic accuracy",
  "Prognostic/response prediction",
  "Reader/workflow",
  "Clinical decision/patient outcome",
  "Model performance/other"
)

cancer_labels <- c(
  lung = "Lung", breast = "Breast", liver = "Liver", prostate = "Prostate",
  brain_cns = "Brain/CNS", colorectal = "Colorectal"
)

cancer_evaluation <- cancer_long %>%
  filter(cancer_type %in% top_cancers) %>%
  left_join(
    primary %>% transmute(nct_id, evaluation_group = collapse_evaluation(primary_ai_evaluation_level)),
    by = "nct_id"
  ) %>%
  count(cancer_type, evaluation_group, name = "n") %>%
  complete(cancer_type = top_cancers, evaluation_group = evaluation_order, fill = list(n = 0)) %>%
  mutate(
    cancer_label = recode(cancer_type, !!!cancer_labels),
    cancer_label = factor(cancer_label, levels = rev(recode(top_cancers, !!!cancer_labels))),
    evaluation_group = factor(evaluation_group, levels = evaluation_order)
  )

evaluation_counts <- primary %>%
  transmute(ai_evaluation_level_locked = primary_ai_evaluation_level) %>%
  count(ai_evaluation_level_locked, sort = TRUE, name = "n") %>%
  mutate(percent = 100 * n / nrow(primary))

modality_long <- split_multilabel(primary, "imaging_modalities", "imaging_modality")
method_long <- split_multilabel(primary, "ai_method_categories", "ai_method")
modality_counts <- modality_long %>% count(imaging_modality, sort = TRUE, name = "n")
method_counts <- method_long %>% count(ai_method, sort = TRUE, name = "n")
modality_method <- modality_long %>%
  inner_join(method_long, by = "nct_id", relationship = "many-to-many") %>%
  count(imaging_modality, ai_method, name = "n") %>%
  arrange(desc(n), imaging_modality, ai_method)

completed_n <- sum(primary$completed)
completed_12_months_n <- sum(primary$completed_at_least_12_months)
posted_completed_n <- sum(primary$completed & primary$registry_results_posted)
published_completed_n <- sum(primary$completed & primary$empirical_results_article_linked_by_exact_nct)
any_results_completed_n <- sum(
  primary$completed &
    (primary$registry_results_posted | primary$empirical_results_article_linked_by_exact_nct)
)
posted_completed_12_months_n <- sum(primary$completed_at_least_12_months & primary$registry_results_posted)
published_completed_12_months_n <- sum(
  primary$completed_at_least_12_months & primary$empirical_results_article_linked_by_exact_nct
)
any_results_completed_12_months_n <- sum(
  primary$completed_at_least_12_months &
    (primary$registry_results_posted | primary$empirical_results_article_linked_by_exact_nct)
)

reporting_summary <- tibble(
  denominator_group = c(
    "Registry results posted: all primary-cohort studies",
    "Registry results posted: completed studies",
    "Registry results posted: completed at least 12 months before extraction",
    "PubMed-linked empirical AI results article by extraction: completed studies",
    "PubMed-linked empirical AI results article by extraction: completed at least 12 months before extraction",
    "Any registry-posted or PubMed-linked results by extraction: completed studies",
    "Any registry-posted or PubMed-linked results by extraction: completed at least 12 months before extraction"
  ),
  numerator = c(
    sum(primary$registry_results_posted),
    posted_completed_n,
    posted_completed_12_months_n,
    published_completed_n,
    published_completed_12_months_n,
    any_results_completed_n,
    any_results_completed_12_months_n
  ),
  denominator = c(
    nrow(primary),
    completed_n,
    completed_12_months_n,
    completed_n,
    completed_12_months_n,
    completed_n,
    completed_12_months_n
  )
) %>%
  rowwise() %>%
  mutate(
    proportion = numerator / denominator,
    ci_low = wilson_interval(numerator, denominator)[[1]],
    ci_high = wilson_interval(numerator, denominator)[[2]]
  ) %>%
  ungroup()

table_value <- function(data, key) {
  n <- nrow(data)
  enrollment <- suppressWarnings(as.numeric(data$enrollment))
  enrollment <- enrollment[!is.na(enrollment)]
  switch(
    key,
    enrollment = {
      q <- quantile(enrollment, c(0.25, 0.75), names = FALSE)
      sprintf("%.0f (%.0f-%.0f)", median(enrollment), q[[1]], q[[2]])
    },
    observational = fmt_n_pct(sum(str_to_upper(clean_chr(data$study_type)) == "OBSERVATIONAL"), n),
    interventional = fmt_n_pct(sum(str_to_upper(clean_chr(data$study_type)) == "INTERVENTIONAL"), n),
    recruiting = fmt_n_pct(sum(str_to_upper(clean_chr(data$overall_status)) == "RECRUITING"), n),
    completed = fmt_n_pct(sum(str_to_upper(clean_chr(data$overall_status)) == "COMPLETED"), n),
    unknown = fmt_n_pct(sum(str_to_upper(clean_chr(data$overall_status)) == "UNKNOWN"), n),
    not_yet = fmt_n_pct(sum(str_to_upper(clean_chr(data$overall_status)) == "NOT_YET_RECRUITING"), n),
    active = fmt_n_pct(sum(str_to_upper(clean_chr(data$overall_status)) == "ACTIVE_NOT_RECRUITING"), n),
    other_status = fmt_n_pct(sum(!str_to_upper(clean_chr(data$overall_status)) %in% c("RECRUITING", "COMPLETED", "UNKNOWN", "NOT_YET_RECRUITING", "ACTIVE_NOT_RECRUITING")), n),
    single = fmt_n_pct(sum(str_to_lower(clean_chr(data$center_category)) == "single_center"), n),
    multi = fmt_n_pct(sum(str_to_lower(clean_chr(data$center_category)) == "multicenter"), n),
    center_missing = fmt_n_pct(sum(str_to_lower(clean_chr(data$center_category)) == "not_reported"), n),
    study_level_prospective = fmt_n_pct(sum(data$study_level_prospective_confirmed), n),
    prospective = fmt_n_pct(sum(data$prospective_confirmed), n),
    study_level_randomized = fmt_n_pct(sum(data$study_level_randomized_confirmed), n),
    randomized = fmt_n_pct(sum(data$randomized_confirmed), n),
    diagnostic = fmt_n_pct(sum(data$primary_ai_evaluation_level == "diagnostic_accuracy"), n),
    downstream = fmt_n_pct(sum(data$downstream_impact_confirmed), n),
    clinical_decision = fmt_n_pct(sum(data$primary_ai_evaluation_level == "clinical_decision_impact"), n),
    true_outcome = fmt_n_pct(sum(data$true_outcome_confirmed), n),
    posted = fmt_n_pct(sum(data$registry_results_posted), n),
    posted_completed = fmt_n_pct(sum(data$registry_results_posted & data$completed), sum(data$completed)),
    linked_completed = fmt_n_pct(sum(data$empirical_results_article_linked_by_exact_nct & data$completed), sum(data$completed)),
    any_completed = fmt_n_pct(sum(data$completed & (data$registry_results_posted | data$empirical_results_article_linked_by_exact_nct)), sum(data$completed)),
    lbp = fmt_n_pct(sum(has_tag(data$cancer_types, c("lung", "breast", "prostate"))), n)
  )
}

table_spec <- tribble(
  ~Characteristic, ~key,
  "Enrollment, median (IQR)", "enrollment",
  "Study type", "header",
  "  Observational", "observational",
  "  Interventional", "interventional",
  "Recruitment status", "header",
  "  Recruiting", "recruiting",
  "  Completed", "completed",
  "  Unknown status", "unknown",
  "  Not yet recruiting", "not_yet",
  "  Active, not recruiting", "active",
  "  Other status", "other_status",
  "Center category", "header",
  "  Single-center", "single",
  "  Multicenter", "multi",
  "  Not reported", "center_missing",
  "Study-level prospective design", "study_level_prospective",
  "AI-specific prospective design", "prospective",
  "Study-level randomized allocation", "study_level_randomized",
  "AI-workflow randomization", "randomized",
  "Diagnostic accuracy as primary evaluation", "diagnostic",
  "Decision, patient, or economic primary evaluation", "downstream",
  "Clinical decision-impact primary evaluation", "clinical_decision",
  "True patient outcome endpoint", "true_outcome",
  "Registry results posted", "posted",
  "Registry results posted among completed studies", "posted_completed",
  "PubMed-linked empirical results among completed studies", "linked_completed",
  "Any registry-posted or PubMed-linked results among completed studies", "any_completed",
  "Lung, breast, or prostate cancer", "lbp"
)

period_early <- primary %>% filter(registration_period == "2015-2020")
period_late <- primary %>% filter(registration_period == "2021-2026")

table_1 <- table_spec %>%
  rowwise() %>%
  mutate(
    `Overall (N=263)` = if (key == "header") "" else table_value(primary, key),
    `2015-2020 (N=52)` = if (key == "header") "" else table_value(period_early, key),
    `2021-2026 (N=211)` = if (key == "header") "" else table_value(period_late, key)
  ) %>%
  ungroup() %>%
  select(-key)

design_reclassification <- bind_rows(
  tibble(
    design_domain = "Prospective design",
    study_level_positive = primary$study_level_prospective_confirmed,
    ai_specific_classification = primary$prospective_ai_validation
  ),
  tibble(
    design_domain = "Randomized design",
    study_level_positive = primary$study_level_randomized_confirmed,
    ai_specific_classification = primary$randomized_ai_workflow_comparison
  )
) %>%
  filter(study_level_positive) %>%
  group_by(design_domain) %>%
  summarise(
    study_level_positive_n = n(),
    ai_specific_yes_n = sum(ai_specific_classification == "yes"),
    ai_specific_no_n = sum(ai_specific_classification == "no"),
    ai_specific_unclear_n = sum(ai_specific_classification == "unclear"),
    ai_specific_confirmed_percent = 100 * ai_specific_yes_n / study_level_positive_n,
    not_confirmed_percent = 100 * (ai_specific_no_n + ai_specific_unclear_n) / study_level_positive_n,
    .groups = "drop"
  )

design_reclassification_plot <- tribble(
  ~design_measure, ~classification_level, ~numerator,
  "Prospective design", "Study-level registry design", sum(primary$study_level_prospective_confirmed),
  "Prospective design", "AI-specific confirmation", sum(primary$prospective_confirmed),
  "Randomized design", "Study-level registry design", sum(primary$study_level_randomized_confirmed),
  "Randomized design", "AI-specific confirmation", sum(primary$randomized_confirmed)
) %>%
  mutate(
    denominator = nrow(primary),
    proportion = numerator / denominator,
    percent = 100 * proportion,
    direct_label = sprintf("%d/%d (%.1f%%)", numerator, denominator, percent)
  )

stage_appropriate_summary <- bind_rows(
  tibble(
    evidence_stage = "Human-interaction or downstream-effect evaluation",
    data = list(primary %>% filter(as_yes(human_or_downstream_evaluation)))
  ),
  tibble(
    evidence_stage = "Decision, patient, or economic primary evaluation",
    data = list(primary %>% filter(downstream_impact_confirmed))
  )
) %>%
  rowwise() %>%
  mutate(
    n = nrow(data),
    prospective_ai_validation_n = sum(data$prospective_confirmed),
    randomized_ai_workflow_n = sum(data$randomized_confirmed),
    completed_n = sum(data$completed),
    unknown_status_n = sum(str_to_upper(clean_chr(data$overall_status)) == "UNKNOWN")
  ) %>%
  ungroup() %>%
  select(-data)

radiomics_sensitivity <- bind_rows(
  tibble(cohort = "Full primary cohort", data = list(primary)),
  tibble(cohort = "Excluding radiomics-only studies", data = list(primary %>% filter(!as_yes(radiomics_only)))),
  tibble(cohort = "Excluding radiology-report-only studies", data = list(primary %>% filter(!as_yes(radiology_report_only)))),
  tibble(
    cohort = "Excluding radiomics-only and radiology-report-only studies",
    data = list(primary %>% filter(!as_yes(radiomics_only), !as_yes(radiology_report_only)))
  )
) %>%
  rowwise() %>%
  mutate(
    n = nrow(data),
    prospective_ai_validation_n = sum(data$prospective_confirmed),
    prospective_ai_validation_percent = 100 * prospective_ai_validation_n / n,
    randomized_ai_workflow_n = sum(data$randomized_confirmed),
    randomized_ai_workflow_percent = 100 * randomized_ai_workflow_n / n,
    downstream_clinical_impact_n = sum(data$downstream_impact_confirmed),
    downstream_clinical_impact_percent = 100 * downstream_clinical_impact_n / n,
    true_patient_outcome_n = sum(data$true_outcome_confirmed),
    true_patient_outcome_percent = 100 * true_patient_outcome_n / n
  ) %>%
  ungroup() %>%
  select(-data)

status_stratified_markers <- primary %>%
  mutate(
    status_group = case_when(
      completed ~ "Completed",
      str_to_upper(clean_chr(overall_status)) == "UNKNOWN" ~ "Unknown status",
      TRUE ~ "Ongoing or other"
    )
  ) %>%
  group_by(status_group) %>%
  summarise(
    n = n(),
    prospective_ai_validation_n = sum(prospective_confirmed),
    randomized_ai_workflow_n = sum(randomized_confirmed),
    downstream_clinical_impact_n = sum(downstream_impact_confirmed),
    true_patient_outcome_n = sum(true_outcome_confirmed),
    .groups = "drop"
  )

sensitivity_summary <- bind_rows(
  tibble(cohort = "Primary cohort", data = list(primary)),
  tibble(cohort = "Final include-core cohort regardless of AI-primary flag", data = list(core_all)),
  tibble(cohort = "Final core-plus-broad cohort with AI-primary focus", data = list(broad_sensitivity))
) %>%
  rowwise() %>%
  mutate(
    n = nrow(data),
    prospective_n = sum(data$prospective_confirmed),
    prospective_percent = 100 * prospective_n / n,
    randomized_n = sum(data$randomized_confirmed),
    randomized_percent = 100 * randomized_n / n,
    downstream_impact_n = sum(data$downstream_impact_confirmed),
    downstream_impact_percent = 100 * downstream_impact_n / n,
    true_outcome_n = sum(data$true_outcome_confirmed),
    true_outcome_percent = 100 * true_outcome_n / n,
    posted_n = sum(data$registry_results_posted),
    posted_percent = 100 * posted_n / n
  ) %>%
  ungroup() %>%
  select(-data)

screening_flow <- bind_rows(
  tibble(stage = 1L, screening_stage = "Candidate records identified and deduplicated", n = nrow(candidate_records)),
  tibble(stage = 2L, screening_stage = "Excluded after registry-record screening", n = sum(candidate_records$eligibility == "exclude")),
  tibble(stage = 3L, screening_stage = "Broad or ancillary AI-imaging records retained for sensitivity analyses", n = sum(candidate_records$eligibility == "include_broad")),
  tibble(stage = 4L, screening_stage = "Core eligible records", n = sum(candidate_records$eligibility == "include_core")),
  tibble(
    stage = 5L,
    screening_stage = "Primary cohort: core eligible and AI as primary focus",
    n = sum(candidate_records$eligibility == "include_core" & candidate_records$ai_as_primary_focus == "yes")
  )
)

overall_markers <- tribble(
  ~marker_key, ~marker_label, ~numerator, ~denominator,
  "prospective", "AI-specific prospective design", sum(primary$prospective_confirmed), nrow(primary),
  "randomized", "AI-workflow randomization", sum(primary$randomized_confirmed), nrow(primary),
  "downstream_impact", "Decision, patient, or economic evaluation", sum(primary$downstream_impact_confirmed), nrow(primary),
  "clinical_decision", "Clinical decision impact", sum(primary$primary_ai_evaluation_level == "clinical_decision_impact"), nrow(primary),
  "true_outcome", "True patient outcome", sum(primary$true_outcome_confirmed), nrow(primary),
  "posted", "Registry results posted", sum(primary$registry_results_posted), nrow(primary),
  "any_results", "Any public results among completed", any_results_completed_n, completed_n
) %>%
  mutate(
    proportion = numerator / denominator,
    percent = 100 * proportion,
    ci_low = purrr::map2_dbl(numerator, denominator, ~wilson_interval(.x, .y)[[1]]),
    ci_high = purrr::map2_dbl(numerator, denominator, ~wilson_interval(.x, .y)[[2]]),
    direct_label = sprintf("%d/%d (%.1f%%)", numerator, denominator, percent)
  )

overall_markers$direct_label[overall_markers$marker_key == "posted"] <- sprintf(
  "%d/%d (%.1f%%); completed: %d/%d (%.1f%%)",
  sum(primary$registry_results_posted), nrow(primary), 100 * mean(primary$registry_results_posted),
  posted_completed_n, completed_n, 100 * posted_completed_n / completed_n
)

write_csv(table_1, file.path(table_dir, "Table_1_characteristics_by_registration_period.csv"), na = "")
write_csv(period_validation, file.path(table_dir, "Table_2_validation_proportions_by_period.csv"), na = "")
write_csv(screening_flow, file.path(table_dir, "Table_S0_screening_flow.csv"), na = "")
write_csv(annual_counts, file.path(table_dir, "Figure_1a_annual_counts.csv"), na = "")
write_csv(design_reclassification_plot, file.path(table_dir, "Figure_1b_design_reclassification.csv"), na = "")
write_csv(cancer_evaluation, file.path(table_dir, "Figure_1c_cancer_evaluation_heatmap.csv"), na = "")
write_csv(overall_markers, file.path(table_dir, "Figure_1d_validation_markers.csv"), na = "")
write_csv(trend_results, file.path(table_dir, "Table_S1_temporal_trend_models.csv"), na = "")
write_csv(reporting_summary, file.path(table_dir, "Table_S2_registry_result_posting_denominators.csv"), na = "")
write_csv(evaluation_counts, file.path(table_dir, "Table_S3_primary_evaluation_levels.csv"), na = "")
write_csv(cancer_counts, file.path(table_dir, "Table_S4_cancer_type_counts.csv"), na = "")
write_csv(modality_counts, file.path(table_dir, "Table_S5a_imaging_modality_counts.csv"), na = "")
write_csv(method_counts, file.path(table_dir, "Table_S5b_ai_method_counts.csv"), na = "")
write_csv(modality_method, file.path(table_dir, "Table_S5c_modality_by_ai_method.csv"), na = "")
write_csv(sensitivity_summary, file.path(table_dir, "Table_S6_cohort_sensitivity.csv"), na = "")
write_csv(design_reclassification, file.path(table_dir, "Table_S7_design_reclassification.csv"), na = "")
write_csv(stage_appropriate_summary, file.path(table_dir, "Table_S8_stage_appropriate_denominators.csv"), na = "")
write_csv(radiomics_sensitivity, file.path(table_dir, "Table_S9_scope_sensitivity.csv"), na = "")
write_csv(status_stratified_markers, file.path(table_dir, "Table_S10_markers_by_registry_status.csv"), na = "")

palette <- c(
  teal = "#007C83",
  orange = "#D55E00",
  gray = "#B8BEC4",
  charcoal = "#27313A",
  pale = "#EEF2F3"
)
base_theme <- theme_minimal(base_size = 9, base_family = "Arial") +
  theme(
    plot.title = element_text(face = "bold", size = 10, color = palette[["charcoal"]], margin = margin(b = 6)),
    axis.title = element_text(size = 9, color = palette[["charcoal"]]),
    axis.text = element_text(size = 8, color = palette[["charcoal"]]),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    plot.margin = margin(8, 10, 8, 8)
  )

annual_counts$prospective_status <- factor(
  annual_counts$prospective_status,
  levels = c("No or unclear", "AI-specific prospective design")
)

p_a <- ggplot(annual_counts, aes(x = submitted_year, y = n, fill = prospective_status)) +
  annotate("rect", xmin = 2025.5, xmax = 2026.5, ymin = -Inf, ymax = Inf, fill = "#F3F3F3") +
  geom_col(width = 0.78, color = "white", linewidth = 0.2) +
  scale_fill_manual(values = c("No or unclear" = palette[["gray"]], "AI-specific prospective design" = palette[["teal"]])) +
  scale_x_continuous(breaks = 2015:2026, labels = function(x) str_sub(as.character(x), 3, 4)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(
    title = "Annual registrations by AI-specific prospective design",
    x = "First-submitted year",
    y = "Registered studies",
    fill = NULL
  ) +
  base_theme + theme(legend.position = "bottom", legend.justification = "left")

panel_b_data <- design_reclassification_plot %>%
  mutate(
    plot_label = case_when(
      design_measure == "Prospective design" & classification_level == "Study-level registry design" ~ "Prospective: study-level",
      design_measure == "Prospective design" ~ "Prospective: AI-specific",
      design_measure == "Randomized design" & classification_level == "Study-level registry design" ~ "Randomized: study-level",
      TRUE ~ "Randomized: AI-specific"
    ),
    plot_label = factor(
      plot_label,
      levels = rev(c(
        "Prospective: study-level",
        "Prospective: AI-specific",
        "Randomized: study-level",
        "Randomized: AI-specific"
      ))
    ),
    classification_level = factor(
      classification_level,
      levels = c("Study-level registry design", "AI-specific confirmation")
    )
  )

p_b <- ggplot(panel_b_data, aes(x = proportion, y = plot_label, color = classification_level)) +
  geom_segment(aes(x = 0, xend = proportion, yend = plot_label), linewidth = 2.5, color = "#D7DEE0", lineend = "round") +
  geom_point(size = 3.1) +
  geom_text(aes(label = direct_label), hjust = 0, nudge_x = 0.018, size = 2.7, color = palette[["charcoal"]]) +
  scale_color_manual(values = c("Study-level registry design" = palette[["orange"]], "AI-specific confirmation" = palette[["teal"]])) +
  scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, 0.78), breaks = seq(0, 0.75, 0.25), expand = expansion(mult = c(0, 0))) +
  labs(
    title = "AI-specific review reclassified registry design labels",
    x = "Proportion of registered studies",
    y = NULL
  ) +
  base_theme +
  theme(legend.position = "bottom", legend.justification = "left")

p_c <- ggplot(cancer_evaluation, aes(x = evaluation_group, y = cancer_label, fill = n)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = if_else(n == 0, "", as.character(n))), size = 2.8, color = palette[["charcoal"]]) +
  scale_fill_gradient(low = "#F5F7F7", high = palette[["teal"]], name = "Studies") +
  scale_x_discrete(labels = c(
    "Diagnostic accuracy" = "Diagnostic\naccuracy",
    "Prognostic/response prediction" = "Prognostic/\nresponse",
    "Reader/workflow" = "Reader/\nworkflow",
    "Clinical decision/patient outcome" = "Decision/\npatient outcome",
    "Model performance/other" = "Model performance/\nother"
  )) +
  labs(title = "Cancer type by primary AI evaluation level", x = NULL, y = NULL) +
  base_theme +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 7.3, lineheight = 0.95),
    legend.position = "bottom",
    legend.key.width = unit(20, "pt")
  )

marker_order <- rev(c(
  "AI-specific prospective design", "AI-workflow randomization",
  "Decision, patient, or economic evaluation", "Clinical decision impact",
  "True patient outcome", "Any public results among completed", "Registry results posted"
))

panel_d <- overall_markers %>% mutate(marker_label = factor(marker_label, levels = marker_order))
p_d <- ggplot(panel_d, aes(x = percent, y = marker_label)) +
  geom_segment(aes(x = 0, xend = percent, yend = marker_label), linewidth = 2.5, color = "#D7DEE0", lineend = "round") +
  geom_point(size = 3.1, color = palette[["teal"]]) +
  geom_text(aes(label = direct_label), hjust = 0, nudge_x = 1.1, size = 2.65, color = palette[["charcoal"]]) +
  scale_x_continuous(limits = c(0, 55), breaks = c(0, 10, 20, 30, 40, 50), labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0))) +
  labs(title = "Registered validation and reporting markers", x = "Percentage (denominator shown)", y = NULL) +
  base_theme

figure_1 <- (p_a | p_b) / (p_c | p_d) +
  plot_layout(heights = c(1, 1.05)) +
  plot_annotation(
    tag_levels = "a",
    theme = theme(
      plot.tag = element_text(face = "bold", size = 12, color = palette[["charcoal"]]),
      plot.tag.position = c(0, 1)
    )
  )

figure_base <- file.path(figure_dir, "Figure_1_growth_and_validation")
ggsave(paste0(figure_base, ".pdf"), figure_1, width = 13, height = 9.5, units = "in", device = cairo_pdf)
ggsave(paste0(figure_base, ".png"), figure_1, width = 13, height = 9.5, units = "in", dpi = 600, bg = "white")
ggsave(paste0(figure_base, ".tif"), figure_1, width = 13, height = 9.5, units = "in", dpi = 600, compression = "lzw", bg = "white")

summary <- list(
  extraction_date = "2026-05-11",
  primary_cohort_n = nrow(primary),
  registration_rate_ratio = unname(registration_rate$estimate),
  registration_rate_ci = unname(registration_rate$conf.int),
  prospective_ai_validation_n = sum(primary$prospective_confirmed),
  randomized_ai_workflow_comparison_n = sum(primary$randomized_confirmed),
  downstream_clinical_impact_n = sum(primary$downstream_impact_confirmed),
  true_patient_outcome_n = sum(primary$true_outcome_confirmed),
  completed_n = completed_n,
  any_public_results_completed_n = any_results_completed_n
)
write_json(summary, file.path(repo_root, "outputs", "analysis_key_results.json"), pretty = TRUE, auto_unbox = TRUE)

cat("Analysis reproduced successfully.\n")
cat("Primary cohort:", nrow(primary), "records\n")
cat("Registration rate ratio:", sprintf("%.2f", registration_rate$estimate), "\n")
cat("Prospective AI validation:", sum(primary$prospective_confirmed), "\n")
cat("Randomized AI-workflow comparison:", sum(primary$randomized_confirmed), "\n")
cat("Decision, patient, or economic primary evaluation:", sum(primary$downstream_impact_confirmed), "\n")
cat("Any public results among completed:", any_results_completed_n, "of", completed_n, "\n")
