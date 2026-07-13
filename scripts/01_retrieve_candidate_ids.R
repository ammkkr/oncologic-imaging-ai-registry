#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(jsonlite)
  library(readr)
  library(stringr)
  library(tidyr)
})

args <- commandArgs(trailingOnly = TRUE)
repo_root <- if (length(args) >= 1) args[[1]] else "."
repo_root <- normalizePath(repo_root, winslash = "/", mustWork = TRUE)
date_start <- as.Date("2015-01-01")
date_end <- as.Date("2026-05-11")

query_file <- file.path(repo_root, "data", "clinicaltrials_search_queries.csv")
output_dir <- file.path(repo_root, "data", "live_retrieval")
raw_dir <- file.path(output_dir, "raw_json")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

queries <- read_csv(query_file, show_col_types = FALSE, progress = FALSE)
api_url <- "https://clinicaltrials.gov/api/v2/studies"

fetch_query <- function(query_id, query_term, page_size = 100L) {
  next_token <- NULL
  page <- 1L
  records <- list()

  repeat {
    params <- c(
      paste0("query.term=", URLencode(query_term, reserved = TRUE)),
      paste0("pageSize=", page_size),
      "countTotal=true",
      "format=json"
    )
    if (!is.null(next_token) && nzchar(next_token)) {
      params <- c(params, paste0("pageToken=", URLencode(next_token, reserved = TRUE)))
    }
    request_url <- paste0(api_url, "?", paste(params, collapse = "&"))
    response <- fromJSON(request_url, simplifyVector = FALSE)

    write_json(
      response,
      file.path(raw_dir, sprintf("%s_page_%03d.json", query_id, page)),
      pretty = FALSE,
      auto_unbox = TRUE,
      null = "null"
    )

    studies <- response$studies %||% list()
    if (length(studies) > 0) {
      records[[page]] <- bind_rows(lapply(studies, function(study) {
        protocol <- study$protocolSection %||% list()
        identification <- protocol$identificationModule %||% list()
        status <- protocol$statusModule %||% list()
        tibble(
          nct_id = identification$nctId %||% NA_character_,
          study_first_submitted_date = status$studyFirstSubmitDate %||% NA_character_,
          matched_query_id = query_id,
          matched_query_term = query_term
        )
      }))
    }

    next_token <- response$nextPageToken %||% NULL
    if (is.null(next_token) || !nzchar(next_token)) break
    page <- page + 1L
    Sys.sleep(0.2)
  }

  bind_rows(records)
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

query_results <- bind_rows(lapply(seq_len(nrow(queries)), function(i) {
  message("Retrieving: ", queries$query_id[[i]])
  fetch_query(queries$query_id[[i]], queries$query_term[[i]])
}))

candidate_ids <- query_results %>%
  mutate(
    submitted_date = suppressWarnings(as.Date(study_first_submitted_date)),
    submitted_year = suppressWarnings(as.integer(str_sub(study_first_submitted_date, 1, 4)))
  ) %>%
  filter(!is.na(submitted_date), submitted_date >= date_start, submitted_date <= date_end) %>%
  group_by(nct_id, study_first_submitted_date) %>%
  summarise(
    matched_query_ids = paste(sort(unique(matched_query_id)), collapse = ";"),
    matched_query_terms = paste(sort(unique(matched_query_term)), collapse = ";"),
    .groups = "drop"
  ) %>%
  arrange(nct_id)

query_log <- query_results %>%
  count(matched_query_id, matched_query_term, name = "records_returned_before_date_filter") %>%
  arrange(matched_query_id)

write_csv(candidate_ids, file.path(output_dir, "candidate_nct_ids_live.csv"), na = "")
write_csv(query_log, file.path(output_dir, "query_log_live.csv"), na = "")

cat("Unique candidate NCT identifiers submitted from", as.character(date_start), "through", as.character(date_end), ":", nrow(candidate_ids), "\n")
cat("This live retrieval may differ from the locked May 11, 2026 snapshot as registry records change.\n")
