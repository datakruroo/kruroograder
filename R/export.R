#' Export Grading Results
#'
#' Export grading results to various formats
#'
#' @param results Grading results from grade_responses()
#' @param output_path Output file path
#' @param format Output format ("csv", "xlsx", "json")
#' @param include_feedback Include detailed feedback (default: TRUE)
#' @return Path to exported file
#' @export
#' @examples
#' \dontrun{
#' export_results(results, "output/grades.csv")
#' export_results(results, "output/grades.xlsx", format = "xlsx")
#' }
export_results <- function(results, output_path, format = NULL, include_feedback = TRUE) {
  
  # Auto-detect format from file extension
  if (is.null(format)) {
    format <- tools::file_ext(output_path)
  }
  
  # Prepare data for export
  export_data <- results
  
  if (!include_feedback) {
    feedback_cols <- grep("feedback|justification", names(export_data), value = TRUE)
    export_data <- export_data[, !names(export_data) %in% feedback_cols, drop = FALSE]
  }
  
  # Create output directory if needed
  output_dir <- dirname(output_path)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Export based on format
  switch(format,
    "csv" = {
      readr::write_csv(export_data, output_path)
    },
    "xlsx" = {
      if (!requireNamespace("openxlsx", quietly = TRUE)) {
        stop("Package 'openxlsx' is needed for Excel export")
      }
      openxlsx::write.xlsx(export_data, output_path)
    },
    "json" = {
      jsonlite::write_json(export_data, output_path, pretty = TRUE)
    },
    stop("Unsupported format: ", format, ". Use 'csv', 'xlsx', or 'json'")
  )
  
  logger::log_info("Results exported to: {output_path}")
  return(output_path)
}

#' Create Grading Report
#'
#' Generate a summary report of grading results
#'
#' @param results Grading results from grade_responses()
#' @param output_path Output file path for report
#' @return Path to generated report
#' @export
create_grading_report <- function(results, output_path) {
  
  # Calculate summary statistics
  summary_stats <- list(
    total_responses = nrow(results),
    mean_score = mean(results$total_score, na.rm = TRUE),
    median_score = median(results$total_score, na.rm = TRUE),
    min_score = min(results$total_score, na.rm = TRUE),
    max_score = max(results$total_score, na.rm = TRUE),
    score_distribution = table(results$total_score)
  )
  
  # Create report content
  report_content <- sprintf("
# Grading Report

## Summary Statistics
- Total Responses: %d
- Mean Score: %.2f
- Median Score: %.2f  
- Min Score: %.2f
- Max Score: %.2f

## Score Distribution
%s

Generated on: %s
", 
    summary_stats$total_responses,
    summary_stats$mean_score,
    summary_stats$median_score,
    summary_stats$min_score,
    summary_stats$max_score,
    paste(capture.output(print(summary_stats$score_distribution)), collapse = "\n"),
    Sys.time()
  )
  
  # Write report
  writeLines(report_content, output_path)
  
  logger::log_info("Grading report created: {output_path}")
  return(output_path)
}