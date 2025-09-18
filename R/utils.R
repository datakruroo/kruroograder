#' Null-coalescing operator
#'
#' @param x Left-hand side
#' @param y Right-hand side  
#' @return x if not NULL, otherwise y
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Initialize Logger
#'
#' Set up logging for the kruroograder package
#'
#' @param log_file Path to log file (optional)
#' @param log_level Log level (default: "INFO")
#' @export
init_logger <- function(log_file = NULL, log_level = "INFO") {
  logger::log_threshold(log_level)
  
  if (!is.null(log_file)) {
    logger::log_appender(logger::appender_file(log_file))
  }
  
  logger::log_info("Logger initialized")
}

#' Validate Response Data
#'
#' Check that response data has required columns
#'
#' @param responses Data frame with student responses
#' @return TRUE if valid, otherwise throws error
#' @export
validate_response_data <- function(responses) {
  required_cols <- c("student_id", "item_id", "response_text")
  missing_cols <- setdiff(required_cols, names(responses))
  
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  if (nrow(responses) == 0) {
    stop("Response data is empty")
  }
  
  TRUE
}

#' Get Optimal Number of Cores for Parallel Processing
#'
#' Determine the optimal number of cores based on system and data size
#'
#' @param n_tasks Number of tasks to process
#' @param max_cores Maximum cores to use (optional)
#' @return Recommended number of cores
#' @export
get_optimal_cores <- function(n_tasks, max_cores = NULL) {
  available_cores <- future::availableCores()
  
  if (is.null(max_cores)) {
    # Use at most available cores - 1, but never more than n_tasks
    max_cores <- max(1, available_cores - 1)
  }
  
  # Don't use more cores than tasks
  optimal_cores <- min(max_cores, n_tasks, available_cores)
  
  # For small tasks, parallel overhead might not be worth it
  if (n_tasks < 4) {
    optimal_cores <- 1
  }
  
  return(optimal_cores)
}

#' Check if Parallel Processing is Beneficial
#'
#' Determine if parallel processing would be beneficial for the given task
#'
#' @param n_tasks Number of tasks to process
#' @param min_tasks_for_parallel Minimum tasks to justify parallel processing
#' @return TRUE if parallel processing is recommended
#' @export
should_use_parallel <- function(n_tasks, min_tasks_for_parallel = 4) {
  return(n_tasks >= min_tasks_for_parallel && future::availableCores() > 1)
}

#' View Grading Results Details
#'
#' Show detailed results for a specific student
#'
#' @param results Grading results from grade_responses()
#' @param student_index Row number of student (default: 1)
#' @return List with detailed grading information
#' @export
view_student_results <- function(results, student_index = 1) {
  if (!is.data.frame(results) || nrow(results) < student_index) {
    stop("Invalid results or student index")
  }
  
  student_row <- results[student_index, ]
  
  cat("=== STUDENT GRADING DETAILS ===\n")
  cat("Student ID:", student_row$student_id, "\n")
  cat("Item ID:", student_row$item_id, "\n") 
  cat("Total Score:", student_row$total_score, "\n")
  cat("\n--- PER CRITERION SCORES ---\n")
  
  criteria <- student_row$per_criterion[[1]]
  if (is.list(criteria) && length(criteria) > 0) {
    for (i in seq_along(criteria)) {
      criterion <- criteria[[i]]
      if (is.list(criterion)) {
        cat("Criterion", criterion$id %||% i, ":\n")
        cat("  Score:", criterion$score %||% "N/A", "\n")
        cat("  Justification:", criterion$justification %||% "N/A", "\n\n")
      }
    }
  } else {
    cat("No detailed criteria available\n")
  }
  
  cat("--- OVERALL FEEDBACK ---\n")
  cat(student_row$overall_feedback, "\n")
  
  invisible(student_row)
}

#' Extract Criteria Scores
#'
#' Extract per-criterion scores into a more accessible format
#'
#' @param results Grading results from grade_responses()
#' @return Tibble with expanded criteria scores
#' @export
extract_criteria_scores <- function(results) {
  if (!is.data.frame(results)) {
    stop("Results must be a data frame")
  }
  
  expanded_rows <- vector("list", nrow(results))
  
  for (i in seq_len(nrow(results))) {
    student_id <- results$student_id[i]
    item_id <- results$item_id[i]
    total_score <- results$total_score[i]
    criteria <- results$per_criterion[[i]]
    
    if (is.list(criteria) && length(criteria) > 0) {
      criterion_rows <- vector("list", length(criteria))
      
      for (j in seq_along(criteria)) {
        criterion <- criteria[[j]]
        criterion_rows[[j]] <- tibble::tibble(
          student_id = student_id,
          item_id = item_id,
          total_score = total_score,
          criterion_id = criterion$id %||% paste0("C", j),
          criterion_score = criterion$score %||% 0,
          justification = criterion$justification %||% ""
        )
      }
      
      expanded_rows[[i]] <- do.call(rbind, criterion_rows)
    } else {
      # No criteria available
      expanded_rows[[i]] <- tibble::tibble(
        student_id = student_id,
        item_id = item_id, 
        total_score = total_score,
        criterion_id = "unknown",
        criterion_score = 0,
        justification = "No criteria data available"
      )
    }
  }
  
  do.call(rbind, expanded_rows)
}

#' Debug Per-Criterion Structure
#'
#' Analyze the structure of per_criterion data for debugging
#'
#' @param results Grading results from grade_responses()
#' @export
debug_per_criterion <- function(results) {
  if (!is.data.frame(results)) {
    stop("Results must be a data frame")
  }
  
  cat("=== PER-CRITERION STRUCTURE ANALYSIS ===\n\n")
  
  for (i in seq_len(nrow(results))) {
    cat("Student", i, "(", results$student_id[i], "):\n")
    cat("  Type:", class(results$per_criterion[[i]])[1], "\n")
    cat("  Length:", length(results$per_criterion[[i]]), "\n")
    
    if (is.data.frame(results$per_criterion[[i]])) {
      cat("  Columns:", paste(names(results$per_criterion[[i]]), collapse = ", "), "\n")
      cat("  Rows:", nrow(results$per_criterion[[i]]), "\n")
    } else if (is.list(results$per_criterion[[i]])) {
      cat("  List elements:", length(results$per_criterion[[i]]), "\n")
      if (length(results$per_criterion[[i]]) > 0) {
        first_elem <- results$per_criterion[[i]][[1]]
        cat("  First element type:", class(first_elem)[1], "\n")
        if (is.list(first_elem)) {
          cat("  First element fields:", paste(names(first_elem), collapse = ", "), "\n")
        }
      }
    }
    cat("\n")
  }
}