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
  available_cores <- parallel::detectCores()
  
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
  return(n_tasks >= min_tasks_for_parallel && parallel::detectCores() > 1)
}