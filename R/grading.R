#' Grade Student Responses
#'
#' Grade multiple student responses using LLM and rubric with optional parallel processing
#'
#' @param responses Data frame with student responses
#' @param rubric Rubric object from load_rubric()
#' @param key Answer key object from load_answer_key() (optional)
#' @param template_path Path to grading template file
#' @param model_config List with model configuration (model name, etc.)
#' @param .progress Show progress bar with percentage (default: TRUE)
#' @param .parallel Use parallel processing (default: FALSE)
#' @param .cores Number of cores for parallel processing (default: detectCores() - 1)
#' @return Data frame with grading results
#' @export
#' @examples
#' \dontrun{
#' responses <- read_csv("data/responses.csv")
#' rubric <- load_rubric("rubric/item_001.md")
#' 
#' # Sequential processing
#' results <- grade_responses(responses, rubric)
#' 
#' # Parallel processing with 4 cores
#' results <- grade_responses(responses, rubric, .parallel = TRUE, .cores = 4)
#' }
grade_responses <- function(responses, rubric, key = NULL, 
                           template_path = NULL, model_config = NULL, 
                           .progress = TRUE, .parallel = FALSE, .cores = NULL) {
  
  # Set defaults
  if (is.null(template_path)) {
    template_path <- system.file("templates", "grade_template.md", package = "kruroograder")
  }
  if (is.null(model_config)) {
    model_config <- list(model = "gpt-5-nano", provider = "openai")
  }
  if (is.null(.cores)) {
    .cores <- max(1, parallel::detectCores() - 1)
  }
  
  # Create grading schema
  schema <- create_grading_schema()
  
  # Define the grading function for a single response
  grade_single_response <- function(i, responses, template_path, rubric, key, model_config, schema) {
    tryCatch({
      response_row <- responses[i, ]
      
      # Render prompt
      prompt <- render_prompt(template_path, rubric, key, response_row)
      
      # Create messages
      messages <- tibble::tibble(
        role = c("system", "user"),
        content = c(
          "คุณเป็นอาจารย์ผู้เชี่ยวชาญด้าน data-driven classroom มีหน้าที่ประเมินผลการตอบของนักเรียนอย่างตามเกณฑ์ที่กำหนดอย่างตรงไปตรงมาและเป็นธรรม คงเส้นคงวาในการให้คะแนน",
          prompt
        )
      ) %>%
      tidyllm::df_llm_message()
      
      # Call LLM
      result <- tidyllm::chat(
        messages, 
        tidyllm::openai(),
        .model = model_config$model,
        .json_schema = schema
      )
      
      # Extract results
      graded_result <- tidyllm::get_reply_data(result)
      
      # Ensure consistent structure - convert to list format
      formatted_result <- list(
        student_id = as.character(graded_result$student_id %||% response_row$student_id %||% "unknown"),
        item_id = as.character(graded_result$item_id %||% response_row$item_id %||% "unknown"),
        total_score = as.numeric(graded_result$total_score %||% 0),
        per_criterion = graded_result$per_criterion %||% list(),
        overall_feedback = as.character(graded_result$overall_feedback %||% "")
      )
      
      return(formatted_result)
      
    }, error = function(e) {
      logger::log_error("Error grading response {i}: {e$message}")
      # Return error result in consistent format
      return(list(
        student_id = as.character(responses[i, ]$student_id %||% "unknown"),
        item_id = as.character(responses[i, ]$item_id %||% "unknown"),
        total_score = 0,
        per_criterion = list(),
        overall_feedback = paste("Error during grading:", e$message)
      ))
    })
  }
  
  # Progress tracking function
  show_progress <- function(i, n) {
    if (.progress) {
      pct <- round(100 * i / n, 1)
      cat(sprintf("\rProgress: %d/%d (%s%%) completed", i, n, pct))
      if (i == n) cat("\n")
      flush.console()
    }
  }
  
  # Grade responses
  n_responses <- nrow(responses)
  indices <- seq_len(n_responses)
  
  logger::log_info("Starting grading of {n_responses} responses")
  logger::log_info("Parallel processing: {.parallel}, Cores: {.cores}")
  
  if (.parallel && n_responses > 1) {
    # Parallel processing
    if (.progress) {
      cat(sprintf("Starting parallel grading with %d cores...\n", .cores))
    }
    
    # Use pbapply for progress tracking with parallel processing
    if (.progress) {
      results <- pbapply::pblapply(
        indices, 
        grade_single_response,
        responses = responses,
        template_path = template_path,
        rubric = rubric,
        key = key,
        model_config = model_config,
        schema = schema,
        cl = .cores
      )
    } else {
      # Use parallel without progress bar
      cl <- parallel::makeCluster(.cores)
      on.exit(parallel::stopCluster(cl))
      
      # Export necessary objects to cluster
      parallel::clusterEvalQ(cl, {
        library(tibble)
        library(tidyllm)
        library(logger)
        library(glue)
        library(yaml)
      })
      parallel::clusterExport(cl, c("render_prompt", "read_file_all"), envir = environment())
      
      results <- parallel::parLapply(
        cl,
        indices,
        grade_single_response,
        responses = responses,
        template_path = template_path,
        rubric = rubric,
        key = key,
        model_config = model_config,
        schema = schema
      )
    }
    
  } else {
    # Sequential processing with enhanced progress
    results <- vector("list", n_responses)
    
    if (.progress) {
      cat("Starting sequential grading...\n")
    }
    
    for (i in indices) {
      results[[i]] <- grade_single_response(
        i, responses, template_path, rubric, key, model_config, schema
      )
      show_progress(i, n_responses)
    }
  }
  
  logger::log_info("Grading completed")
  
  # Combine results into a proper tibble
  combined_results <- combine_grading_results(results)
  
  return(combined_results)
}

#' Combine Grading Results
#'
#' Properly combine list of grading results into a tibble
#'
#' @param results List of grading results from individual responses
#' @return Tibble with properly structured results
#' @keywords internal
combine_grading_results <- function(results) {
  
  # Initialize vectors for each column
  n_results <- length(results)
  student_ids <- character(n_results)
  item_ids <- character(n_results)
  total_scores <- numeric(n_results)
  overall_feedbacks <- character(n_results)
  per_criteria <- vector("list", n_results)
  
  # Extract data from each result
  for (i in seq_along(results)) {
    result <- results[[i]]
    
    # Handle different result structures
    if (is.data.frame(result)) {
      student_ids[i] <- as.character(result$student_id[1] %||% "unknown")
      item_ids[i] <- as.character(result$item_id[1] %||% "unknown")
      total_scores[i] <- as.numeric(result$total_score[1] %||% 0)
      overall_feedbacks[i] <- as.character(result$overall_feedback[1] %||% "")
      
      # Handle per_criterion
      if ("per_criterion" %in% names(result)) {
        per_criteria[[i]] <- result$per_criterion[[1]]
      } else {
        per_criteria[[i]] <- list()
      }
      
    } else if (is.list(result)) {
      student_ids[i] <- as.character(result$student_id %||% "unknown")
      item_ids[i] <- as.character(result$item_id %||% "unknown")  
      total_scores[i] <- as.numeric(result$total_score %||% 0)
      overall_feedbacks[i] <- as.character(result$overall_feedback %||% "")
      per_criteria[[i]] <- result$per_criterion %||% list()
      
    } else {
      # Handle error cases
      student_ids[i] <- "error"
      item_ids[i] <- "error"
      total_scores[i] <- 0
      overall_feedbacks[i] <- "Error in processing"
      per_criteria[[i]] <- list()
    }
  }
  
  # Create tibble
  tibble::tibble(
    student_id = student_ids,
    item_id = item_ids,
    total_score = total_scores,
    per_criterion = per_criteria,
    overall_feedback = overall_feedbacks
  )
}

#' Create Grading Schema
#'
#' Create JSON schema for LLM grading output
#'
#' @return tidyllm schema object
#' @export
create_grading_schema <- function() {
  tidyllm::tidyllm_schema(
    name = "grading_schema",
    student_id = tidyllm::field_chr("student_id"),
    item_id = tidyllm::field_chr("item_id"),
    total_score = tidyllm::field_dbl("total_score"),
    per_criterion = tidyllm::field_object(
      id = tidyllm::field_chr("id"),
      score = tidyllm::field_dbl("score"),
      justification = tidyllm::field_chr("justification หากคะแนนไม่ได้เต็มให้ระบุเหตุผล โดยยกตัวอย่างจากคำตอบของนักเรียน มาอ้างอิงให้ชัดเจน")
    ),
    overall_feedback = tidyllm::field_chr("ระบุจุดแข็ง จุดที่ควรปรับปรุง และคำแนะนำเพิ่มเติม")
  )
}