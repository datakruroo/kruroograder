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
      
      # Add row index for debugging
      graded_result$row_index <- i
      
      return(graded_result)
      
    }, error = function(e) {
      logger::log_error("Error grading response {i}: {e$message}")
      # Return error result
      return(tibble::tibble(
        row_index = i,
        student_id = responses[i, ]$student_id %||% NA_character_,
        item_id = responses[i, ]$item_id %||% NA_character_,
        total_score = NA_real_,
        overall_feedback = paste("Error during grading:", e$message),
        error = TRUE
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
  
  # Combine results
  combined_results <- do.call(rbind, results)
  
  # Remove row_index column if it exists
  if ("row_index" %in% names(combined_results)) {
    combined_results$row_index <- NULL
  }
  
  return(combined_results)
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