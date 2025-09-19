#' Grade Student Responses
#'
#' Grade multiple student responses using LLM and rubric with optional parallel processing
#'
#' @param responses Data frame with student responses
#' @param rubric Rubric object from load_rubric()
#' @param key Answer key object from load_answer_key() (optional)
#' @param template_path Path to grading template file
#' @param model_config List with model configuration (model name, etc.)
#' @param system_prompt Custom system prompt for LLM (optional)
#' @param .progress Show progress bar with percentage (default: TRUE)
#' @param .parallel Use parallel processing (default: FALSE)
#' @param .cores Number of cores for parallel processing (default: detectCores() - 1)
#' @return Data frame with grading results
#' @export
#' @examples
#' \dontrun{
#' responses <- read_csv("data/responses.csv")
#' rubric <- load_rubric("rubric/item_001.md")
#' key <- load_answer_key("rubric/item_001_key.md")
#' 
#' # Sequential processing (default)
#' results <- grade_responses(responses, rubric, key)
#' 
#' # Parallel processing with 4 cores
#' results <- grade_responses(responses, rubric, key, .parallel = TRUE, .cores = 4)
#' 
#' # Custom system prompt - strict grading
#' strict_prompt <- "ให้คะแนนอย่างเข้มงวดตาม rubric ไม่ให้คะแนนเต็มง่ายๆ"
#' results <- grade_responses(responses, rubric, key, system_prompt = strict_prompt)
#' 
#' # Custom system prompt - encouraging feedback
#' encouraging_prompt <- "ให้คะแนนที่เป็นธรรมและ feedback ที่สร้างสรรค์"
#' results <- grade_responses(responses, rubric, key, system_prompt = encouraging_prompt)
#' 
#' # Complete example with all options
#' results <- grade_responses(
#'   responses = responses,
#'   rubric = rubric,
#'   key = key,
#'   system_prompt = "ประเมินอย่างรอบด้านและให้คำแนะนำที่เป็นประโยชน์",
#'   model_config = "gpt-5-nano",
#'   .parallel = TRUE,
#'   .cores = 4,
#'   .progress = TRUE
#' )
#' }
grade_responses <- function(responses, rubric, key = NULL, 
                           template_path = NULL, model_config = NULL, 
                           system_prompt = NULL,
                           .temperature = NULL,
                           .top_p = NULL,
                           .progress = TRUE, .parallel = FALSE, .cores = NULL) {
  
  # Set defaults
  if (is.null(template_path)) {
    template_path <- system.file("templates", "grade_template.md", package = "kruroograder")
  }
  # Handle model_config parameter
  if (is.null(model_config)) {
    model_config <- list(model = "gpt-5-nano", provider = "openai")
  } else if (is.character(model_config)) {
    # If user passes just model name as string
    model_config <- list(model = model_config, provider = "openai")
  } else if (!is.list(model_config)) {
    stop("model_config must be a list or character string")
  }
  if (is.null(.cores)) {
    .cores <- max(1, future::availableCores() - 1)
  }
  if (is.null(system_prompt)) {
    system_prompt <- "คุณเป็นอาจารย์ผู้เชี่ยวชาญด้าน data-driven classroom มีหน้าที่ประเมินผลการตอบของนักเรียนอย่างตามเกณฑ์ที่กำหนดอย่างตรงไปตรงมาและเป็นธรรม คงเส้นคงวาในการให้คะแนน ใช้หลักการประเมินแบบเดียวกันสำหรับทุกคำตอบ"
  }
  
  # Create grading schema
  schema <- create_grading_schema()
  
  # Define the grading function for a single response
  grade_single_response <- function(i, p = NULL) {
    # Progress update
    if (.progress && !is.null(p)) p()
    
    # Define normalize function inline to avoid export issues
    normalize_per_criterion_local <- function(per_criterion) {
      if (is.null(per_criterion) || length(per_criterion) == 0) {
        return(list())
      }
      
      # If it's already a data.frame, convert to list
      if (is.data.frame(per_criterion)) {
        result <- vector("list", nrow(per_criterion))
        for (idx in seq_len(nrow(per_criterion))) {
          result[[idx]] <- list(
            id = as.character(per_criterion$id[idx] %||% paste0("C", idx)),
            score = as.numeric(per_criterion$score[idx] %||% 0),
            justification = as.character(per_criterion$justification[idx] %||% "")
          )
        }
        return(result)
      }
      
      # If it's a list, ensure each element has consistent structure
      if (is.list(per_criterion)) {
        if (all(sapply(per_criterion, is.list))) {
          result <- vector("list", length(per_criterion))
          for (idx in seq_along(per_criterion)) {
            criterion <- per_criterion[[idx]]
            result[[idx]] <- list(
              id = as.character(criterion$id %||% paste0("C", idx)),
              score = as.numeric(criterion$score %||% 0),
              justification = as.character(criterion$justification %||% "")
            )
          }
          return(result)
        }
      }
      
      # Fallback
      return(list())
    }
    
    tryCatch({
      response_row <- responses[i, ]
      
      # Ensure response_row is a data.frame, not a vector
      if (!is.data.frame(response_row)) {
        response_row <- as.data.frame(response_row)
      }
      
      # Render prompt
      prompt <- render_prompt(template_path, rubric, key, response_row)
      
      # Create messages
      messages <- tibble::tibble(
        role = c("system", "user"),
        content = c(
          system_prompt,  # Use the configurable system prompt
          prompt
        )
      ) %>%
      tidyllm::df_llm_message()
      
      # Call LLM
      result <- tidyllm::chat(
        messages, 
        tidyllm::openai(),
        .model = as.character(model_config$model %||% "gpt-5-nano"),
        .json_schema = schema,
        .temperature = .temperature ,
        .top_p = .top_p
      )
      
      # Extract and format results
      graded_result <- tidyllm::get_reply_data(result)
      per_criterion_normalized <- normalize_per_criterion_local(graded_result$per_criterion %||% list())
      
      list(
        student_id = as.character(response_row$student_id[1] %||% "unknown"),  # Add [1] for safety
        item_id = as.character(graded_result$item_id %||% response_row$item_id[1] %||% "unknown"),
        total_score = as.numeric(graded_result$total_score %||% 0),
        per_criterion = per_criterion_normalized,
        overall_feedback = as.character(graded_result$overall_feedback %||% "")
      )
      
    }, error = function(e) {
      logger::log_error("Error grading response {i}: {e$message}")
      # Safely access response_row in error case
      safe_student_id <- tryCatch(
        as.character(responses[i, ]$student_id[1] %||% "unknown"),
        error = function(e2) "error"
      )
      safe_item_id <- tryCatch(
        as.character(responses[i, ]$item_id[1] %||% "unknown"),
        error = function(e2) "error"
      )
      
      list(
        student_id = safe_student_id,
        item_id = safe_item_id,
        total_score = 0,
        per_criterion = list(),
        overall_feedback = paste("Error during grading:", e$message)
      )
    })
  }
  
  # Setup future plan
  n_responses <- nrow(responses)
  indices <- seq_len(n_responses)
  
  logger::log_info("Starting grading of {n_responses} responses")
  logger::log_info("Parallel processing: {.parallel}, Cores: {.cores}")
  
  # Configure future plan
  if (.parallel && n_responses > 1) {
    future::plan(future::multisession, workers = .cores)
    logger::log_info("Using multisession with {.cores} workers")
  } else {
    future::plan(future::sequential)
    logger::log_info("Using sequential processing")
  }
  
  # Ensure plan is reset on exit
  on.exit(future::plan(future::sequential), add = TRUE)
  
  # Grade responses with progress tracking
  if (.progress) {
    results <- progressr::with_progress({
      p <- progressr::progressor(steps = n_responses)
      furrr::future_map(indices, ~grade_single_response(.x, p), .options = furrr::furrr_options(seed = TRUE))
    })
  } else {
    results <- furrr::future_map(indices, ~grade_single_response(.x), .options = furrr::furrr_options(seed = TRUE))
  }
  
  logger::log_info("Grading completed")
  
  # Combine results into a proper tibble
  combine_grading_results(results)
}

#' Normalize Per-Criterion Data Structure
#'
#' Convert per_criterion data to consistent list format regardless of input type
#'
#' @param per_criterion Per-criterion data from LLM response
#' @return Normalized list of criteria
#' @keywords internal
normalize_per_criterion <- function(per_criterion) {
  
  if (is.null(per_criterion) || length(per_criterion) == 0) {
    return(list())
  }
  
  # If it's already a data.frame, convert to list
  if (is.data.frame(per_criterion)) {
    # Convert data.frame rows to list of lists
    result <- vector("list", nrow(per_criterion))
    for (i in seq_len(nrow(per_criterion))) {
      result[[i]] <- list(
        id = as.character(per_criterion$id[i] %||% paste0("C", i)),
        score = as.numeric(per_criterion$score[i] %||% 0),
        justification = as.character(per_criterion$justification[i] %||% "")
      )
    }
    return(result)
  }
  
  # If it's a list, ensure each element has consistent structure
  if (is.list(per_criterion)) {
    # Check if it's a list of lists (what we want)
    if (all(sapply(per_criterion, is.list))) {
      # Normalize each criterion
      result <- vector("list", length(per_criterion))
      for (i in seq_along(per_criterion)) {
        criterion <- per_criterion[[i]]
        result[[i]] <- list(
          id = as.character(criterion$id %||% paste0("C", i)),
          score = as.numeric(criterion$score %||% 0),
          justification = as.character(criterion$justification %||% "")
        )
      }
      return(result)
    } else {
      # It might be a flat list, try to restructure
      # This is a fallback for unexpected formats
      return(list(list(
        id = "unknown",
        score = 0,
        justification = "Unable to parse criterion data"
      )))
    }
  }
  
  # Fallback for any other type
  return(list())
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
      
      # Handle per_criterion with normalization
      if ("per_criterion" %in% names(result)) {
        per_criteria[[i]] <- normalize_per_criterion(result$per_criterion[[1]])
      } else {
        per_criteria[[i]] <- list()
      }
      
    } else if (is.list(result)) {
      # student_id now comes from input data, not LLM
      student_ids[i] <- as.character(result$student_id %||% "unknown")
      item_ids[i] <- as.character(result$item_id %||% "unknown")  
      total_scores[i] <- as.numeric(result$total_score %||% 0)
      overall_feedbacks[i] <- as.character(result$overall_feedback %||% "")
      per_criteria[[i]] <- normalize_per_criterion(result$per_criterion %||% list())
      
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