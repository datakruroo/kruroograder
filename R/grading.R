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

# ===== MULTIAGENT GRADING SYSTEM =====

#' Check Grading Consistency (Agent 3)
#'
#' Compare two grading results to determine if they are consistent
#'
#' @param result1 First grading result
#' @param result2 Second grading result
#' @param rubric Rubric object
#' @param model_config Model configuration
#' @param consistency_threshold Score difference threshold (default: 0.2 of max_score)
#' @return List with is_consistent (boolean) and feedback (character)
#' @keywords internal
check_grading_consistency <- function(result1, result2, rubric, model_config = NULL, consistency_threshold = 0.2) {

  if (is.null(model_config)) {
    model_config <- list(model = "gpt-5-nano", provider = "openai")
  }

  # Calculate score difference
  score_diff <- abs(result1$total_score - result2$total_score)
  max_score <- rubric$max_score %||% 10
  relative_diff <- score_diff / max_score

  # Create consistency check schema
  consistency_schema <- tidyllm::tidyllm_schema(
    name = "consistency_check",
    is_consistent = tidyllm::field_lgl("true if the two grading results are reasonably consistent, false otherwise"),
    score_alignment = tidyllm::field_chr("analysis of how well the scores align"),
    criterion_alignment = tidyllm::field_chr("analysis of per-criterion score alignment"),
    feedback_for_regrading = tidyllm::field_chr("specific feedback for re-grading if inconsistent, empty if consistent")
  )

  # Build comparison prompt
  comparison_prompt <- glue::glue("
# บทบาท
คุณเป็น AI ตัวที่ 3 ทำหน้าที่ตรวจสอบความสอดคล้องระหว่างผลการตรวจของ AI สองตัว

# Rubric ที่ใช้ตรวจ
{yaml::as.yaml(rubric)}

# ผลการตรวจจาก AI ตัวที่ 1
คะแนนรวม: {result1$total_score}
คะแนนแต่ละหัวข้อ:
{paste(sapply(result1$per_criterion, function(c) glue::glue('- {c$id}: {c$score} คะแนน - {c$justification}')), collapse = '\n')}

Feedback รวม: {result1$overall_feedback}

# ผลการตรวจจาก AI ตัวที่ 2
คะแนนรวม: {result2$total_score}
คะแนนแต่ละหัวข้อ:
{paste(sapply(result2$per_criterion, function(c) glue::glue('- {c$id}: {c$score} คะแนน - {c$justification}')), collapse = '\n')}

Feedback รวม: {result2$overall_feedback}

# คำสั่ง
วิเคราะห์ว่าผลการตรวจทั้งสองสอดคล้องกันหรือไม่ โดยพิจารณา:
1. ความแตกต่างของคะแนนรวม (ปัจจุบัน: {score_diff} จาก {max_score} = {round(relative_diff * 100, 1)}%)
2. ความสอดคล้องของคะแนนในแต่ละหัวข้อ
3. ความสอดคล้องของเหตุผลที่ให้คะแนน

หากไม่สอดคล้อง ให้ระบุ feedback เฉพาะเจาะจงสำหรับการตรวจใหม่
หากสอดคล้อง ให้ระบุ feedback_for_regrading เป็นค่าว่าง
  ")

  # Call LLM to check consistency
  messages <- tibble::tibble(
    role = c("system", "user"),
    content = c(
      "คุณเป็นผู้เชี่ยวชาญในการตรวจสอบคุณภาพการตรวจข้อสอบ วิเคราะห์อย่างรอบคอบและเป็นกลาง",
      comparison_prompt
    )
  ) %>%
    tidyllm::df_llm_message()

  result <- tidyllm::chat(
    messages,
    tidyllm::openai(),
    .model = as.character(model_config$model %||% "gpt-5-nano"),
    .json_schema = consistency_schema
  )

  consistency_data <- tidyllm::get_reply_data(result)

  # Return consistency check result
  list(
    is_consistent = consistency_data$is_consistent %||% FALSE,
    score_alignment = consistency_data$score_alignment %||% "",
    criterion_alignment = consistency_data$criterion_alignment %||% "",
    feedback = consistency_data$feedback_for_regrading %||% "",
    score_difference = score_diff,
    relative_difference = relative_diff
  )
}

#' Synthesize Multiagent Feedback (Agent 4)
#'
#' Combine feedback from two graders into final student feedback
#'
#' @param result1 First grading result
#' @param result2 Second grading result
#' @param rubric Rubric object
#' @param response_row Student response data
#' @param model_config Model configuration
#' @return List with synthesized grading result
#' @keywords internal
synthesize_multiagent_feedback <- function(result1, result2, rubric, response_row, model_config = NULL) {

  if (is.null(model_config)) {
    model_config <- list(model = "gpt-5-nano", provider = "openai")
  }

  # Create synthesis schema
  synthesis_schema <- tidyllm::tidyllm_schema(
    name = "synthesis_schema",
    item_id = tidyllm::field_chr("item_id"),
    total_score = tidyllm::field_dbl("final consensus score"),
    per_criterion = tidyllm::field_object(
      id = tidyllm::field_chr("id"),
      score = tidyllm::field_dbl("final consensus score for this criterion"),
      justification = tidyllm::field_chr("synthesized justification combining insights from both graders")
    ),
    overall_feedback = tidyllm::field_chr("synthesized feedback for student combining strengths and suggestions from both graders"),
    grader_agreement_note = tidyllm::field_chr("brief note on how the two graders agreed or differed")
  )

  # Build synthesis prompt
  synthesis_prompt <- glue::glue("
# บทบาท
คุณเป็น AI ตัวที่ 4 ทำหน้าที่สังเคราะห์ feedback จากผู้ตรวจสองท่านให้เป็น feedback เดียวสำหรับนักเรียน

# Rubric
{yaml::as.yaml(rubric)}

# คำตอบของนักเรียน
{response_row$response_text}

# ผลการตรวจจากผู้ตรวจท่านที่ 1
คะแนนรวม: {result1$total_score}
คะแนนแต่ละหัวข้อ:
{paste(sapply(result1$per_criterion, function(c) glue::glue('- {c$id}: {c$score} คะแนน - {c$justification}')), collapse = '\n')}

Feedback: {result1$overall_feedback}

# ผลการตรวจจากผู้ตรวจท่านที่ 2
คะแนนรวม: {result2$total_score}
คะแนนแต่ละหัวข้อ:
{paste(sapply(result2$per_criterion, function(c) glue::glue('- {c$id}: {c$score} คะแนน - {c$justification}')), collapse = '\n')}

Feedback: {result2$overall_feedback}

# คำสั่ง
สังเคราะห์ผลการตรวจทั้งสองให้เป็นผลการตรวจฉบับสมบูรณ์สำหรับนักเรียน โดย:
1. ให้คะแนนที่เป็น consensus ระหว่างสองผู้ตรวจ (อาจเป็นค่าเฉลี่ย หรือค่าที่เหมาะสมกว่า)
2. รวม justification ที่ดีที่สุดจากทั้งสองผู้ตรวจ
3. สังเคราะห์ feedback ที่ครอบคลุมและเป็นประโยชน์สำหรับนักเรียน
4. ระบุสั้นๆ ว่าผู้ตรวจทั้งสองเห็นตรงกันหรือแตกต่างกันอย่างไร
  ")

  # Call LLM to synthesize
  messages <- tibble::tibble(
    role = c("system", "user"),
    content = c(
      "คุณเป็นผู้เชี่ยวชาญในการสังเคราะห์ผลการประเมิน ให้ความสำคัญกับการให้ feedback ที่เป็นประโยชน์และสร้างสรรค์",
      synthesis_prompt
    )
  ) %>%
    tidyllm::df_llm_message()

  result <- tidyllm::chat(
    messages,
    tidyllm::openai(),
    .model = as.character(model_config$model %||% "gpt-5-nano"),
    .json_schema = synthesis_schema
  )

  synthesis_data <- tidyllm::get_reply_data(result)

  # Normalize per_criterion
  per_criterion_normalized <- normalize_per_criterion(synthesis_data$per_criterion %||% list())

  # Return synthesized result
  list(
    student_id = as.character(response_row$student_id %||% "unknown"),
    item_id = as.character(synthesis_data$item_id %||% response_row$item_id %||% "unknown"),
    total_score = as.numeric(synthesis_data$total_score %||% 0),
    per_criterion = per_criterion_normalized,
    overall_feedback = as.character(synthesis_data$overall_feedback %||% ""),
    grader_agreement_note = as.character(synthesis_data$grader_agreement_note %||% ""),
    agent1_score = result1$total_score,
    agent2_score = result2$total_score
  )
}

#' Grade Single Response with Multiagent System
#'
#' Grade a single response using two independent graders with consistency checking
#'
#' @param response_row Single row of response data
#' @param rubric Rubric object
#' @param key Answer key object
#' @param template_path Path to template file
#' @param model_config Model configuration
#' @param system_prompt_agent1 System prompt for first grader
#' @param system_prompt_agent2 System prompt for second grader
#' @param max_iterations Maximum re-grading iterations (default: 2)
#' @param consistency_threshold Consistency threshold (default: 0.2)
#' @param .temperature Temperature parameter
#' @param .top_p Top_p parameter
#' @return Synthesized grading result
#' @keywords internal
grade_single_response_multiagent <- function(response_row, rubric, key, template_path,
                                             model_config, system_prompt_agent1, system_prompt_agent2,
                                             max_iterations = 2, consistency_threshold = 0.2,
                                             .temperature = NULL, .top_p = NULL) {

  schema <- create_grading_schema()
  prompt <- render_prompt(template_path, rubric, key, response_row)

  iteration <- 0
  is_consistent <- FALSE
  consistency_feedback <- ""

  # Define helper function to grade with specific agent
  grade_with_agent <- function(system_prompt, previous_feedback = NULL) {

    # Add feedback to prompt if re-grading
    final_prompt <- if (!is.null(previous_feedback) && previous_feedback != "") {
      paste0(prompt, "\n\n# Feedback จากการตรวจครั้งก่อน\n", previous_feedback, "\n\nโปรดตรวจใหม่โดยคำนึงถึง feedback นี้")
    } else {
      prompt
    }

    messages <- tibble::tibble(
      role = c("system", "user"),
      content = c(system_prompt, final_prompt)
    ) %>%
      tidyllm::df_llm_message()

    result <- tidyllm::chat(
      messages,
      tidyllm::openai(),
      .model = as.character(model_config$model %||% "gpt-5-nano"),
      .json_schema = schema,
      .temperature = .temperature,
      .top_p = .top_p
    )

    graded_result <- tidyllm::get_reply_data(result)
    per_criterion_normalized <- normalize_per_criterion(graded_result$per_criterion %||% list())

    list(
      student_id = as.character(response_row$student_id %||% "unknown"),
      item_id = as.character(graded_result$item_id %||% response_row$item_id %||% "unknown"),
      total_score = as.numeric(graded_result$total_score %||% 0),
      per_criterion = per_criterion_normalized,
      overall_feedback = as.character(graded_result$overall_feedback %||% "")
    )
  }

  # Iterative grading loop
  while (iteration <= max_iterations && !is_consistent) {
    iteration <- iteration + 1

    # Agent 1 and Agent 2: Grade independently
    result1 <- grade_with_agent(system_prompt_agent1, consistency_feedback)
    result2 <- grade_with_agent(system_prompt_agent2, consistency_feedback)

    # Agent 3: Check consistency
    consistency_check <- check_grading_consistency(
      result1, result2, rubric, model_config, consistency_threshold
    )

    is_consistent <- consistency_check$is_consistent
    consistency_feedback <- consistency_check$feedback

    # Log iteration result
    logger::log_debug("Iteration {iteration}: Consistent = {is_consistent}, Score diff = {consistency_check$score_difference}")

    # If consistent or max iterations reached, proceed to synthesis
    if (is_consistent || iteration > max_iterations) {
      # Agent 4: Synthesize feedback
      final_result <- synthesize_multiagent_feedback(
        result1, result2, rubric, response_row, model_config
      )

      # Add metadata
      final_result$iterations = iteration
      final_result$was_consistent = is_consistent
      final_result$consistency_score_diff = consistency_check$score_difference

      return(final_result)
    }
  }

  # This should not be reached, but as a safety net
  logger::log_warn("Multiagent grading did not converge properly")
  return(synthesize_multiagent_feedback(result1, result2, rubric, response_row, model_config))
}

#' Grade Student Responses with Multiagent System
#'
#' Grade responses using a multiagent system with two independent graders,
#' consistency checking, and feedback synthesis
#'
#' @param responses Data frame with student responses
#' @param rubric Rubric object from load_rubric()
#' @param key Answer key object from load_answer_key() (optional)
#' @param template_path Path to grading template file
#' @param model_config List with model configuration (model name, etc.)
#' @param system_prompt_agent1 Custom system prompt for first grader (optional)
#' @param system_prompt_agent2 Custom system prompt for second grader (optional)
#' @param max_iterations Maximum re-grading iterations when inconsistent (default: 2)
#' @param consistency_threshold Score difference threshold for consistency (default: 0.2)
#' @param .temperature Temperature parameter
#' @param .top_p Top_p parameter
#' @param .progress Show progress bar (default: TRUE)
#' @param .parallel Use parallel processing (default: FALSE)
#' @param .cores Number of cores for parallel processing
#' @return Data frame with grading results including multiagent metadata
#' @export
#' @examples
#' \dontrun{
#' responses <- read_csv("data/responses.csv")
#' rubric <- load_rubric("rubric/item_001.md")
#' key <- load_answer_key("rubric/item_001_key.md")
#'
#' # Multiagent grading with default settings
#' results <- grade_responses_multiagent(responses, rubric, key)
#'
#' # With custom prompts for different perspectives
#' results <- grade_responses_multiagent(
#'   responses, rubric, key,
#'   system_prompt_agent1 = "ให้คะแนนโดยเน้นแนวคิดและความเข้าใจ",
#'   system_prompt_agent2 = "ให้คะแนนโดยเน้นรายละเอียดและความสมบูรณ์",
#'   max_iterations = 3
#' )
#'
#' # With parallel processing
#' results <- grade_responses_multiagent(
#'   responses, rubric, key,
#'   .parallel = TRUE,
#'   .cores = 4
#' )
#' }
grade_responses_multiagent <- function(responses, rubric, key = NULL,
                                       template_path = NULL, model_config = NULL,
                                       system_prompt_agent1 = NULL, system_prompt_agent2 = NULL,
                                       max_iterations = 2, consistency_threshold = 0.2,
                                       .temperature = NULL, .top_p = NULL,
                                       .progress = TRUE, .parallel = FALSE, .cores = NULL) {

  # Set defaults
  if (is.null(template_path)) {
    template_path <- system.file("templates", "grade_template.md", package = "kruroograder")
  }

  if (is.null(model_config)) {
    model_config <- list(model = "gpt-5-nano", provider = "openai")
  } else if (is.character(model_config)) {
    model_config <- list(model = model_config, provider = "openai")
  }

  if (is.null(.cores)) {
    .cores <- max(1, future::availableCores() - 1)
  }

  # Default system prompts with different perspectives
  if (is.null(system_prompt_agent1)) {
    system_prompt_agent1 <- "คุณเป็นอาจารย์ผู้ตรวจข้อสอบท่านที่ 1 ให้ความสำคัญกับความเข้าใจแนวคิดหลักและการคิดวิเคราะห์ ประเมินอย่างรอบคอบและเป็นธรรม"
  }

  if (is.null(system_prompt_agent2)) {
    system_prompt_agent2 <- "คุณเป็นอาจารย์ผู้ตรวจข้อสอบท่านที่ 2 ให้ความสำคัญกับรายละเอียดและความสมบูรณ์ของคำตอบ ประเมินอย่างรอบคอบและเป็นธรรม"
  }

  # Define grading function for single response
  grade_single <- function(i, p = NULL) {
    if (.progress && !is.null(p)) p()

    tryCatch({
      response_row <- responses[i, ]
      if (!is.data.frame(response_row)) {
        response_row <- as.data.frame(response_row)
      }

      grade_single_response_multiagent(
        response_row, rubric, key, template_path,
        model_config, system_prompt_agent1, system_prompt_agent2,
        max_iterations, consistency_threshold,
        .temperature, .top_p
      )

    }, error = function(e) {
      logger::log_error("Error in multiagent grading for response {i}: {e$message}")
      safe_student_id <- tryCatch(
        as.character(responses[i, ]$student_id %||% "unknown"),
        error = function(e2) "error"
      )
      safe_item_id <- tryCatch(
        as.character(responses[i, ]$item_id %||% "unknown"),
        error = function(e2) "error"
      )

      list(
        student_id = safe_student_id,
        item_id = safe_item_id,
        total_score = 0,
        per_criterion = list(),
        overall_feedback = paste("Error during multiagent grading:", e$message),
        grader_agreement_note = "Error occurred",
        agent1_score = 0,
        agent2_score = 0,
        iterations = 0,
        was_consistent = FALSE
      )
    })
  }

  # Setup processing
  n_responses <- nrow(responses)
  indices <- seq_len(n_responses)

  logger::log_info("Starting multiagent grading of {n_responses} responses")
  logger::log_info("Max iterations: {max_iterations}, Consistency threshold: {consistency_threshold}")
  logger::log_info("Parallel processing: {.parallel}, Cores: {.cores}")

  # Configure future plan
  if (.parallel && n_responses > 1) {
    future::plan(future::multisession, workers = .cores)
    logger::log_info("Using multisession with {.cores} workers")
  } else {
    future::plan(future::sequential)
    logger::log_info("Using sequential processing")
  }

  on.exit(future::plan(future::sequential), add = TRUE)

  # Grade responses with progress tracking
  if (.progress) {
    results <- progressr::with_progress({
      p <- progressr::progressor(steps = n_responses)
      furrr::future_map(indices, ~grade_single(.x, p), .options = furrr::furrr_options(seed = TRUE))
    })
  } else {
    results <- furrr::future_map(indices, ~grade_single(.x), .options = furrr::furrr_options(seed = TRUE))
  }

  logger::log_info("Multiagent grading completed")

  # Combine results
  combine_multiagent_results(results)
}

#' Combine Multiagent Grading Results
#'
#' Combine list of multiagent grading results into a tibble
#'
#' @param results List of grading results
#' @return Tibble with structured results
#' @keywords internal
combine_multiagent_results <- function(results) {

  n_results <- length(results)
  student_ids <- character(n_results)
  item_ids <- character(n_results)
  total_scores <- numeric(n_results)
  overall_feedbacks <- character(n_results)
  per_criteria <- vector("list", n_results)
  grader_agreement_notes <- character(n_results)
  agent1_scores <- numeric(n_results)
  agent2_scores <- numeric(n_results)
  iterations <- integer(n_results)
  was_consistent <- logical(n_results)
  consistency_score_diffs <- numeric(n_results)

  for (i in seq_along(results)) {
    result <- results[[i]]

    student_ids[i] <- as.character(result$student_id %||% "unknown")
    item_ids[i] <- as.character(result$item_id %||% "unknown")
    total_scores[i] <- as.numeric(result$total_score %||% 0)
    overall_feedbacks[i] <- as.character(result$overall_feedback %||% "")
    per_criteria[[i]] <- normalize_per_criterion(result$per_criterion %||% list())
    grader_agreement_notes[i] <- as.character(result$grader_agreement_note %||% "")
    agent1_scores[i] <- as.numeric(result$agent1_score %||% 0)
    agent2_scores[i] <- as.numeric(result$agent2_score %||% 0)
    iterations[i] <- as.integer(result$iterations %||% 0)
    was_consistent[i] <- as.logical(result$was_consistent %||% FALSE)
    consistency_score_diffs[i] <- as.numeric(result$consistency_score_diff %||% 0)
  }

  tibble::tibble(
    student_id = student_ids,
    item_id = item_ids,
    total_score = total_scores,
    per_criterion = per_criteria,
    overall_feedback = overall_feedbacks,
    grader_agreement_note = grader_agreement_notes,
    agent1_score = agent1_scores,
    agent2_score = agent2_scores,
    score_difference = abs(agent1_scores - agent2_scores),
    iterations = iterations,
    was_consistent = was_consistent,
    consistency_score_diff = consistency_score_diffs
  )
}