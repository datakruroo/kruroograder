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

#' Split PDF by Question for Exam Grading
#'
#' @title แยก PDF ตามข้อสอบสำหรับการตรวจข้อสอบ
#' @description 
#' แยกไฟล์ PDF ที่มีกระดาษคำตอบของนักเรียนหลายคนออกเป็นไฟล์แยกตามข้อสอบ
#' เหมาะสำหรับกรณีที่สแกนกระดาษคำตอบแบบเรียงลำดับ (นักเรียนคนที่ 1 ทุกข้อ, 
#' แล้วนักเรียนคนที่ 2 ทุกข้อ, ...)
#' 
#' @param input_pdf ชื่อไฟล์ PDF ต้นฉบับที่ต้องการแยก
#' @param n_questions จำนวนข้อสอบทั้งหมด (default: 4)
#' @param out_dir โฟลเดอร์ปลายทางสำหรับเก็บไฟล์ที่แยกแล้ว (default: ".")
#' @param filename_prefix คำนำหน้าชื่อไฟล์ผลลัพธ์ (default: "Q")
#' @param filename_suffix คำต่อท้ายชื่อไฟล์ผลลัพธ์ (default: "_all.pdf")
#' @param dry_run ทดสอบการทำงานโดยไม่สร้างไฟล์จริง (default: FALSE)
#' @param overwrite เขียนทับไฟล์เดิมหากมีอยู่แล้ว (default: FALSE)
#'
#' @return 
#' List ที่ประกอบด้วย:
#' \describe{
#'   \item{pages_by_question}{Named list ของหมายเลขหน้าที่เป็นของแต่ละข้อ}
#'   \item{summary}{List ข้อมูลสรุป ประกอบด้วย total_pages, n_questions, n_students, etc.}
#' }
#'
#' @details
#' ฟังก์ชันนี้ใช้หลักการที่ว่า หากมีนักเรียน n คน และข้อสอบ m ข้อ 
#' หน้าที่ 1,1+m,1+2m,... จะเป็นข้อที่ 1 ของนักเรียนแต่ละคน
#' หน้าที่ 2,2+m,2+2m,... จะเป็นข้อที่ 2 ของนักเรียนแต่ละคน
#' และต่อไปเรื่อยๆ
#' 
#' หากจำนวนหน้าไม่หารลงตัวกับจำนวนข้อสอบ ฟังก์ชันจะแสดง warning
#' และประมวลผลเฉพาะหน้าที่ครบชุดเท่านั้น
#'
#' @examples
#' \dontrun{
#' # ตัวอย่างการใช้งานพื้นฐาน
#' result <- split_pdf_by_question("exam_scanned.pdf", n_questions = 4)
#' 
#' # ทดสอบการทำงานก่อน (dry run)
#' test_result <- split_pdf_by_question(
#'   "exam_scanned.pdf", 
#'   n_questions = 4,
#'   dry_run = TRUE
#' )
#' print(test_result$summary)
#' 
#' # การใช้งานแบบกำหนดค่าเต็มรูปแบบ
#' result <- split_pdf_by_question(
#'   input_pdf = "midterm_exam_2024.pdf",
#'   n_questions = 6,
#'   out_dir = "separated_questions",
#'   filename_prefix = "Question_",
#'   filename_suffix = "_AllStudents.pdf",
#'   overwrite = TRUE
#' )
#' 
#' # ตรวจสอบผลลัพธ์
#' cat("แยกได้", length(result$pages_by_question), "ไฟล์\n")
#' cat("นักเรียนทั้งหมด", result$summary$n_students, "คน\n")
#' }
#' 
#' @seealso 
#' \code{\link{inspect_pdf_structure}} สำหรับตรวจสอบโครงสร้าง PDF ก่อนแยก
#' \code{\link{prepare_pdf_for_grading}} สำหรับเตรียมข้อมูลสำหรับ kruroograder
#' 
#' @importFrom pdftools pdf_length pdf_subset
#' @export
split_pdf_by_question <- function(input_pdf,
                                  n_questions = 4,
                                  out_dir = ".",
                                  filename_prefix = "Q",
                                  filename_suffix = "_all.pdf",
                                  dry_run = FALSE,
                                  overwrite = FALSE) {
  
  # Input validation
  stopifnot(file.exists(input_pdf))
  stopifnot(is.numeric(n_questions) && n_questions > 0 && n_questions == round(n_questions))
  stopifnot(is.character(out_dir) && length(out_dir) == 1)
  stopifnot(is.logical(dry_run) && is.logical(overwrite))
  
  n_pages <- pdftools::pdf_length(input_pdf)
  
  # ตรวจสอบความเหมาะสมของข้อมูล
  if (n_pages < n_questions) {
    stop("จำนวนหน้า (", n_pages, ") น้อยกว่าจำนวนข้อสอบ (", n_questions, ")")
  }
  
  n_students <- n_pages %/% n_questions
  if (n_pages %% n_questions != 0) {
    warning("จำนวนหน้า (", n_pages, ") ไม่หารด้วยจำนวนข้อสอบ (", n_questions, ") ลงตัว",
            "\nจะได้นักเรียน ", n_students, " คน (เหลือ ", n_pages %% n_questions, " หน้า)")
  }
  
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  # คำนวณหน้าสำหรับแต่ละข้อ
  pages_list <- lapply(seq_len(n_questions), function(q) {
    seq(from = q, to = n_pages, by = n_questions)
  })
  names(pages_list) <- paste0(filename_prefix, seq_len(n_questions))
  
  # สร้างไฟล์จริง (ถ้าไม่ใช่ dry run)
  output_files <- character(0)
  if (!dry_run) {
    cat("กำลังแยก PDF เป็น", n_questions, "ไฟล์...\n")
    
    for (q in seq_len(n_questions)) {
      out_file <- file.path(out_dir, paste0(filename_prefix, q, filename_suffix))
      
      # ตรวจสอบไฟล์มีอยู่แล้ว
      if (file.exists(out_file) && !overwrite) {
        warning("ไฟล์ ", out_file, " มีอยู่แล้ว (ข้าม)")
        next
      }
      
      # พยายามสร้างไฟล์พร้อม error handling
      tryCatch({
        pdftools::pdf_subset(input = input_pdf, pages = pages_list[[q]], output = out_file)
        output_files <- c(output_files, out_file)
        cat("✓ สร้าง", basename(out_file), "(", length(pages_list[[q]]), "หน้า)\n")
      }, error = function(e) {
        warning("ไม่สามารถสร้างไฟล์ ", out_file, ": ", e$message)
      })
    }
  }
  
  # เตรียมผลลัพธ์
  result <- list(
    pages_by_question = pages_list,
    summary = list(
      input_file = input_pdf,
      total_pages = n_pages,
      n_questions = n_questions,
      n_students = n_students,
      pages_per_question = sapply(pages_list, length),
      output_directory = out_dir,
      output_files = if (!dry_run) output_files else NULL,
      dry_run = dry_run
    )
  )
  
  return(result)
}



#' Inspect PDF Structure for Question Separation
#'
#' @title ตรวจสอบโครงสร้าง PDF ก่อนการแยกตามข้อสอบ
#' @description 
#' วิเคราะห์และแสดงข้อมูลโครงสร้างของไฟล์ PDF เพื่อช่วยตัดสินใจว่า
#' เหมาะสมกับการแยกตามข้อสอบหรือไม่
#'
#' @param input_pdf ชื่อไฟล์ PDF ที่ต้องการตรวจสอบ
#' @param n_questions จำนวนข้อสอบที่คาดว่าจะแยก (default: 4)
#'
#' @return 
#' List (invisible) ที่ประกอบด้วย:
#' \describe{
#'   \item{n_pages}{จำนวนหน้าทั้งหมด}
#'   \item{n_students}{จำนวนนักเรียนที่คำนวณได้}
#'   \item{remainder}{จำนวนหน้าที่เหลือ (ถ้าไม่หารลงตัว)}
#' }
#'
#' @examples
#' \dontrun{
#' # ตรวจสอบโครงสร้าง PDF
#' inspect_pdf_structure("exam_scanned.pdf", n_questions = 4)
#' 
#' # เก็บผลลัพธ์สำหรับการวิเคราะห์เพิ่มเติม
#' structure_info <- inspect_pdf_structure("exam_scanned.pdf")
#' if (structure_info$remainder == 0) {
#'   message("PDF มีโครงสร้างที่เหมาะสมสำหรับการแยก")
#' }
#' }
#'
#' @seealso \code{\link{split_pdf_by_question}}
#' @importFrom pdftools pdf_length
#' @export
inspect_pdf_structure <- function(input_pdf, n_questions = 4) {
  n_pages <- pdf_length(input_pdf)
  n_students <- n_pages %/% n_questions
  remainder <- n_pages %% n_questions
  
  cat("=== PDF Structure Analysis ===\n")
  cat("ไฟล์:", basename(input_pdf), "\n")
  cat("จำนวนหน้าทั้งหมด:", n_pages, "\n")
  cat("จำนวนข้อสอบ:", n_questions, "\n")
  cat("จำนวนนักเรียน:", n_students, "\n")
  
  if (remainder > 0) {
    cat("⚠️  เหลือหน้า:", remainder, "(อาจไม่ครบชุด)\n")
  } else {
    cat("✓ โครงสร้างสมบูรณ์\n")
  }
  
  # แสดงตัวอย่างการแจกจ่ายหน้า
  cat("\n=== การกระจายหน้าตามข้อ ===\n")
  for (q in seq_len(min(n_questions, 3))) {  # แสดงแค่ 3 ข้อแรก
    pages <- seq(from = q, to = n_pages, by = n_questions)
    cat("ข้อ", q, ":", paste(head(pages, 5), collapse = ", "))
    if (length(pages) > 5) cat(", ... (รวม", length(pages), "หน้า)")
    cat("\n")
  }
  
  return(invisible(list(n_pages = n_pages, n_students = n_students, remainder = remainder)))
}

#' OCR Handwriting from Image using OpenAI Vision API
#'
#' @title แปลงลายมือจากภาพเป็นข้อความ
#' @param image_path เส้นทางไฟล์ภาพ
#' @param api_key OpenAI API key (ถ้าไม่ระบุจะใช้จาก environment)
#' @param system_prompt system prompt สำหรับการ OCR
#' @param model_config ชื่อโมเดลที่ใช้ (default: "gpt-4o-mini")
#' @param max_tokens จำนวน token สูงสุด (default: 500)
#' @return tibble ที่มี answer, prompt_tokens, completion_tokens, tokens_used
#' @export
ocr_handwriting <- function(image_path, 
                            api_key = NULL, 
                            system_prompt = NULL, 
                            model_config = NULL,
                            max_tokens = 500) {
  
  library(httr2)
  library(jsonlite)
  
  
  # ตรวจสอบไฟล์มีอยู่จริง
  if (!file.exists(image_path)) {
    stop("ไม่พบไฟล์ภาพ: ", image_path)
  }
  
  # ตั้งค่า API key
  if (is.null(api_key)) {
    api_key <- Sys.getenv("OPENAI_API_KEY")
    if (api_key == "" || is.null(api_key)) {
      stop("กรุณาตั้งค่า OPENAI_API_KEY ใน environment variables")
    }
  }
  
  
  # ตั้งค่า system prompt - แก้ไขจาก ifelse เป็น if
  if (is.null(system_prompt)) {
    system_prompt <- "คุณกำลังช่วยสกัดคำตอบจากแบบเขียนของนักเรียนที่ถูกแปลงเป็นข้อความจาก OCR
ในเนื้อหานี้จะมีคำตอบของนักเรียนที่เขียนด้วยลายมืออยู่ในกรอบสี่เหลี่ยม กรุณา
สกัดคำตอบทั้งหมดที่อยู่ภายในกรอบ โดยพยายามคงความหมายดั้งเดิมให้มากที่สุด และสกัดคำให้อยู่ในบริบทเดียวกัน
สอดคล้องกัน ออกมาให้ครบถ้วน **ไม่เอาบรรทัดที่เขียนว่า พื้นที่สำหรับตอบคำถาม ...**"
  }
  
  # ตั้งค่า model config
  if (is.null(model_config)) {
    model_config <- "gpt-4.1-mini"  
  }
  
  # ตรวจสอบนามสกุลไฟล์
  file_ext <- tools::file_ext(tolower(image_path))
  if (!file_ext %in% c("png", "jpg", "jpeg", "gif", "webp")) {
    warning("ไฟล์อาจไม่ใช่รูปภาพที่รองรับ: ", file_ext)
  }
  
  # อ่านไฟล์ภาพและแปลงเป็น base64
  tryCatch({
    img <- base64enc::base64encode(image_path)
  }, error = function(e) {
    stop("ไม่สามารถอ่านไฟล์ภาพได้: ", e$message)
  })
  
  # เตรียม MIME type
  mime_type <- switch(file_ext,
                      "png" = "image/png",
                      "jpg" = "image/jpeg", 
                      "jpeg" = "image/jpeg",
                      "gif" = "image/gif",
                      "webp" = "image/webp",
                      "image/png"  # default
  )
  
  # ส่งคำขอไปยัง OpenAI API
  tryCatch({
    resp <- request("https://api.openai.com/v1/chat/completions") |>
      req_headers(
        "Authorization" = paste("Bearer", api_key),
        "Content-Type" = "application/json"
      ) |>
      req_body_json(list(
        model = model_config,
        max_tokens = max_tokens,
        presence_penalty = 0,
        frequency_penalty = 0,
        temperature = 0.2,
        messages = list(
          list(role = "system", content = system_prompt),
          list(role = "user", content = list(
            list(type = "text", text = "ถอดข้อความจากภาพนี้ให้เป็นตัวอักษรไทย ไม่เอาส่วนที่เขียนว่า 'พื้นที่สำหรับตอบคำถาม ...' "),
            list(
              type = "image_url",
              image_url = list(url = paste0("data:", mime_type, ";base64,", img))
            )
          ))
        )
      )) |>
      req_perform()
    
  }, error = function(e) {
    stop("การเรียก API ล้มเหลว: ", e$message)
  })
  
  
  # ตรวจสอบ response status
  if (resp_status(resp) != 200) {
    stop("API response error: ", resp_status(resp), " - ", resp_status_desc(resp))
  }
  
  # แปลงผลลัพธ์เป็น JSON
  tryCatch({
    res_json <- resp_body_json(resp)
  }, error = function(e) {
    stop("ไม่สามารถแปลง response เป็น JSON ได้: ", e$message)
  })
  
  # ตรวจสอบ response structure
  if (is.null(res_json$choices) || length(res_json$choices) == 0) {
    stop("ไม่ได้รับคำตอบจาก API")
  }
  
  # เพิ่ม null coalescing operator ถ้าไม่มี
  `%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0 || (is.character(x) && x == "")) y else x
  }
  
  # แสดงผลลัพธ์
  output <- tibble::tibble(
    image_file = basename(image_path),
    answer = res_json$choices[[1]]$message$content %||% "",
    prompt_tokens = res_json$usage$prompt_tokens %||% 0,
    completion_tokens = res_json$usage$completion_tokens %||% 0,
    tokens_used = res_json$usage$total_tokens %||% 0,
    model_used = model_config,
    processed_at = Sys.time()
  )
  
  return(output)
}





### ---

#' Batch OCR Processing with Configurable System Prompt
#'
#' @title ประมวลผล OCR หลายไฟล์พร้อมกัน
#' @param image_paths vector ของ path ไฟล์รูป  
#' @param batch_size จำนวนไฟล์ต่อ batch (default: 10)
#' @param system_prompt system prompt สำหรับการ OCR (default: NULL ใช้ default ของ ocr_handwriting)
#' @param delay_seconds หน่วงเวลาระหว่างการเรียก API (default: 2)
#' @param progress แสดง progress (default: TRUE)
#' @param save_intermediate บันทึกผลลัพธ์ระหว่างทาง (default: FALSE)
#' @param ... arguments อื่นๆ ส่งต่อไปยัง ocr_handwriting()
#' @return data frame รวมผลลัพธ์ทั้งหมด
#' @export
ocr_batch_handwriting <- function(image_paths,
                                  batch_size = 10,
                                  system_prompt = NULL,
                                  delay_seconds = 2,
                                  progress = TRUE,
                                  save_intermediate = FALSE,
                                  ...) {
  
  library(purrr)
  library(dplyr)
  library(readr)
  
  # ตรวจสอบไฟล์ที่มีอยู่จริง
  existing_files <- image_paths[file.exists(image_paths)]
  missing_files <- image_paths[!file.exists(image_paths)]
  
  if (length(missing_files) > 0) {
    warning("ไฟล์ที่ไม่พบ: ", paste(basename(missing_files), collapse = ", "))
  }
  
  if (length(existing_files) == 0) {
    stop("ไม่มีไฟล์ที่สามารถประมวลผลได้")
  }
  
  # แบ่งเป็น batches
  n_batches <- ceiling(length(existing_files) / batch_size)
  
  cat("กำลังประมวลผล", length(existing_files), "ไฟล์\n")
  cat("แบ่งเป็น", n_batches, "batch (", batch_size, "ไฟล์/batch)\n")
  
  # แสดง system prompt ที่ใช้
  if (!is.null(system_prompt)) {
    cat("ใช้ custom system prompt:", substr(system_prompt, 1, 50), "...\n")
  } else {
    cat("ใช้ default system prompt\n")
  }
  
  # สร้าง safe version
  safe_ocr <- safely(ocr_handwriting, otherwise = NULL)
  
  # เตรียม progress bar
  if (progress) {
    pb <- txtProgressBar(min = 0, max = length(existing_files), style = 3)
    processed_count <- 0
  }
  
  all_results <- list()
  failed_files <- character(0)
  
  # ประมวลผลแต่ละ batch
  for (batch_num in 1:n_batches) {
    start_idx <- (batch_num - 1) * batch_size + 1
    end_idx <- min(batch_num * batch_size, length(existing_files))
    batch_files <- existing_files[start_idx:end_idx]
    
    cat("\nBatch", batch_num, "/", n_batches, ":", length(batch_files), "ไฟล์\n")
    
    # ประมวลผลไฟล์ใน batch นี้
    for (i in seq_along(batch_files)) {
      file_path <- batch_files[i]
      
      if (progress) {
        processed_count <- processed_count + 1
        setTxtProgressBar(pb, processed_count)
      }
      
      # เรียก OCR พร้อม system_prompt
      result <- safe_ocr(
        image_path = file_path, 
        system_prompt = system_prompt,  # ส่ง system_prompt ไปด้วย
        ...
      )
      
      if (!is.null(result$result)) {
        all_results <- append(all_results, list(result$result))
      } else {
        failed_files <- c(failed_files, file_path)
        if (!is.null(result$error)) {
          warning("Error with file ", basename(file_path), ": ", result$error$message)
        }
      }
      
      # หน่วงเวลา (ยกเว้นไฟล์สุดท้ายของ batch สุดท้าย)
      if (!(batch_num == n_batches && i == length(batch_files)) && delay_seconds > 0) {
        Sys.sleep(delay_seconds)
      }
    }
    
    # บันทึกผลลัพธ์ระหว่างทาง
    if (save_intermediate && length(all_results) > 0) {
      temp_results <- bind_rows(all_results)
      write_csv(temp_results, paste0("ocr_batch_", batch_num, "_", Sys.Date(), ".csv"))
      cat("บันทึกผลลัพธ์ batch", batch_num, "แล้ว\n")
    }
    
    cat("Batch", batch_num, "เสร็จแล้ว\n")
  }
  
  if (progress) {
    close(pb)
  }
  
  # รวมผลลัพธ์
  if (length(all_results) > 0) {
    final_results <- bind_rows(all_results)
    
    # สรุปผล
    cat("\n=== สรุปผลการประมวลผล ===\n")
    cat("ไฟล์ทั้งหมด:", length(existing_files), "\n")
    cat("ประมวลผลสำเร็จ:", nrow(final_results), "\n")
    cat("ล้มเหลว:", length(failed_files), "\n")
    cat("รวม tokens ใช้:", sum(final_results$tokens_used, na.rm = TRUE), "\n")
    
    if (length(failed_files) > 0) {
      cat("ไฟล์ที่ล้มเหลว:\n")
      cat(paste("-", basename(failed_files), collapse = "\n"), "\n")
    }
    
    return(final_results)
  } else {
    stop("ไม่มีไฟล์ที่ประมวลผลสำเร็จ")
  }
}



#' OCR Handwriting from Image using OpenAI Vision API
#'
#' @title แปลงลายมือจากภาพเป็นข้อความ
#' @param image_path เส้นทางไฟล์ภาพ
#' @param api_key OpenAI API key (ถ้าไม่ระบุจะใช้จาก environment)
#' @param system_prompt system prompt สำหรับการ OCR
#' @param model_config ชื่อโมเดลที่ใช้ (default: "gpt-4o-mini")
#' @param max_tokens จำนวน token สูงสุด (default: 500)
#' @return tibble ที่มี answer, prompt_tokens, completion_tokens, tokens_used
#' @export
ocr_handwriting <- function(image_path, 
                            api_key = NULL, 
                            system_prompt = NULL, 
                            model_config = NULL,
                            max_tokens = 500) {
  
  library(httr2)
  library(jsonlite)
  
  
  # ตรวจสอบไฟล์มีอยู่จริง
  if (!file.exists(image_path)) {
    stop("ไม่พบไฟล์ภาพ: ", image_path)
  }
  
  # ตั้งค่า API key
  if (is.null(api_key)) {
    api_key <- Sys.getenv("OPENAI_API_KEY")
    if (api_key == "" || is.null(api_key)) {
      stop("กรุณาตั้งค่า OPENAI_API_KEY ใน environment variables")
    }
  }
  
  
  # ตั้งค่า system prompt - แก้ไขจาก ifelse เป็น if
  if (is.null(system_prompt)) {
    system_prompt <- "คุณกำลังช่วยสกัดคำตอบจากแบบเขียนของนักเรียนที่ถูกแปลงเป็นข้อความจาก OCR
ในเนื้อหานี้จะมีคำตอบของนักเรียนที่เขียนด้วยลายมืออยู่ในกรอบสี่เหลี่ยม กรุณา
สกัดคำตอบทั้งหมดที่อยู่ภายในกรอบ โดยพยายามคงความหมายดั้งเดิมให้มากที่สุด และสกัดคำให้อยู่ในบริบทเดียวกัน
สอดคล้องกัน ออกมาให้ครบถ้วน **ไม่เอาบรรทัดที่เขียนว่า พื้นที่สำหรับตอบคำถาม ...**"
  }
  
  # ตั้งค่า model config
  if (is.null(model_config)) {
    model_config <- "gpt-4.1-mini"  
  }
  
  # ตรวจสอบนามสกุลไฟล์
  file_ext <- tools::file_ext(tolower(image_path))
  if (!file_ext %in% c("png", "jpg", "jpeg", "gif", "webp")) {
    warning("ไฟล์อาจไม่ใช่รูปภาพที่รองรับ: ", file_ext)
  }
  
  # อ่านไฟล์ภาพและแปลงเป็น base64
  tryCatch({
    img <- base64enc::base64encode(image_path)
  }, error = function(e) {
    stop("ไม่สามารถอ่านไฟล์ภาพได้: ", e$message)
  })
  
  # เตรียม MIME type
  mime_type <- switch(file_ext,
                      "png" = "image/png",
                      "jpg" = "image/jpeg", 
                      "jpeg" = "image/jpeg",
                      "gif" = "image/gif",
                      "webp" = "image/webp",
                      "image/png"  # default
  )
  
  # ส่งคำขอไปยัง OpenAI API
  tryCatch({
    resp <- request("https://api.openai.com/v1/chat/completions") |>
      req_headers(
        "Authorization" = paste("Bearer", api_key),
        "Content-Type" = "application/json"
      ) |>
      req_body_json(list(
        model = model_config,
        max_tokens = max_tokens,
        presence_penalty = 0,
        frequency_penalty = 0,
        temperature = 0.2,
        messages = list(
          list(role = "system", content = system_prompt),
          list(role = "user", content = list(
            list(type = "text", text = "ถอดข้อความจากภาพนี้ให้เป็นตัวอักษรไทย ไม่เอาส่วนที่เขียนว่า 'พื้นที่สำหรับตอบคำถาม ...' "),
            list(
              type = "image_url",
              image_url = list(url = paste0("data:", mime_type, ";base64,", img))
            )
          ))
        )
      )) |>
      req_perform()
    
  }, error = function(e) {
    stop("การเรียก API ล้มเหลว: ", e$message)
  })
  
  
  # ตรวจสอบ response status
  if (resp_status(resp) != 200) {
    stop("API response error: ", resp_status(resp), " - ", resp_status_desc(resp))
  }
  
  # แปลงผลลัพธ์เป็น JSON
  tryCatch({
    res_json <- resp_body_json(resp)
  }, error = function(e) {
    stop("ไม่สามารถแปลง response เป็น JSON ได้: ", e$message)
  })
  
  # ตรวจสอบ response structure
  if (is.null(res_json$choices) || length(res_json$choices) == 0) {
    stop("ไม่ได้รับคำตอบจาก API")
  }
  
  # เพิ่ม null coalescing operator ถ้าไม่มี
  `%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0 || (is.character(x) && x == "")) y else x
  }
  
  # แสดงผลลัพธ์
  output <- tibble::tibble(
    image_file = basename(image_path),
    answer = res_json$choices[[1]]$message$content %||% "",
    prompt_tokens = res_json$usage$prompt_tokens %||% 0,
    completion_tokens = res_json$usage$completion_tokens %||% 0,
    tokens_used = res_json$usage$total_tokens %||% 0,
    model_used = model_config,
    processed_at = Sys.time()
  )
  
  return(output)
}


### ---

#' Batch OCR Processing with Configurable System Prompt
#'
#' @title ประมวลผล OCR หลายไฟล์พร้อมกัน
#' @param image_paths vector ของ path ไฟล์รูป  
#' @param batch_size จำนวนไฟล์ต่อ batch (default: 10)
#' @param system_prompt system prompt สำหรับการ OCR (default: NULL ใช้ default ของ ocr_handwriting)
#' @param delay_seconds หน่วงเวลาระหว่างการเรียก API (default: 2)
#' @param progress แสดง progress (default: TRUE)
#' @param save_intermediate บันทึกผลลัพธ์ระหว่างทาง (default: FALSE)
#' @param ... arguments อื่นๆ ส่งต่อไปยัง ocr_handwriting()
#' @return data frame รวมผลลัพธ์ทั้งหมด
#' @export
ocr_batch_handwriting <- function(image_paths,
                                  batch_size = 10,
                                  system_prompt = NULL,
                                  delay_seconds = 2,
                                  progress = TRUE,
                                  save_intermediate = FALSE,
                                  ...) {
  
  library(purrr)
  library(dplyr)
  library(readr)
  
  # ตรวจสอบไฟล์ที่มีอยู่จริง
  existing_files <- image_paths[file.exists(image_paths)]
  missing_files <- image_paths[!file.exists(image_paths)]
  
  if (length(missing_files) > 0) {
    warning("ไฟล์ที่ไม่พบ: ", paste(basename(missing_files), collapse = ", "))
  }
  
  if (length(existing_files) == 0) {
    stop("ไม่มีไฟล์ที่สามารถประมวลผลได้")
  }
  
  # แบ่งเป็น batches
  n_batches <- ceiling(length(existing_files) / batch_size)
  
  cat("กำลังประมวลผล", length(existing_files), "ไฟล์\n")
  cat("แบ่งเป็น", n_batches, "batch (", batch_size, "ไฟล์/batch)\n")
  
  # แสดง system prompt ที่ใช้
  if (!is.null(system_prompt)) {
    cat("ใช้ custom system prompt:", substr(system_prompt, 1, 50), "...\n")
  } else {
    cat("ใช้ default system prompt\n")
  }
  
  # สร้าง safe version
  safe_ocr <- safely(ocr_handwriting, otherwise = NULL)
  
  # เตรียม progress bar
  if (progress) {
    pb <- txtProgressBar(min = 0, max = length(existing_files), style = 3)
    processed_count <- 0
  }
  
  all_results <- list()
  failed_files <- character(0)
  
  # ประมวลผลแต่ละ batch
  for (batch_num in 1:n_batches) {
    start_idx <- (batch_num - 1) * batch_size + 1
    end_idx <- min(batch_num * batch_size, length(existing_files))
    batch_files <- existing_files[start_idx:end_idx]
    
    cat("\nBatch", batch_num, "/", n_batches, ":", length(batch_files), "ไฟล์\n")
    
    # ประมวลผลไฟล์ใน batch นี้
    for (i in seq_along(batch_files)) {
      file_path <- batch_files[i]
      
      if (progress) {
        processed_count <- processed_count + 1
        setTxtProgressBar(pb, processed_count)
      }
      
      # เรียก OCR พร้อม system_prompt
      result <- safe_ocr(
        image_path = file_path, 
        system_prompt = system_prompt,  # ส่ง system_prompt ไปด้วย
        ...
      )
      
      if (!is.null(result$result)) {
        all_results <- append(all_results, list(result$result))
      } else {
        failed_files <- c(failed_files, file_path)
        if (!is.null(result$error)) {
          warning("Error with file ", basename(file_path), ": ", result$error$message)
        }
      }
      
      # หน่วงเวลา (ยกเว้นไฟล์สุดท้ายของ batch สุดท้าย)
      if (!(batch_num == n_batches && i == length(batch_files)) && delay_seconds > 0) {
        Sys.sleep(delay_seconds)
      }
    }
    
    # บันทึกผลลัพธ์ระหว่างทาง
    if (save_intermediate && length(all_results) > 0) {
      temp_results <- bind_rows(all_results)
      write_csv(temp_results, paste0("ocr_batch_", batch_num, "_", Sys.Date(), ".csv"))
      cat("บันทึกผลลัพธ์ batch", batch_num, "แล้ว\n")
    }
    
    cat("Batch", batch_num, "เสร็จแล้ว\n")
  }
  
  if (progress) {
    close(pb)
  }
  
  # รวมผลลัพธ์
  if (length(all_results) > 0) {
    final_results <- bind_rows(all_results)
    
    # สรุปผล
    cat("\n=== สรุปผลการประมวลผล ===\n")
    cat("ไฟล์ทั้งหมด:", length(existing_files), "\n")
    cat("ประมวลผลสำเร็จ:", nrow(final_results), "\n")
    cat("ล้มเหลว:", length(failed_files), "\n")
    cat("รวม tokens ใช้:", sum(final_results$tokens_used, na.rm = TRUE), "\n")
    
    if (length(failed_files) > 0) {
      cat("ไฟล์ที่ล้มเหลว:\n")
      cat(paste("-", basename(failed_files), collapse = "\n"), "\n")
    }
    
    return(final_results)
  } else {
    stop("ไม่มีไฟล์ที่ประมวลผลสำเร็จ")
  }
}



#' OCR with Conversation Context for Handwriting Images
#'
#' @title ประมวลผล OCR แบบ conversation สำหรับการเก็บ context
#' @description 
#' ใช้ conversation API ของ OpenAI เพื่อให้โมเดลเรียนรู้จากรูปก่อนหน้า
#' และสามารถแปลลายมือได้ดีขึ้นจากบริบทที่มีอยู่
#' 
#' @param image_paths vector ของ path ไฟล์รูป
#' @param batch_size จำนวนไฟล์ต่อ conversation batch (default: 10)
#' @param delay_seconds หน่วงเวลาระหว่าง batch (default: 2)
#' @param api_key OpenAI API key (ถ้าไม่ระบุจะใช้จาก environment)
#' @param model_config ชื่อโมเดลที่ใช้ (default: "gpt-4o-mini")
#' @param progress แสดง progress (default: TRUE)
#' @return data frame รวมผลลัพธ์ทั้งหมด
#' @export
ocr_batch_conversation <- function(image_paths,
                                   batch_size = 10,
                                   delay_seconds = 2,
                                   ...) {
  
  # แบ่งเป็น batch
  file_batches <- split(image_paths, ceiling(seq_along(image_paths) / batch_size))
  
  all_results <- list()
  
  for (batch_num in seq_along(file_batches)) {
    cat("Batch", batch_num, "/", length(file_batches), "\n")
    
    # เริ่ม conversation ใหม่แต่ละ batch
    conversation_results <- ocr_conversation_batch(
      image_paths = file_batches[[batch_num]],
      ...
    )
    
    all_results <- append(all_results, list(conversation_results))
    
    if (batch_num < length(file_batches)) {
      Sys.sleep(delay_seconds)
    }
  }
  
  return(bind_rows(all_results))
}

ocr_conversation_batch <- function(image_paths, 
                                   api_key = NULL,
                                   model_config = "gpt-4.1-mini") {
  
  if (is.null(api_key)) {
    api_key <- Sys.getenv("OPENAI_API_KEY")
  }
  
  # เริ่มต้น conversation
  messages <- list(
    list(role = "system", content ="
         คุณกำลังช่วยสกัดคำตอบจากแบบเขียนของนักเรียนที่ถูกแปลงเป็นข้อความจาก OCR
         ในเนื้อหานี้จะมีคำตอบของนักเรียนที่เขียนด้วยลายมืออยู่ในกรอบสี่เหลี่่ยม กรุณา
         สกัดคำตอบทั้งหมดที่อยู่ภายในกรอบ โดยพยายามคงความหมายดั้งเดิมให้มากที่สุด และสกัดคำให้อยู่ในบริบทเดียวกัน
         สอดคล้องกัน ออกมาให้ครบถ้วน ถ้ามีคำที่อ่านไม่ออกหรือผิดปกติให้คาดเดาให้สอดคล้องกับบริบทได้ โดนไม่่เปลี่ยนสำนวนหรือความหมายในภาพรวม **ไม่เอาบรรทัดที่เขียนว่า พื้นที่สำหรับตอบคำถาม ...**
        "
         )
  )
  
  results <- list()
  
  for (i in seq_along(image_paths)) {
    img_path <- image_paths[i]
    
    # เพิ่มรูปใน conversation
    file_ext <- tools::file_ext(tolower(img_path))
    mime_type <- ifelse(file_ext == "png", "image/png", "image/jpeg")
    img_b64 <- base64enc::base64encode(img_path)
    
    # เพิ่ม user message
    user_content <- list(
      list(type = "text", text = paste("รูปที่", i, ":")),
      list(type = "image_url", 
           image_url = list(url = paste0("data:", mime_type, ";base64,", img_b64)))
    )
    
    messages <- append(messages, list(list(role = "user", content = user_content)))
    
    # ส่ง request
    resp <- request("https://api.openai.com/v1/chat/completions") |>
      req_headers(
        "Authorization" = paste("Bearer", api_key),
        "Content-Type" = "application/json"
      ) |>
      req_body_json(list(
        model = model_config,
        max_tokens = 300,
        temperature = 0.1,
        messages = messages
      )) |>
      req_perform()
    
    res_json <- resp_body_json(resp)
    answer <- res_json$choices[[1]]$message$content
    
    # เก็บผลลัพธ์
    results[[i]] <- tibble::tibble(
      image_file = basename(img_path),
      answer = answer,
      prompt_tokens = res_json$usage$prompt_tokens,
      completion_tokens = res_json$usage$completion_tokens,
      tokens_used = res_json$usage$total_tokens,
      conversation_position = i
    )
    
    # เพิ่ม assistant response ใน conversation
    messages <- append(messages, list(list(role = "assistant", content = answer)))
  }
  
  return(bind_rows(results))
}




