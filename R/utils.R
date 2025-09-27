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