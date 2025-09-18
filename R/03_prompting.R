#' Read File Contents
#'
#' @param p File path
#' @return File contents as single string
#' @keywords internal
read_file_all <- function(p) paste(readLines(p, warn = FALSE), collapse = "\n")

#' Render Grading Prompt
#'
#' Create a grading prompt by combining template, rubric, and student response
#'
#' @param template_path Path to prompt template file
#' @param rubric Rubric object from load_rubric()
#' @param key Answer key object from load_answer_key() (optional)
#' @param response_row Single row of student response data
#' @return Rendered prompt string
#' @export
#' @examples
#' \dontrun{
#' rubric <- load_rubric("rubric/item_001.md")
#' key <- load_answer_key("rubric/item_001_key.md")
#' prompt <- render_prompt("templates/grade_template.md", rubric, key, response_row)
#' }
render_prompt <- function(template_path, rubric, key = NULL, response_row) {
  tpl <- read_file_all(template_path)
  rubric_yaml <- yaml::as.yaml(list(
    item_id = rubric$item_id,
    max_score = rubric$max_score,
    criteria = rubric$criteria
  ))
  
  key_yaml <- if (!is.null(key)) yaml::as.yaml(key) else ""
  question <- rubric$question %||% response_row$question_text %||% ""

  glue::glue(tpl,
    .open = "{{", .close = "}}",
    rubric_yaml   = rubric_yaml,
    key_yaml      = key_yaml,
    response_text = response_row$response_text,
    question_text = question
  )
}