#' Read Markdown Frontmatter
#'
#' @param path Path to markdown file with YAML frontmatter
#' @return Parsed YAML data
#' @keywords internal
read_md_frontmatter <- function(path) {
  txt <- readLines(path, warn = FALSE)
  i <- which(txt == "---")
  stopifnot(length(i) >= 2)
  yaml::yaml.load(paste(txt[(i[1]+1):(i[2]-1)], collapse = "\n"))
}

#' Load Rubric from Markdown File
#'
#' Load grading rubric from a markdown file with YAML frontmatter
#'
#' @param path Path to rubric markdown file
#' @return List containing item_id, max_score, criteria, and question
#' @export
#' @examples
#' \dontrun{
#' rubric <- load_rubric("rubric/item_001.md")
#' }
load_rubric <- function(path) {
  fm <- read_md_frontmatter(path)
  stopifnot(!is.null(fm$item_id), !is.null(fm$criteria), !is.null(fm$max_score))
  list(
    item_id = fm$item_id, 
    max_score = fm$max_score, 
    criteria = fm$criteria, 
    question = fm$question %||% NULL
  )
}

#' Load Answer Key from Markdown File
#'
#' Load answer key with core points and exemplars from markdown file
#'
#' @param path Path to answer key markdown file
#' @return List containing item_id, core_points, exemplar, and common_misconceptions
#' @export  
#' @examples
#' \dontrun{
#' key <- load_answer_key("rubric/item_001_key.md")
#' }
load_answer_key <- function(path) {
  fm <- read_md_frontmatter(path)
  list(
    item_id = fm$item_id,
    core_points = fm$core_points %||% list(),
    exemplar = fm$exemplar %||% NULL,
    common_misconceptions = fm$common_misconceptions %||% list()
  )
}