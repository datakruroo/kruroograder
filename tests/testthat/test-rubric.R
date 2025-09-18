test_that("load_rubric works correctly", {
  # Create temporary rubric file
  temp_file <- tempfile(fileext = ".md")
  rubric_content <- "---
item_id: 'test_item'
max_score: 10
question: 'Test question'
criteria:
  - id: C1
    name: 'Test criterion'
    points: 5
---
Test rubric content"
  
  writeLines(rubric_content, temp_file)
  
  # Test loading
  rubric <- load_rubric(temp_file)
  
  expect_equal(rubric$item_id, "test_item")
  expect_equal(rubric$max_score, 10)
  expect_equal(rubric$question, "Test question")
  expect_length(rubric$criteria, 1)
  
  # Clean up
  unlink(temp_file)
})

test_that("load_answer_key works correctly", {
  # Create temporary key file
  temp_file <- tempfile(fileext = ".md")
  key_content <- "---
item_id: 'test_item'
core_points:
  - 'Point 1'
  - 'Point 2'
exemplar: 'Example answer'
---
Test key content"
  
  writeLines(key_content, temp_file)
  
  # Test loading
  key <- load_answer_key(temp_file)
  
  expect_equal(key$item_id, "test_item")
  expect_length(key$core_points, 2)
  expect_equal(key$exemplar, "Example answer")
  
  # Clean up
  unlink(temp_file)
})