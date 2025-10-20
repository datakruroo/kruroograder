# ==============================================================================
# Example: Multiagent Grading System
# ==============================================================================
# This script demonstrates how to use the multiagent grading system in
# kruroograder. The system uses 4 AI agents:
#   - Agent 1 & 2: Two independent graders with different perspectives
#   - Agent 3: Consistency checker
#   - Agent 4: Feedback synthesizer
# ==============================================================================

library(kruroograder)
library(readr)
library(dplyr)
library(logger)

# ------------------------------------------------------------------------------
# Setup
# ------------------------------------------------------------------------------

# Initialize logging
log_threshold(INFO)

# Load your data
responses <- read_csv("inst/extdata/responses.csv")
rubric <- load_rubric("inst/rubric/item_006_rubric.md")
key <- load_answer_key("inst/rubric/item_006_key.md")

# View the data
cat("\n=== Input Data ===\n")
cat("Number of responses:", nrow(responses), "\n")
print(responses)

# ------------------------------------------------------------------------------
# Example 1: Basic Multiagent Grading
# ------------------------------------------------------------------------------
cat("\n=== Example 1: Basic Multiagent Grading ===\n")

# Use default settings (2 iterations max, 0.2 threshold)
results_basic <- grade_responses_multiagent(
  responses = responses,
  rubric = rubric,
  key = key,
  .progress = TRUE
)

# View results
print(results_basic)

# View multiagent-specific columns
cat("\n--- Multiagent Metadata ---\n")
results_basic %>%
  select(student_id, total_score, agent1_score, agent2_score,
         score_difference, was_consistent, iterations) %>%
  print()

# ------------------------------------------------------------------------------
# Example 2: Custom Agent Prompts
# ------------------------------------------------------------------------------
cat("\n=== Example 2: Custom Agent Prompts ===\n")

# Define custom system prompts for different grading perspectives
agent1_prompt <- "
คุณเป็นอาจารย์ที่เน้นความคิดสร้างสรรค์และความเข้าใจเชิงลึก
ให้คะแนนโดยพิจารณา:
1. ความเข้าใจแนวคิดหลัก
2. การคิดวิเคราะห์และเชื่อมโยง
3. ความคิดสร้างสรรค์ในการอธิบาย
"

agent2_prompt <- "
คุณเป็นอาจารย์ที่เน้นความถูกต้องและครบถ้วน
ให้คะแนนโดยพิจารณา:
1. ความถูกต้องของข้อมูล
2. ความสมบูรณ์ของคำตอบ
3. การใช้ศัพท์เทคนิคที่เหมาะสม
"

results_custom <- grade_responses_multiagent(
  responses = responses,
  rubric = rubric,
  key = key,
  system_prompt_agent1 = agent1_prompt,
  system_prompt_agent2 = agent2_prompt,
  max_iterations = 3,  # Allow up to 3 re-grading attempts
  consistency_threshold = 0.15,  # Stricter consistency requirement
  .progress = TRUE
)

print(results_custom)

# ------------------------------------------------------------------------------
# Example 3: Parallel Processing for Large Datasets
# ------------------------------------------------------------------------------
cat("\n=== Example 3: Parallel Processing ===\n")

# For large datasets, use parallel processing
# Note: This example assumes you have more responses
# Uncomment and modify for your actual large dataset:

# results_parallel <- grade_responses_multiagent(
#   responses = large_responses,  # Your large dataset
#   rubric = rubric,
#   key = key,
#   .parallel = TRUE,
#   .cores = 4,  # Use 4 CPU cores
#   .progress = TRUE
# )

# ------------------------------------------------------------------------------
# Example 4: Analyzing Multiagent Results
# ------------------------------------------------------------------------------
cat("\n=== Example 4: Analyzing Results ===\n")

# Consistency analysis
cat("\nConsistency Summary:\n")
cat("Consistent grading:", sum(results_basic$was_consistent), "out of",
    nrow(results_basic), "\n")
cat("Average score difference:",
    round(mean(results_basic$score_difference), 2), "\n")
cat("Max score difference:",
    max(results_basic$score_difference), "\n")

# Distribution of iterations
cat("\nIteration Distribution:\n")
print(table(results_basic$iterations))

# Cases where graders disagreed significantly
cat("\n--- Cases with High Disagreement ---\n")
high_disagreement <- results_basic %>%
  filter(score_difference > 2) %>%
  select(student_id, item_id, agent1_score, agent2_score,
         score_difference, grader_agreement_note)

if (nrow(high_disagreement) > 0) {
  print(high_disagreement)
} else {
  cat("No cases with score difference > 2\n")
}

# View detailed feedback for a specific student
cat("\n--- Detailed View for First Student ---\n")
view_student_results(results_basic, student_id = results_basic$student_id[1])

# Compare agent scores
cat("\n--- Agent Score Comparison ---\n")
results_basic %>%
  select(student_id, agent1_score, agent2_score, total_score) %>%
  mutate(
    avg_agent_score = (agent1_score + agent2_score) / 2,
    synthesis_adjustment = total_score - avg_agent_score
  ) %>%
  print()

# ------------------------------------------------------------------------------
# Example 5: Export Multiagent Results
# ------------------------------------------------------------------------------
cat("\n=== Example 5: Export Results ===\n")

# Export to CSV with all multiagent metadata
export_results(
  results_basic,
  output_path = "output/multiagent_results.csv",
  format = "csv"
)

# Export to Excel for easier review
export_results(
  results_basic,
  output_path = "output/multiagent_results.xlsx",
  format = "excel"
)

# Create summary report
create_grading_report(
  results_basic,
  output_path = "output/multiagent_report.md"
)

cat("\nResults exported successfully!\n")

# ------------------------------------------------------------------------------
# Example 6: Comparing Single-Agent vs Multiagent
# ------------------------------------------------------------------------------
cat("\n=== Example 6: Single-Agent vs Multiagent Comparison ===\n")

# Grade with single agent
results_single <- grade_responses(
  responses = responses,
  rubric = rubric,
  key = key,
  .progress = TRUE
)

# Grade with multiagent
results_multi <- grade_responses_multiagent(
  responses = responses,
  rubric = rubric,
  key = key,
  .progress = TRUE
)

# Compare scores
comparison <- data.frame(
  student_id = results_single$student_id,
  single_agent_score = results_single$total_score,
  multiagent_score = results_multi$total_score,
  agent1_score = results_multi$agent1_score,
  agent2_score = results_multi$agent2_score,
  score_diff = results_multi$total_score - results_single$total_score
)

cat("\n--- Score Comparison ---\n")
print(comparison)

cat("\nAverage difference (multiagent - single):",
    round(mean(comparison$score_diff), 2), "\n")

# ------------------------------------------------------------------------------
# Tips for Using Multiagent Grading
# ------------------------------------------------------------------------------
cat("\n=== Tips for Using Multiagent Grading ===\n")
cat("
1. Use multiagent grading for high-stakes assessments where reliability is critical

2. Customize agent prompts to represent different evaluation perspectives:
   - Conceptual understanding vs technical accuracy
   - Creative thinking vs systematic approach
   - Theoretical knowledge vs practical application

3. Adjust consistency_threshold based on your rubric:
   - Lower threshold (0.1-0.15): Stricter consistency requirement
   - Medium threshold (0.2): Default, balanced approach
   - Higher threshold (0.3-0.4): More lenient, faster grading

4. Set max_iterations based on response complexity:
   - Simple questions: 1-2 iterations
   - Complex essays: 2-3 iterations
   - Very complex assessments: 3-4 iterations

5. Use parallel processing (.parallel = TRUE) for:
   - More than 10 responses
   - Complex rubrics with many criteria
   - High iteration limits

6. Review cases with high disagreement manually:
   - Check score_difference column
   - Read grader_agreement_note for insights
   - Consider these for human review

7. Monitor iterations column:
   - Many iterations might indicate unclear rubric
   - Consistent low iterations suggest good rubric design

8. Export results with all metadata for analysis:
   - Track agent scores over time
   - Identify patterns in disagreement
   - Refine rubrics based on consistency data
")

cat("\n=== Multiagent Grading Complete! ===\n")
