source("R/00_utils.R")
source("R/01_ingest.R")
source("R/02_load_rubric.R")
source("R/03_prompting.R")
source("R/04_scoring.R")
source("R/05_aggregate.R")
source("R/06_export.R")

cfg_model <- yaml::read_yaml("config/model.yml")
cfg_data  <- yaml::read_yaml("config/data.yml")


init_logger(cfg_data$logging$file, cfg_data$logging$level)

dir.create(cfg_data$paths$interim_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(cfg_data$paths$scores_dir,  recursive = TRUE, showWarnings = FALSE)
dir.create(cfg_data$paths$reports_dir, recursive = TRUE, showWarnings = FALSE)
dir.create("data/output/manifests", recursive = TRUE, showWarnings = FALSE)


# 1) ## importing data
responses <- read_csv("data/raw/responses.csv", show_col_types = FALSE)
log_info("Ingested {nrow(responses)} responses")


# 2) ## loading rubric
rubric <- load_rubric_md("rubric/item_006_rubric.md")
key <- load_key_md("rubric/item_006_key.md")

# 3) ## prompting
template_file <- "prompts/grade_template.md"

prompt <- render_prompt(template_file, rubric, key, response_row = responses[12,])

library(tidyllm)

schema <- tidyllm_schema(
    name = "grading_schema",
    student_id = field_chr("student_id"),
    item_id   = field_chr("item_id"),
    total_score = field_dbl("total_score"),
    per_criterion = field_object(
      id = field_chr("id"),
      score = field_dbl("score"),
      justification = field_chr("justification หากคะแนนไม่ได้เต็มให้ระบุเหตุผล โดยยกตัวอย่างจากคำตอบของนักเรียน มาอ้างอิงให้ชัดเจน")),
    overall_feedback = field_chr("ระบุจุดแข็ง จุดที่ควรปรับปรุง และคำแนะนำเพิ่มเติม")
)


test <- tibble(
    role = c("system", "user"),
    content = c("คุณเป็นอาจารย์ผู้เชี่ยวชาญด้าน data-driven classroom มีหน้าที่ประเมินผลการตอบของนักเรียนอย่างตามเกณฑ์ที่กำหนดอย่างตรงไปตรงมาและเป็นธรรม คงเส้นคงวาในการให้คะแนน",
                prompt)
) %>%
df_llm_message() %>%
chat(openai(),
  .model = "gpt-5-nano",
  .json_schema = schema
) 

test %>% get_reply_data()


