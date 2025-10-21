
![S__75743284_0 copy](https://github.com/user-attachments/assets/e00d7606-b3f4-48fe-9943-fd2fe2050dcb)

# kruroograder 🤖📝
ระบบตรวจข้อสอบอัตนัยอัตโนมัติที่ใช้ LLM (Large Language Model) สำหรับประเมินคำตอบของนักเรียนอย่างเป็นระบบและโปร่งใส

[![R](https://img.shields.io/badge/R-%3E%3D4.0-blue.svg)](https://www.r-project.org/)
[![License](https://img.shields.io/badge/License-MIT-green.svg)](LICENSE)

## 🎯 ภาพรวมโครงการ

**kruroograder** เป็น R package ที่พัฒนาขึ้นเพื่อแก้ปัญหาการตรวจข้อสอบอัตนัยที่ใช้เวลานานและอาจมีความไม่สม่ำเสมอในการให้คะแนน โดยใช้พลังของ AI เพื่อ:

### 🌟 คุณสมบัติหลัก
- **🔍 โปร่งใส**: ทุกการตัดสินใจมี justification ที่ชัดเจน
- **🔄 Reproducible**: ผลลัพธ์สามารถทำซ้ำได้เหมือนเดิม  
- **⚡ Performance**: รองรับ parallel processing สำหรับข้อสอบจำนวนมาก
- **📊 Progress Tracking**: แสดงความคืบหน้าแบบ real-time พร้อม percentage
- **🎓 เชิงคุณภาพ**: ให้ feedback ที่มีประโยชน์ต่อการเรียนรู้
- **🔧 ยืดหยุ่น**: ปรับแต่งเกณฑ์การให้คะแนนได้ตามต้องการ

### 🏗️ สถาปัตยกรรมระบบ
```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   Student       │    │   Rubric &      │    │   LLM Engine    │
│   Responses     │───▶│   Answer Key    │───▶│   (GPT-5-nano)  │
│   (.csv)        │    │   (.md files)   │    │   + tidyllm     │
└─────────────────┘    └─────────────────┘    └─────────────────┘
                                                        │
┌─────────────────┐    ┌─────────────────┐              │
│   Results &     │◀───│   Grading       │◀─────────────┘
│   Reports       │    │   Engine        │
│   (CSV/Excel)   │    │   (Parallel)    │
└─────────────────┘    └─────────────────┘
```

### 🎯 กลุ่มเป้าหมาย
- **อาจารย์/ครู**: ที่ต้องการลดเวลาการตรวจข้อสอบอัตนัย
- **นักวิชาการ**: ที่ต้องการระบบประเมินที่มีมาตรฐานและตรวจสอบได้
- **สถาบันการศึกษา**: ที่มีนักเรียนจำนวนมากและต้องการประสิทธิภาพในการประเมิน

## 📦 การติดตั้ง

### ความต้องการของระบบ
- **R**: เวอร์ชั่น 4.0 ขึ้นไป
- **API Key**: OpenAI หรือ provider อื่นๆ ที่ tidyllm รองรับ

### วิธีการติดตั้ง

```r
# ติดตั้ง dependencies ก่อน
install.packages(c("devtools", "tidyllm", "yaml", "tibble", "dplyr", 
                   "readr", "glue", "logger", "parallel", "pbapply"))

# ติดตั้งจาก local development
devtools::install(".")

# หรือติดตั้งจาก GitHub (เมื่อพร้อม)
# devtools::install_github("username/kruroograder")
```

### ตั้งค่า API
```r
# สำหรับ OpenAI
Sys.setenv(OPENAI_API_KEY = "your-api-key-here")

# หรือใส่ใน .Renviron
# OPENAI_API_KEY=your-api-key-here
```

## 🚀 การใช้งานอย่างละเอียด

### ขั้นตอนที่ 1: โหลดแพ็กเกจและเตรียมข้อมูล

```r
library(kruroograder)
library(tidyllm)
library(readr)
library(dplyr)

# เปิดใช้งาน logging
init_logger(log_file = "logs/grading.log", log_level = "INFO")
```

### ขั้นตอนที่ 2: เตรียมไฟล์ข้อมูล

#### 2.1 ไฟล์คำตอบนักเรียน (`data/responses.csv`)
```csv
student_id,item_id,response_text,question_text
001,item_006,"การใช้ข้อมูลในการตัดสินใจคือการนำข้อมูลที่มีคุณภาพมาวิเคราะห์...","อธิบายความสำคัญของการใช้ข้อมูลในการตัดสินใจ"
002,item_006,"ข้อมูลเป็นสิ่งสำคัญในการตัดสินใจเพราะช่วยให้เราเข้าใจสถานการณ์...","อธิบายความสำคัญของการใช้ข้อมูลในการตัดสินใจ"
```

#### 2.2 ไฟล์ Rubric (`rubric/item_006_rubric.md`)
```yaml
---
item_id: "item_006"
max_score: 9
question: |
  อธิบายความสำคัญของการใช้ข้อมูลในการตัดสินใจ พร้อมยกตัวอย่างประกอบ
criteria:
  - id: C1
    name: "ความเข้าใจแนวคิด"
    points: 3
    descriptors:
      full: "อธิบายความสำคัญของข้อมูลได้ครบถ้วนและแม่นยำ (3 คะแนน)"
      partial: "อธิบายได้บางส่วนหรือไม่ชัดเจน (1-2 คะแนน)"
      none: "ไม่สามารถอธิบายได้หรืออธิบายผิด (0 คะแนน)"
  - id: C2
    name: "การให้ตัวอย่าง"
    points: 3
    descriptors:
      full: "ยกตัวอย่างที่เหมาะสมและสอดคล้องกับเนื้อหา (3 คะแนน)"
      partial: "ยกตัวอย่างได้บางส่วนหรือไม่ชัดเจน (1-2 คะแนน)"
      none: "ไม่มีตัวอย่างหรือตัวอย่างไม่เหมาะสม (0 คะแนน)"
  - id: C3
    name: "ความสมบูรณ์ของคำตอบ"
    points: 3
    descriptors:
      full: "คำตอบครบถ้วน มีการเชื่อมโยงและสรุป (3 คะแนน)"
      partial: "คำตอบค่อนข้างสมบูรณ์แต่ขาดบางส่วน (1-2 คะแนน)"
      none: "คำตอบไม่สมบูรณ์หรือขาดหลายส่วน (0 คะแนน)"
---
```

#### 2.3 ไฟล์ Answer Key (`rubric/item_006_key.md`)
```yaml
---
item_id: "item_006"
core_points:
  - "ข้อมูลช่วยลดความไม่แน่นอนในการตัดสินใจ"
  - "ข้อมูลที่ถูกต้องและทันสมัยช่วยให้ตัดสินใจได้อย่างมีประสิทธิภาพ"
  - "การวิเคราะห์ข้อมูลช่วยให้เห็นแนวโน้มและรูปแบบ"
exemplar: |
  การใช้ข้อมูลในการตัดสินใจมีความสำคัญอย่างยิ่งในยุคปัจจุบัน เนื่องจากข้อมูลช่วยลดความไม่แน่นอน
  และเพิ่มความแม่นยำในการตัดสินใจ ตัวอย่างเช่น ในธุรกิจ การวิเคราะห์ข้อมูลยอดขายในอดีต
  ช่วยให้ผู้จัดการสามารถวางแผนการผลิตและการตลาดได้อย่างเหมาะสม
common_misconceptions:
  - "คิดว่าข้อมูลมากเท่านั้นก็เพียงพอ โดยไม่คำนึงถึงคุณภาพ"
  - "ใช้ข้อมูลเก่าหรือไม่เกี่ยวข้องในการตัดสินใจ"
---
```

### ขั้นตอนที่ 3: โหลดข้อมูลและเตรียมการตรวจ

```r
# โหลดข้อมูล
responses <- read_csv("data/responses.csv")
rubric <- load_rubric("rubric/item_006_rubric.md")
answer_key <- load_answer_key("rubric/item_006_key.md")

# ตรวจสอบข้อมูล
validate_response_data(responses)
cat("จำนวนนักเรียน:", nrow(responses), "คน\n")
cat("คะแนนเต็ม:", rubric$max_score, "คะแนน\n")
```

### ขั้นตอนที่ 4: การตรวจและให้คะแนน

#### 4.1 การตรวจแบบ Sequential (ทีละคน)
```r
# การตรวจแบบปกติ
results_seq <- grade_responses(
  responses = responses,
  rubric = rubric,
  key = answer_key,
  .progress = TRUE,           # แสดง progress bar
  .parallel = FALSE           # ไม่ใช้ parallel processing
)
```

#### 4.2 การตรวจแบบ Parallel (หลายคนพร้อมกัน)
```r
# ตรวจสอบว่าควรใช้ parallel หรือไม่
n_students <- nrow(responses)
if (should_use_parallel(n_students)) {
  recommended_cores <- get_optimal_cores(n_students)
  cat("แนะนำให้ใช้", recommended_cores, "cores สำหรับ", n_students, "นักเรียน\n")
  
  # การตรวจแบบ parallel
  results_par <- grade_responses(
    responses = responses,
    rubric = rubric, 
    key = answer_key,
    .progress = TRUE,         # แสดง progress bar
    .parallel = TRUE,         # ใช้ parallel processing
    .cores = recommended_cores # จำนวน cores
  )
} else {
  cat("จำนวนนักเรียนน้อย แนะนำให้ใช้ sequential processing\n")
  results_par <- results_seq
}
```

#### 4.3 การปรับแต่งโมเดล
```r
# กำหนดค่าโมเดลเอง
custom_model_config <- list(
  model = "gpt-4o-mini",      # เปลี่ยนโมเดล
  provider = "openai"         # ผู้ให้บริการ
)

results_custom <- grade_responses(
  responses = responses,
  rubric = rubric,
  key = answer_key,
  model_config = custom_model_config,
  .parallel = TRUE,
  .cores = 4
)
```

#### 4.4 การปรับแต่ง System Prompt
```r
# ตัวอย่าง 1: อาจารย์เข้มงวด
strict_prompt <- "คุณเป็นอาจารย์ที่เข้มงวดและใช้เกณฑ์สูง ให้คะแนนอย่างเข้มข้น ไม่ให้คะแนนเต็มง่ายๆ และชี้ข้อผิดพลาดอย่างชัดเจน"

results_strict <- grade_responses(
  responses = responses,
  rubric = rubric,
  key = answer_key,
  system_prompt = strict_prompt
)

# ตัวอย่าง 2: อาจารย์ให้กำลังใจ
encouraging_prompt <- "คุณเป็นอาจารย์ที่ให้กำลังใจและสร้างแรงบันดาลใจ ให้คะแนนอย่างเป็นธรรมแต่เน้นการให้คำแนะนำเชิงบวกและสร้างสรรค์"

results_encouraging <- grade_responses(
  responses = responses,
  rubric = rubric,
  key = answer_key,
  system_prompt = encouraging_prompt
)

# ตัวอย่าง 3: ภาษาอังกฤษ
english_prompt <- "You are an expert educator specializing in data-driven classroom assessment. Grade student responses fairly and consistently according to the provided rubric."

results_english <- grade_responses(
  responses = responses,
  rubric = rubric,
  key = answer_key,
  system_prompt = english_prompt
)
```

### ขั้นตอนที่ 5: การตรวจสอบผลลัพธ์

```r
# ดูผลลัพธ์โดยสรุป
head(results_par)
summary(results_par$total_score)

# ตรวจสอบการกระจายคะแนน
table(results_par$total_score)

# ดู feedback ของนักเรียนคนแรก
cat("คะแนน:", results_par$total_score[1], "/", rubric$max_score, "\n")
cat("Feedback:", results_par$overall_feedback[1], "\n")
```

### ขั้นตอนที่ 6: การส่งออกผลลัพธ์

```r
# ส่งออกเป็น CSV
export_results(results_par, "output/grades.csv", format = "csv")

# ส่งออกเป็น Excel พร้อม feedback
export_results(results_par, "output/grades_with_feedback.xlsx", 
               format = "xlsx", include_feedback = TRUE)

# ส่งออกเป็น JSON
export_results(results_par, "output/grades.json", format = "json")

# สร้างรายงานสรุป
create_grading_report(results_par, "output/grading_report.md")
```

## 📊 ตัวอย่างผลลัพธ์

### Progress Display
```
Starting parallel grading with 4 cores...
Progress: 25/100 (25.0%) completed
Progress: 50/100 (50.0%) completed  
Progress: 75/100 (75.0%) completed
Progress: 100/100 (100.0%) completed
```

### ตัวอย่างข้อมูลผลลัพธ์
```r
# ผลลัพธ์ในรูปแบบ data frame
> head(results_par)
  student_id item_id total_score            overall_feedback
1        001 item_006        7.5  จุดแข็ง: เข้าใจแนวคิดพื้นฐาน...
2        002 item_006        6.0  ควรให้ตัวอย่างเพิ่มเติม...
3        003 item_006        8.5  คำตอบครบถ้วนและชัดเจน...
```

### ตัวอย่างรายงานสรุป
```
# Grading Report

## Summary Statistics
- Total Responses: 100
- Mean Score: 7.25
- Median Score: 7.50
- Min Score: 3.00
- Max Score: 9.00

## Score Distribution
3.0: 2 students
4.5: 5 students  
6.0: 15 students
7.5: 45 students
9.0: 33 students

Generated on: 2024-01-15 14:30:25
```

## 🔧 ฟังก์ชันหลักและการใช้งาน

### 📝 ฟังก์ชันหลักสำหรับการตรวจข้อสอบ

| ฟังก์ชัน | คำอธิบาย | ตัวอย่างการใช้ |
|---------|----------|----------------|
| `load_rubric()` | โหลดเกณฑ์การให้คะแนน | `rubric <- load_rubric("rubric/item_001.md")` |
| `load_answer_key()` | โหลดเฉลยและแนวคำตอบ | `key <- load_answer_key("rubric/item_001_key.md")` |
| `grade_responses()` | ตรวจและให้คะแนน (single-agent) | `results <- grade_responses(responses, rubric, key, .parallel = TRUE)` |
| `grade_responses_multiagent()` | ตรวจและให้คะแนน (multiagent) 🆕 | `results <- grade_responses_multiagent(responses, rubric, key, max_iterations = 3)` |
| `export_results()` | ส่งออกผลลัพธ์ | `export_results(results, "output.csv")` |
| `create_grading_report()` | สร้างรายงานสรุป | `create_grading_report(results, "report.md")` |
| `create_grading_schema()` | สร้าง JSON schema สำหรับ LLM | `schema <- create_grading_schema()` |

### 🔧 ฟังก์ชัน Utility และการจัดการข้อมูล

| ฟังก์ชัน | คำอธิบาย | ตัวอย่างการใช้ |
|---------|----------|----------------|
| `validate_response_data()` | ตรวจสอบข้อมูล input | `validate_response_data(responses)` |
| `init_logger()` | ตั้งค่า logging ระบบ | `init_logger("logs/app.log", "INFO")` |
| `should_use_parallel()` | ตรวจสอบว่าควรใช้ parallel | `if(should_use_parallel(n)) { ... }` |
| `get_optimal_cores()` | หาจำนวน cores ที่เหมาะสม | `cores <- get_optimal_cores(nrow(responses))` |

### 📊 ฟังก์ชันการวิเคราะห์และตรวจสอบผลลัพธ์

| ฟังก์ชัน | คำอธิบาย | ตัวอย่างการใช้ |
|---------|----------|----------------|
| `view_student_results()` | ดูรายละเอียดนักเรียน | `view_student_results(results, 1)` |
| `extract_criteria_scores()` | แยกคะแนนแต่ละ criterion | `extract_criteria_scores(results)` |
| `debug_per_criterion()` | วิเคราะห์โครงสร้าง per_criterion | `debug_per_criterion(results)` |

### 📄 ฟังก์ชันจัดการ PDF สำหรับข้อสอบ

| ฟังก์ชัน | คำอธิบาย | ตัวอย่างการใช้ |
|---------|----------|----------------|
| `split_pdf_by_question()` | แยก PDF ตามข้อสอบ | `split_pdf_by_question("exam.pdf", n_questions = 4)` |
| `inspect_pdf_structure()` | ตรวจสอบโครงสร้าง PDF | `inspect_pdf_structure("exam.pdf", 4)` |

### 🔍 ฟังก์ชัน OCR สำหรับลายมือ

| ฟังก์ชัน | คำอธิบาย | ตัวอย่างการใช้ |
|---------|----------|----------------|
| `ocr_handwriting()` | แปลงลายมือเป็นข้อความ | `result <- ocr_handwriting("handwriting.jpg")` |
| `ocr_batch_handwriting()` | OCR หลายไฟล์พร้อมกัน | `results <- ocr_batch_handwriting(image_paths)` |

## 🎨 การปรับแต่ง System Prompt

### ประเภทของ System Prompt

#### 1. **อาจารย์เข้มงวด**
```r
strict_prompt <- "คุณเป็นอาจารย์ที่เข้มงวดและใช้เกณฑ์สูง ให้คะแนนอย่างเข้มข้นตามรายละเอียดใน rubric อย่างเคร่งครัด ไม่ให้คะแนนเต็มง่ายๆ หากคำตอบไม่ครบถ้วนสมบูรณ์"

results <- grade_responses(responses, rubric, key, system_prompt = strict_prompt)
```

#### 2. **อาจารย์ให้กำลังใจ**
```r
encouraging_prompt <- "คุณเป็นอาจารย์ที่ให้กำลังใจและสร้างแรงบันดาลใจ ให้คะแนนอย่างเป็นธรรมแต่เน้นการให้คำแนะนำเชิงบวก ชี้จุดแข็งก่อนจุดที่ควรปรับปรุง และให้คำแนะนำที่สร้างสรรค์"

results <- grade_responses(responses, rubric, key, system_prompt = encouraging_prompt)
```

#### 3. **ผู้เชี่ยวชาญเฉพาะทาง**
```r
expert_prompt <- "คุณเป็นผู้เชี่ยวชาญด้านการศึกษาและการประเมินผล มีประสบการณ์ในการใช้ข้อมูลเพื่อการเรียนการสอนมากว่า 15 ปี ให้คะแนนด้วยความเข้าใจลึกในสาขาวิชา"

results <- grade_responses(responses, rubric, key, system_prompt = expert_prompt)
```

#### 4. **การประเมินแบบโฮลิสติก**
```r
holistic_prompt <- "ประเมินคำตอบอย่างรอบด้าน มองภาพรวมของความเข้าใจและการคิดวิเคราะห์ของนักเรียน ไม่เพียงแค่ดูคำตอบที่ถูกต้องแต่ดูกระบวนการคิดด้วย"

results <- grade_responses(responses, rubric, key, system_prompt = holistic_prompt)
```

#### 5. **ภาษาอังกฤษ**
```r
english_prompt <- "You are an expert educator specializing in assessment and evaluation. Grade student responses fairly and consistently, providing constructive feedback that promotes learning."

results <- grade_responses(responses, rubric, key, system_prompt = english_prompt)
```

### การรวม System Prompt กับ Parallel Processing

```r
# กรณีมีนักเรียนจำนวนมาก ใช้ parallel processing พร้อม custom prompt
custom_prompt <- "ให้คะแนนอย่างเป็นธรรมและสม่ำเสมอ เน้นการให้ feedback ที่มีประโยชน์"

large_results <- grade_responses(
  responses = responses,
  rubric = rubric,
  key = answer_key,
  system_prompt = custom_prompt,
  .parallel = TRUE,
  .cores = get_optimal_cores(nrow(responses)),
  .progress = TRUE
)
```

## 🤖🤖 ระบบตรวจแบบ Multiagent (ใหม่!)

### ภาพรวม

ระบบ **Multiagent Grading** เป็นฟีเจอร์ใหม่ที่ใช้ AI 4 ตัวทำงานร่วมกันเพื่อให้ผลการตรวจที่มีความน่าเชื่อถือและแม่นยำมากขึ้น เหมาะสำหรับข้อสอบที่มีความสำคัญสูง (high-stakes assessment)

### สถาปัตยกรรม Multiagent System

```
┌─────────────────────────────────────────────────────────┐
│              MULTIAGENT GRADING WORKFLOW                │
└─────────────────────────────────────────────────────────┘

Step 1: Independent Grading
┌──────────────┐                    ┌──────────────┐
│   Agent 1    │                    │   Agent 2    │
│  (Conceptual │                    │  (Detail &   │
│   Thinking)  │                    │ Completeness)│
└──────┬───────┘                    └──────┬───────┘
       │                                   │
       │      ผลการตรวจ                     │      ผลการตรวจ
       │      + คะแนน                       │      + คะแนน
       │      + feedback                    │      + feedback
       │                                   │
       └────────────┬──────────────────────┘
                    │
                    ▼
         ┌──────────────────┐
         │     Agent 3      │
         │   (Consistency   │ ◄── ตรวจสอบความสอดคล้อง
         │     Checker)     │
         └─────────┬────────┘
                   │
        ┌──────────┴──────────┐
        │                     │
   ไม่สอดคล้อง             สอดคล้อง
   (Re-grade)               │
        │                     │
        └─► กลับไปตรวจใหม่      │
            (ส่ง feedback)     │
            max iterations     │
                              ▼
                   ┌──────────────────┐
                   │     Agent 4      │
                   │   (Feedback      │
                   │   Synthesizer)   │
                   └─────────┬────────┘
                             │
                             ▼
                   ┌──────────────────┐
                   │  Final Result    │
                   │  + คะแนนรวม       │
                   │  + feedback       │
                   │  + metadata       │
                   └──────────────────┘
```

### บทบาทของแต่ละ Agent

| Agent | บทบาท | จุดเน้น |
|-------|--------|---------|
| **Agent 1** | ผู้ตรวจท่านที่ 1 | ความเข้าใจแนวคิดหลัก, การคิดวิเคราะห์ |
| **Agent 2** | ผู้ตรวจท่านที่ 2 | รายละเอียด, ความสมบูรณ์ของคำตอบ |
| **Agent 3** | ตรวจสอบความสอดคล้อง | เปรียบเทียบผลการตรวจ, ให้ feedback สำหรับการตรวจใหม่ |
| **Agent 4** | สังเคราะห์ feedback | รวม feedback จากทั้งสอง agent ให้เป็น feedback เดียวที่สมบูรณ์ |

### การใช้งาน Multiagent Grading

#### ตัวอย่างพื้นฐาน

```r
library(kruroograder)

# โหลดข้อมูล
responses <- read_csv("data/responses.csv")
rubric <- load_rubric("rubric/item_001.md")
key <- load_answer_key("rubric/item_001_key.md")

# ตรวจแบบ multiagent
results <- grade_responses_multiagent(
  responses = responses,
  rubric = rubric,
  key = key,
  .progress = TRUE
)

# ดูผลลัพธ์
print(results)
```

#### ผลลัพธ์ที่ได้

```r
> head(results)
  student_id item_id total_score agent1_score agent2_score score_difference iterations was_consistent
1        001     001         7.5          7.0          8.0              1.0          1          TRUE
2        002     001         6.0          5.5          6.5              1.0          2         FALSE
3        003     001         8.5          8.5          8.5              0.0          1          TRUE

  overall_feedback                                    grader_agreement_note
1 "จุดแข็ง: เข้าใจแนวคิดพื้นฐาน..."                  "ผู้ตรวจทั้งสองเห็นตรงกันในภาพรวม..."
2 "ควรเพิ่มตัวอย่างประกอบ..."                        "มีความแตกต่างเล็กน้อยในการประเมิน..."
```

#### ปรับแต่ง Agent Prompts

```r
# กำหนด system prompt สำหรับแต่ละ agent เพื่อมุมมองที่แตกต่างกัน

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

results <- grade_responses_multiagent(
  responses = responses,
  rubric = rubric,
  key = key,
  system_prompt_agent1 = agent1_prompt,
  system_prompt_agent2 = agent2_prompt,
  max_iterations = 3,              # จำนวนครั้งสูงสุดในการตรวจใหม่
  consistency_threshold = 0.15,     # threshold ความแตกต่างของคะแนน (15% ของคะแนนเต็ม)
  .progress = TRUE
)
```

#### ใช้โมเดลต่างกันสำหรับแต่ละ Agent (แนะนำ!)

```r
# 🎯 BEST PRACTICE: ใช้โมเดลต่างกันเพื่อลด bias และเพิ่มความหลากหลาย
results <- grade_responses_multiagent(
  responses = responses,
  rubric = rubric,
  key = key,

  # Agent 1: โมเดลสำหรับการคิดแบบ conceptual
  model_config_agent1 = list(model = "gpt-4o-mini", provider = "openai"),

  # Agent 2: โมเดลสำหรับการตรวจรายละเอียด
  model_config_agent2 = list(model = "gpt-5-nano", provider = "openai"),

  # Agent 3: โมเดลที่แข็งแกร่งสำหรับตรวจสอบความสอดคล้อง
  model_config_agent3 = list(model = "gpt-4o", provider = "openai"),

  # Agent 4: โมเดลสำหรับสังเคราะห์ feedback
  model_config_agent4 = list(model = "gpt-4o", provider = "openai"),

  .progress = TRUE
)
```

#### ปรับแต่ง Temperature และ Top_p สำหรับแต่ละ Agent

```r
# ใช้โมเดลต่างกัน + พารามิเตอร์ต่างกันเพื่อความหลากหลายสูงสุด
results <- grade_responses_multiagent(
  responses = responses,
  rubric = rubric,
  key = key,

  # Agent 1: Creative conceptual thinking
  model_config_agent1 = "gpt-4o-mini",
  .temperature_agent1 = 0.7,  # สูงกว่า = คิดสร้างสรรค์มากขึ้น
  .top_p_agent1 = 0.95,

  # Agent 2: Conservative detail checking
  model_config_agent2 = "gpt-5-nano",
  .temperature_agent2 = 0.3,  # ต่ำกว่า = เข้มงวดและสม่ำเสมอมากขึ้น
  .top_p_agent2 = 0.85,

  # Agent 3: Balanced consistency checker
  model_config_agent3 = "gpt-4o",
  .temperature_agent3 = 0.5,  # กลางๆ = สมดุล

  # Agent 4: Synthesis with moderate creativity
  model_config_agent4 = "gpt-4o",
  .temperature_agent4 = 0.6,  # ค่อนข้างสร้างสรรค์เพื่อ feedback ที่ดี

  max_iterations = 3,
  .progress = TRUE
)
```

#### Multiagent + Parallel Processing

```r
# สำหรับข้อสอบจำนวนมาก พร้อมใช้โมเดลต่างกัน
results <- grade_responses_multiagent(
  responses = large_responses,
  rubric = rubric,
  key = key,
  model_config_agent1 = "gpt-4o-mini",
  model_config_agent2 = "gpt-5-nano",
  model_config_agent3 = "gpt-4o",
  model_config_agent4 = "gpt-4o",
  .parallel = TRUE,
  .cores = 4,
  .progress = TRUE
)
```

### พารามิเตอร์สำคัญ

| พารามิเตอร์ | ค่าเริ่มต้น | คำอธิบาย |
|------------|------------|----------|
| `max_iterations` | 2 | จำนวนครั้งสูงสุดในการตรวจใหม่เมื่อผลไม่สอดคล้อง |
| `consistency_threshold` | 0.2 | เกณฑ์ความแตกต่างของคะแนน (สัมพัทธ์กับคะแนนเต็ม) |
| `system_prompt_agent1` | มุมมองแนวคิด | system prompt สำหรับ agent ตัวที่ 1 |
| `system_prompt_agent2` | มุมมองรายละเอียด | system prompt สำหรับ agent ตัวที่ 2 |
| `model_config_agent1` | NULL (ใช้ model_config) | Model config สำหรับ agent 1 |
| `model_config_agent2` | NULL (ใช้ model_config) | Model config สำหรับ agent 2 |
| `model_config_agent3` | NULL (ใช้ model_config) | Model config สำหรับ agent 3 |
| `model_config_agent4` | NULL (ใช้ model_config) | Model config สำหรับ agent 4 |
| `.temperature_agent1-4` | NULL (ใช้ .temperature) | Temperature สำหรับแต่ละ agent |
| `.top_p_agent1-4` | NULL (ใช้ .top_p) | Top_p สำหรับแต่ละ agent |

### การวิเคราะห์ผลลัพธ์ Multiagent

```r
# สรุปความสอดคล้อง
cat("Consistent grading:", sum(results$was_consistent), "out of", nrow(results), "\n")
cat("Average score difference:", round(mean(results$score_difference), 2), "\n")

# กรณีที่มีความแตกต่างสูง
high_disagreement <- results %>%
  filter(score_difference > 2) %>%
  select(student_id, agent1_score, agent2_score, score_difference, grader_agreement_note)

print(high_disagreement)

# เปรียบเทียบคะแนนจากแต่ละ agent
results %>%
  select(student_id, agent1_score, agent2_score, total_score) %>%
  mutate(
    avg_agent_score = (agent1_score + agent2_score) / 2,
    synthesis_adjustment = total_score - avg_agent_score
  )
```

### ข้อดีของ Multiagent Grading

| ข้อดี | คำอธิบาย |
|------|----------|
| 🎯 **ความน่าเชื่อถือสูง** | การให้คะแนนจากหลายมุมมองลดอคติและความผิดพลาด |
| 🔄 **Quality Control** | Agent 3 ตรวจสอบความสอดคล้องอัตโนมัติ |
| 🔁 **Iterative Refinement** | ตรวจใหม่อัตโนมัติเมื่อพบความไม่สอดคล้อง |
| 💡 **Feedback ที่ดีขึ้น** | รวม insight จากหลาย perspective |
| 📊 **Metadata ครบถ้วน** | ข้อมูลเพิ่มเติมสำหรับวิเคราะห์และปรับปรุง |

### เมื่อไหร่ควรใช้ Multiagent

✅ **ควรใช้เมื่อ:**
- ข้อสอบมีความสำคัญสูง (midterm, final exam)
- ต้องการความน่าเชื่อถือสูง
- ข้อสอบมีความซับซ้อน มีหลายมิติในการประเมิน
- มีงบประมาณ API เพียงพอ (ใช้ API calls มากกว่า single-agent 2-4 เท่า)

❌ **ไม่จำเป็นต้องใช้เมื่อ:**
- ข้อสอบง่าย มีเกณฑ์ชัดเจน
- quiz ทั่วไป หรือการประเมินรายวัน
- งบประมาณจำกัด
- ต้องการความเร็วเป็นหลัก

### การประหยัดต้นทุน

```r
# ใช้ multiagent เฉพาะข้อที่ต้องการความแม่นยำสูง
high_stake_responses <- responses %>% filter(item_type == "essay")
quiz_responses <- responses %>% filter(item_type == "quiz")

# High-stake: ใช้ multiagent
essay_results <- grade_responses_multiagent(high_stake_responses, rubric, key)

# Quiz: ใช้ single-agent
quiz_results <- grade_responses(quiz_responses, rubric, key)

# รวมผลลัพธ์
all_results <- bind_rows(essay_results, quiz_results)
```

### ตัวอย่างการใช้งานจริง

ดูไฟล์ `example_multiagent.R` สำหรับตัวอย่างการใช้งานแบบละเอียด:
```r
source("example_multiagent.R")
```

## 📄 การจัดการไฟล์ PDF และ OCR

### การแยก PDF ตามข้อสอบ

kruroograder รองรับการแยกไฟล์ PDF ที่มีกระดาษคำตอบหลายคนออกเป็นไฟล์แยกตามข้อสอบ เหมาะสำหรับกรณีที่สแกนกระดาษคำตอบแบบเรียงลำดับ:

```r
# ตรวจสอบโครงสร้าง PDF ก่อน
inspect_pdf_structure("exam_scanned.pdf", n_questions = 4)

# แยก PDF เป็นไฟล์ตามข้อ
result <- split_pdf_by_question(
  input_pdf = "exam_scanned.pdf",
  n_questions = 4,
  out_dir = "separated_questions",
  filename_prefix = "Q",
  filename_suffix = "_all.pdf",
  dry_run = FALSE,       # ตั้งเป็น TRUE เพื่อทดสอบก่อน
  overwrite = TRUE
)

# ดูผลลัพธ์
print(result$summary)
```

#### ตัวอย่าง Output:
```
กำลังแยก PDF เป็น 4 ไฟล์...
✓ สร้าง Q1_all.pdf (25 หน้า)
✓ สร้าง Q2_all.pdf (25 หน้า)  
✓ สร้าง Q3_all.pdf (25 หน้า)
✓ สร้าง Q4_all.pdf (25 หน้า)
```

### การใช้งาน OCR สำหรับลายมือ

ระบบ kruroograder รองรับการแปลงลายมือจากภาพเป็นข้อความโดยใช้ OpenAI Vision API ซึ่งเหมาะสมสำหรับการประมวลผลข้อสอบที่เขียนด้วยลายมือ

#### 🎯 คุณสมบัติของ OCR System

- **รองรับภาพหลายรูปแบบ**: JPG, PNG, JPEG, GIF, WebP
- **ประมวลผลแบบ Batch**: สามารถประมวลผลภาพหลายไฟล์พร้อมกัน
- **ปรับแต่ง System Prompt ได้**: สำหรับการสกัดข้อความแบบเฉพาะทาง
- **จัดการ Rate Limiting**: มีการหน่วงเวลาระหว่างการเรียก API
- **Error Handling**: จัดการข้อผิดพลาดและไฟล์ที่ประมวลผลไม่ได้
- **Progress Tracking**: แสดงความคืบหน้าระหว่างการประมวลผล

#### 🔧 การตั้งค่าเบื้องต้น

```r
# ตั้งค่า OpenAI API Key
Sys.setenv(OPENAI_API_KEY = "your-openai-api-key")

# ตรวจสอบการตั้งค่า
if (Sys.getenv("OPENAI_API_KEY") == "") {
  stop("กรุณาตั้งค่า OPENAI_API_KEY")
}

# โหลดแพ็กเกจที่จำเป็น
library(kruroograder)
library(dplyr)
library(readr)
```

#### 📸 OCR ไฟล์เดี่ยว

```r
# OCR ไฟล์เดี่ยวแบบพื้นฐาน
result <- ocr_handwriting(
  image_path = "student_answer.jpg"
)

# ดูผลลัพธ์
cat("คำตอบที่สกัดได้:\n", result$answer)
cat("\nสถิติการใช้งาน:\n")
cat("- Prompt tokens:", result$prompt_tokens, "\n")
cat("- Completion tokens:", result$completion_tokens, "\n")
cat("- รวม tokens:", result$tokens_used, "\n")
```

#### 🎨 การปรับแต่ง System Prompt สำหรับ OCR

```r
# สำหรับข้อสอบคณิตศาสตร์
math_prompt <- "สกัดคำตอบทางคณิตศาสตร์จากภาพลายมือ รวมถึงสูตร สมการ และการคำนวณ 
ให้ระวังเลขและสัญลักษณ์ทางคณิตศาสตร์ เช่น +, -, ×, ÷, =, √ ให้แม่นยำ"

math_result <- ocr_handwriting(
  image_path = "math_answer.jpg",
  system_prompt = math_prompt,
  model_config = "gpt-4o"  # ใช้โมเดลที่แม่นยำกว่า
)

# สำหรับข้อสอบวิทยาศาสตร์
science_prompt <- "สกัดคำตอบวิทยาศาสตร์จากภาพลายมือ ให้ความสำคัญกับ:
- คำศัพท์ทางวิทยาศาสตร์
- ชื่อสารเคมี และสูตรเคมี
- หน่วยการวัด เช่น มก., ลิตร, เซลเซียส
- ขั้นตอนการทดลอง"

science_result <- ocr_handwriting(
  image_path = "science_answer.jpg",
  system_prompt = science_prompt
)

# สำหรับข้อสอบภาษาไทย
thai_prompt <- "สกัดคำตอบภาษาไทยจากภาพลายมือ ให้ความสำคัญกับ:
- การสะกดคำให้ถูกต้อง
- เครื่องหมายวรรคตอน
- การใช้ไม้หันอากาศ ไม้ไต่คู้
- ความสมบูรณ์ของประโยค"

thai_result <- ocr_handwriting(
  image_path = "thai_answer.jpg",
  system_prompt = thai_prompt
)
```

#### 🔄 OCR หลายไฟล์พร้อมกัน (Batch Processing)

```r
# เตรียมรายการไฟล์
image_dir <- "student_images"
image_files <- list.files(image_dir, 
                         pattern = "\\.(jpg|jpeg|png)$", 
                         full.names = TRUE)

cat("พบไฟล์ภาพ", length(image_files), "ไฟล์\n")

# OCR แบบ batch พร้อมการตั้งค่าที่เหมาะสม
batch_results <- ocr_batch_handwriting(
  image_paths = image_files,
  batch_size = 5,                    # ประมวลผลทีละ 5 ไฟล์
  system_prompt = "สกัดคำตอบของนักเรียนจากภาพลายมือให้ครบถ้วนและแม่นยำ 
                  ไม่เอาข้อความที่เขียนว่า 'พื้นที่สำหรับตอบคำถาม' หรือคำแนะนำอื่นๆ",
  delay_seconds = 3,                 # หน่วงเวลา 3 วินาทีระหว่างไฟล์
  progress = TRUE,                   # แสดง progress bar
  save_intermediate = TRUE,          # บันทึกผลลัพธ์ระหว่างทาง
  model_config = "gpt-4o-mini",
  max_tokens = 800
)

# ดูสรุปผลลัพธ์
print(batch_results)
```

#### 📊 ตัวอย่างผลลัพธ์ OCR:

```r
> head(batch_results, 3)
# A tibble: 3 × 7
  image_file     answer                          prompt_tokens completion_tokens tokens_used model_used  processed_at       
  <chr>          <chr>                                   <int>             <int>       <int> <chr>       <dttm>            
1 student001.jpg "การใช้ข้อมูลในการตัดสินใจมีค…"            850               120         970 gpt-4o-mini 2024-10-11 14:30:45
2 student002.jpg "ข้อมูลมีความสำคัญต่อการตัดส…"             780               95          875 gpt-4o-mini 2024-10-11 14:30:48  
3 student003.jpg "ในยุคปัจจุบันข้อมูลเป็นสิ่ง…"              920               135        1055 gpt-4o-mini 2024-10-11 14:30:51

> # สถิติการใช้งาน
> summarise(batch_results,
+   total_images = n(),
+   total_tokens = sum(tokens_used),
+   avg_tokens_per_image = mean(tokens_used),
+   total_cost_estimate = total_tokens * 0.00015 / 1000  # ประมาณการค่าใช้จ่าย
+ )
# A tibble: 1 × 4
  total_images total_tokens avg_tokens_per_image total_cost_estimate
         <int>        <int>                <dbl>               <dbl>
1           25        24500                  980               0.037
```

#### 🔍 การตรวจสอบคุณภาพ OCR

```r
# ฟังก์ชันตรวจสอบคุณภาพ OCR
check_ocr_quality <- function(ocr_results) {
  cat("=== การประเมินคุณภาพ OCR ===\n")
  
  # ตรวจสอบความยาวคำตอบ
  answer_lengths <- nchar(ocr_results$answer)
  cat("ความยาวคำตอบ:\n")
  cat("  - เฉลี่ย:", round(mean(answer_lengths)), "ตัวอักษร\n")
  cat("  - น้อยสุด:", min(answer_lengths), "ตัวอักษร\n")
  cat("  - มากสุด:", max(answer_lengths), "ตัวอักษร\n")
  
  # ตรวจสอบคำตอบที่สั้นผิดปกติ (อาจเป็น OCR error)
  short_answers <- which(answer_lengths < 20)
  if (length(short_answers) > 0) {
    cat("\n⚠️  คำตอบที่สั้นผิดปกติ (< 20 ตัวอักษร):\n")
    for (i in short_answers) {
      cat("  -", ocr_results$image_file[i], ":", ocr_results$answer[i], "\n")
    }
  }
  
  # ตรวจสอบการใช้ tokens
  cat("\nการใช้ tokens:\n")
  cat("  - เฉลี่ย:", round(mean(ocr_results$tokens_used)), "tokens/ภาพ\n")
  cat("  - รวมทั้งหมด:", sum(ocr_results$tokens_used), "tokens\n")
  
  return(invisible(ocr_results))
}

# ใช้งาน
check_ocr_quality(batch_results)
```

#### 💡 เทคนิคการปรับปรุงคุณภาพ OCR

```r
# 1. การเลือกโมเดลตามความซับซ้อน
simple_ocr <- function(image_path) {
  ocr_handwriting(image_path, model_config = "gpt-4o-mini")  # เร็ว, ประหยัด
}

complex_ocr <- function(image_path) {
  ocr_handwriting(image_path, model_config = "gpt-4o")       # แม่นยำกว่า
}

# 2. การใช้ System Prompt เฉพาะสำหรับข้อสอบแต่ละประเภท
create_subject_prompt <- function(subject) {
  prompts <- list(
    math = "สกัดคำตอบคณิตศาสตร์ ให้ความสำคัญกับตัวเลข สูตร และเครื่องหมาย",
    science = "สกัดคำตอบวิทยาศาสตร์ ให้ความสำคัญกับคำศัพท์เฉพาะและหน่วย", 
    thai = "สกัดคำตอบภาษาไทย ให้ความสำคัญกับการสะกดและเครื่องหมายวรรคตอน",
    english = "สกัดคำตอบภาษาอังกฤษ ให้ความสำคัญกับ grammar และ spelling",
    social = "สกัดคำตอบสังคมศึกษา ให้ความสำคัญกับชื่อสถานที่ วันที่ และเหตุการณ์"
  )
  return(prompts[[subject]] %||% "สกัดคำตอบจากภาพลายมือให้ถูกต้องและครบถ้วน")
}

# ใช้งาน
math_results <- ocr_batch_handwriting(
  image_paths = math_images,
  system_prompt = create_subject_prompt("math")
)
```

#### 🚀 Workflow ครบวงจร: OCR + การตรวจข้อสอบ

```r
# ========== ขั้นตอนที่ 1: เตรียมข้อมูล ==========
library(kruroograder)
library(dplyr)
library(stringr)

# ตั้งค่า API
Sys.setenv(OPENAI_API_KEY = "your-api-key")

# โหลด rubric และ answer key
rubric <- load_rubric("rubric/item_006_rubric.md")
answer_key <- load_answer_key("rubric/item_006_key.md")

# ========== ขั้นตอนที่ 2: OCR ภาพลายมือ ==========
image_dir <- "student_handwriting/"
image_files <- list.files(image_dir, pattern = "\\.(jpg|png)$", full.names = TRUE)

# กำหนด system prompt สำหรับวิชาที่ตรวจ
subject_prompt <- "สกัดคำตอบของนักเรียนจากภาพลายมือในวิชาการใช้ข้อมูลเพื่อการตัดสินใจ 
ให้ความสำคัญกับ:
- แนวคิดและทฤษฎีที่เกี่ยวข้อง
- ตัวอย่างประกอบการอธิบาย  
- การเชื่อมโยงเนื้อหาเข้าด้วยกัน
ไม่ต้องเอาข้อความแนะนำหรือส่วนหัวของกระดาษมา"

# OCR แบบ batch
cat("🔍 เริ่มการ OCR ภาพลายมือ...\n")
ocr_results <- ocr_batch_handwriting(
  image_paths = image_files,
  batch_size = 8,
  system_prompt = subject_prompt,
  delay_seconds = 2,
  progress = TRUE,
  save_intermediate = TRUE,
  model_config = "gpt-4o-mini"
)

cat("✅ OCR เสร็จสิ้น! ประมวลผลได้", nrow(ocr_results), "ไฟล์\n")

# ========== ขั้นตอนที่ 3: เตรียมข้อมูลสำหรับการตรวจ ==========
# แปลงผล OCR เป็นรูปแบบที่ grade_responses() ต้องการ
responses_from_ocr <- ocr_results %>%
  mutate(
    # สกัด student_id จากชื่อไฟล์ (เช่น student_001.jpg -> 001)
    student_id = str_extract(image_file, "\\d+"),
    item_id = "item_006",                    # รหัสข้อสอบ
    response_text = answer,                  # คำตอบที่ OCR ได้
    question_text = rubric$question          # คำถาม
  ) %>%
  select(student_id, item_id, response_text, question_text) %>%
  filter(!is.na(student_id), nchar(response_text) > 10)  # กรองข้อมูลที่ไม่สมบูรณ์

cat("📝 เตรียมข้อมูลสำหรับการตรวจได้", nrow(responses_from_ocr), "รายการ\n")

# ตรวจสอบข้อมูล
validate_response_data(responses_from_ocr)

# ========== ขั้นตอนที่ 4: ตรวจและให้คะแนน ==========
cat("🤖 เริ่มการตรวจข้อสอบด้วย AI...\n")

grading_results <- grade_responses(
  responses = responses_from_ocr,
  rubric = rubric,
  key = answer_key,
  system_prompt = "ประเมินคำตอบที่มาจาก OCR โดยคำนึงว่าอาจมีข้อผิดพลาดในการสกัดข้อความ 
                  ให้ความสำคัญกับเนื้อหาและความเข้าใจมากกว่าการสะกดที่แม่นยำ",
  .parallel = should_use_parallel(nrow(responses_from_ocr)),
  .cores = get_optimal_cores(nrow(responses_from_ocr)),
  .progress = TRUE,
  model_config = "gpt-4o-mini"
)

cat("✅ การตรวจเสร็จสิ้น!\n")

# ========== ขั้นตอนที่ 5: รวมผลลัพธ์และส่งออก ==========
# รวมข้อมูล OCR และผลการตรวจ
final_results <- grading_results %>%
  left_join(
    ocr_results %>% 
      select(image_file, tokens_used_ocr = tokens_used, processed_at), 
    by = c("student_id" = str_extract(ocr_results$image_file, "\\d+"))
  )

# สร้างรายงานสรุป
summary_report <- final_results %>%
  summarise(
    total_students = n(),
    mean_score = round(mean(total_score, na.rm = TRUE), 2),
    median_score = median(total_score, na.rm = TRUE),
    min_score = min(total_score, na.rm = TRUE),
    max_score = max(total_score, na.rm = TRUE),
    total_ocr_tokens = sum(tokens_used_ocr, na.rm = TRUE),
    estimated_cost = round(total_ocr_tokens * 0.00015 / 1000, 4)
  )

print(summary_report)

# ส่งออกผลลัพธ์
export_results(final_results, "output/ocr_grading_results.xlsx", 
               format = "xlsx", include_feedback = TRUE)

# สร้างรายงานการตรวจ
create_grading_report(final_results, "output/ocr_grading_report.md")

cat("📊 ส่งออกผลลัพธ์เรียบร้อย!\n")
```

#### 📈 การตรวจสอบและปรับปรุงคุณภาพ

```r
# ฟังก์ชันเปรียบเทียบคำตอบ OCR กับการพิมพ์
compare_ocr_manual <- function(ocr_text, manual_text) {
  # คำนวณความคล้ายคลึง (simple similarity)
  similarity <- 1 - adist(ocr_text, manual_text) / max(nchar(ocr_text), nchar(manual_text))
  
  list(
    similarity = round(similarity, 3),
    ocr_length = nchar(ocr_text),
    manual_length = nchar(manual_text),
    length_diff = abs(nchar(ocr_text) - nchar(manual_text))
  )
}

# ฟังก์ชันหา OCR errors ที่พบบ่อย
analyze_ocr_errors <- function(ocr_results) {
  errors <- list()
  
  # คำที่อาจมีปัญหาในการ OCR
  problematic_patterns <- c(
    "กำลัง" = c("กาลัง", "การัง", "กำร่ง"),
    "ความ" = c("ฃวาม", "ค ว าม", "ความ"),
    "ตัวอย่าง" = c("ตัวอย าง", "ตัวอยาง", "ตั ว อ ย่ า ง"),
    "ข้อมูล" = c("ข อมูล", "ขอมูล", "ข้อ มูล")
  )
  
  for (correct in names(problematic_patterns)) {
    wrong_versions <- problematic_patterns[[correct]]
    for (wrong in wrong_versions) {
      count <- sum(grepl(wrong, ocr_results$answer, fixed = TRUE))
      if (count > 0) {
        errors[[paste(wrong, "->", correct)]] <- count
      }
    }
  }
  
  return(errors)
}

# วิเคราะห์ความน่าเชื่อถือของ OCR
assess_ocr_reliability <- function(ocr_results) {
  cat("=== การประเมินความน่าเชื่อถือ OCR ===\n")
  
  # วิเคราะห์ความยาวคำตอบ
  answer_lengths <- nchar(ocr_results$answer)
  
  cat("📏 สถิติความยาวคำตอบ:\n")
  cat("   เฉลี่ย:", round(mean(answer_lengths)), "ตัวอักษร\n")
  cat("   ส่วนเบี่ยงเบนมาตรฐาน:", round(sd(answer_lengths)), "\n")
  
  # หาคำตอบที่ผิดปกติ
  outliers <- which(answer_lengths < quantile(answer_lengths, 0.1) | 
                   answer_lengths > quantile(answer_lengths, 0.9))
  
  if (length(outliers) > 0) {
    cat("\n⚠️  คำตอบที่อาจมีปัญหา OCR:\n")
    for (i in outliers[1:min(5, length(outliers))]) {
      cat("   -", ocr_results$image_file[i], 
          "(", answer_lengths[i], "ตัวอักษร):\n")
      cat("     ", substr(ocr_results$answer[i], 1, 100), "...\n")
    }
  }
  
  # วิเคราะห์ errors ที่พบบ่อย
  errors <- analyze_ocr_errors(ocr_results)
  if (length(errors) > 0) {
    cat("\n🔍 ข้อผิดพลาด OCR ที่พบบ่อย:\n")
    for (error in names(errors)) {
      cat("   -", error, ":", errors[[error]], "ครั้ง\n")
    }
  }
  
  return(invisible(list(lengths = answer_lengths, errors = errors)))
}

# ใช้งาน
reliability_check <- assess_ocr_reliability(batch_results)
```

#### 💰 การคำนวณต้นทุน OCR

```r
# ฟังก์ชันคำนวณต้นทุน
calculate_ocr_cost <- function(ocr_results, 
                              cost_per_1k_tokens = 0.00015) {  # ราคา gpt-4o-mini
  
  total_tokens <- sum(ocr_results$tokens_used, na.rm = TRUE)
  total_cost <- total_tokens * cost_per_1k_tokens / 1000
  cost_per_image <- total_cost / nrow(ocr_results)
  
  cat("💰 การคำนวณต้นทุน OCR\n")
  cat("   จำนวนภาพ:", nrow(ocr_results), "ภาพ\n")
  cat("   รวม tokens:", format(total_tokens, big.mark = ","), "tokens\n")
  cat("   ต้นทุนรวม: $", round(total_cost, 4), "\n")
  cat("   ต้นทุนต่อภาพ: $", round(cost_per_image, 4), "\n")
  cat("   ต้นทุนต่อภาพ:", round(cost_per_image * 35, 2), "บาท (ประมาณ)\n")
  
  return(list(
    total_cost = total_cost,
    cost_per_image = cost_per_image,
    total_tokens = total_tokens
  ))
}

# เปรียบเทียบต้นทุนโมเดลต่างๆ
compare_model_costs <- function(n_images, avg_tokens_per_image = 1000) {
  models <- list(
    "gpt-4o-mini" = 0.00015,
    "gpt-4o" = 0.0025,
    "gpt-4-turbo" = 0.001
  )
  
  cat("📊 เปรียบเทียบต้นทุนโมเดล (", n_images, "ภาพ):\n")
  
  for (model in names(models)) {
    total_tokens <- n_images * avg_tokens_per_image
    cost <- total_tokens * models[[model]] / 1000
    cat("   ", model, ": $", round(cost, 4), 
        " (", round(cost * 35, 2), " บาท)\n")
  }
}

# ใช้งาน
cost_analysis <- calculate_ocr_cost(batch_results)
compare_model_costs(100)  # สำหรับ 100 ภาพ
```

#### ⚡ Best Practices สำหรับ OCR

```r
# 1. ✅ การเตรียมภาพที่ดี
prepare_images_for_ocr <- function(image_dir) {
  cat("📋 เช็คลิสต์การเตรียมภาพ:\n")
  cat("   ✓ ความละเอียด: อย่างน้อย 300 DPI\n")
  cat("   ✓ ความชัดเจน: ไม่เบลอ, แสงเพียงพอ\n") 
  cat("   ✓ การครอบตัด: เฉพาะพื้นที่คำตอบ\n")
  cat("   ✓ ทิศทาง: ภาพตั้งตรง, ไม่เอียง\n")
  cat("   ✓ รูปแบบ: JPG หรือ PNG\n")
  cat("   ✓ ขนาดไฟล์: ไม่เกิน 20MB ต่อไฟล์\n")
}

# 2. ✅ การจัดระเบียบไฟล์
organize_image_files <- function(source_dir, output_dir) {
  # สร้างโครงสร้างโฟลเดอร์
  dir.create(file.path(output_dir, "processed"), showWarnings = FALSE)
  dir.create(file.path(output_dir, "failed"), showWarnings = FALSE)
  dir.create(file.path(output_dir, "backup"), showWarnings = FALSE)
  
  cat("📁 โครงสร้างโฟลเดอร์:\n")
  cat("   📂", output_dir, "/processed/  <- ไฟล์ที่ประมวลผลแล้ว\n")
  cat("   📂", output_dir, "/failed/     <- ไฟล์ที่ประมวลผลไม่ได้\n") 
  cat("   📂", output_dir, "/backup/     <- สำรองไฟล์ต้นฉบับ\n")
}

# 3. ✅ การตั้งชื่อไฟล์แบบเป็นระบบ
create_naming_convention <- function() {
  cat("📝 แนวทางการตั้งชื่อไฟล์:\n")
  cat("   รูปแบบ: student_[ID]_item_[ITEM]_page_[PAGE].jpg\n")
  cat("   ตัวอย่าง:\n")
  cat("     - student_001_item_006_page_1.jpg\n")
  cat("     - student_002_item_006_page_1.jpg\n")
  cat("     - student_003_item_007_page_1.jpg\n")
}

# 4. ✅ การประมวลผลแบบปลอดภัย
safe_ocr_processing <- function(image_paths, ...) {
  # สร้างการสำรองข้อมูล
  backup_dir <- paste0("backup_", Sys.Date())
  dir.create(backup_dir, showWarnings = FALSE)
  
  cat("💾 สำรองข้อมูลใน:", backup_dir, "\n")
  
  # ประมวลผลพร้อม error handling
  tryCatch({
    results <- ocr_batch_handwriting(
      image_paths = image_paths,
      save_intermediate = TRUE,  # บันทึกผลระหว่างทาง
      ...
    )
    
    # สำรองผลลัพธ์
    write_csv(results, file.path(backup_dir, "ocr_results.csv"))
    cat("✅ บันทึกผลลัพธ์สำรองแล้ว\n")
    
    return(results)
    
  }, error = function(e) {
    cat("❌ เกิดข้อผิดพลาด:", e$message, "\n")
    cat("💡 ตรวจสอบ:\n")
    cat("   - API key ถูกต้องหรือไม่\n")
    cat("   - ไฟล์ภาพเสียหายหรือไม่\n")
    cat("   - เน็ตเวิร์กเชื่อมต่อปกติหรือไม่\n")
    return(NULL)
  })
}

# 5. ✅ การตรวจสอบก่อนเริ่มงาน
pre_flight_check <- function(image_dir, api_key = NULL) {
  cat("🚀 การตรวจสอบก่อนเริ่ม OCR\n")
  
  # ตรวจสอบ API key
  api_key <- api_key %||% Sys.getenv("OPENAI_API_KEY")
  if (api_key == "" || is.null(api_key)) {
    cat("❌ ไม่พบ OpenAI API key\n")
    return(FALSE)
  } else {
    cat("✅ พบ API key\n")
  }
  
  # ตรวจสอบไฟล์ภาพ
  if (!dir.exists(image_dir)) {
    cat("❌ ไม่พบโฟลเดอร์:", image_dir, "\n")
    return(FALSE)
  }
  
  image_files <- list.files(image_dir, pattern = "\\.(jpg|jpeg|png)$", ignore.case = TRUE)
  cat("📁 พบไฟล์ภาพ:", length(image_files), "ไฟล์\n")
  
  if (length(image_files) == 0) {
    cat("❌ ไม่พบไฟล์ภาพในโฟลเดอร์\n")
    return(FALSE)
  }
  
  # ตรวจสอบขนาดไฟล์
  file_sizes <- file.info(file.path(image_dir, image_files))$size / 1024 / 1024  # MB
  large_files <- which(file_sizes > 20)
  
  if (length(large_files) > 0) {
    cat("⚠️  ไฟล์ใหญ่เกิน 20MB:", length(large_files), "ไฟล์\n")
    cat("   แนะนำให้ลดขนาดก่อนประมวลผล\n")
  }
  
  # ประมาณการต้นทุน
  estimated_cost <- length(image_files) * 1000 * 0.00015 / 1000  # ประมาณ 1000 tokens/ภาพ
  cat("💰 ประมาณการต้นทุน: $", round(estimated_cost, 4), 
      " (", round(estimated_cost * 35, 2), " บาท)\n")
  
  cat("✅ พร้อมเริ่ม OCR!\n")
  return(TRUE)
}

# ตัวอย่างการใช้งาน Best Practices
run_complete_ocr_workflow <- function(image_dir, output_dir) {
  # 1. ตรวจสอบก่อนเริ่ม
  if (!pre_flight_check(image_dir)) {
    stop("การตรวจสอบไม่ผ่าน กรุณาแก้ไขปัญหาก่อน")
  }
  
  # 2. จัดระเบียบไฟล์
  organize_image_files(image_dir, output_dir)
  
  # 3. ประมวลผลอย่างปลอดภัย
  image_files <- list.files(image_dir, pattern = "\\.(jpg|png)$", 
                           full.names = TRUE, ignore.case = TRUE)
  
  results <- safe_ocr_processing(
    image_paths = image_files,
    batch_size = 5,
    delay_seconds = 2,
    progress = TRUE
  )
  
  # 4. ตรวจสอบคุณภาพ
  if (!is.null(results)) {
    reliability_check <- assess_ocr_reliability(results)
    cost_analysis <- calculate_ocr_cost(results)
  }
  
  return(results)
}
```

#### 🎯 เคล็ดลับการใช้ OCR ให้มีประสิทธิภาพ

##### 1. **การปรับปรุงคุณภาพภาพก่อน OCR**
```bash
# ใช้ ImageMagick ปรับปรุงภาพ (ถ้ามี)
convert input.jpg -density 300 -sharpen 0x1 -contrast-stretch 0.1x0.1% output.jpg

# หรือใช้ R package magick
library(magick)
img <- image_read("student_answer.jpg")
img_processed <- img %>%
  image_resize("2000x") %>%      # เพิ่มความละเอียด
  image_enhance() %>%            # เพิ่มความคมชัด
  image_contrast(sharpen = 1)    # เพิ่ม contrast

image_write(img_processed, "student_answer_processed.jpg")
```

##### 2. **การจัดการไฟล์ขนาดใหญ่**
```r
# แบ่งการประมวลผลเป็นกลุ่มเล็กๆ
process_large_batch <- function(image_dir, max_batch_size = 20) {
  all_files <- list.files(image_dir, pattern = "\\.(jpg|png)$", full.names = TRUE)
  n_batches <- ceiling(length(all_files) / max_batch_size)
  
  all_results <- list()
  
  for (i in 1:n_batches) {
    start_idx <- (i - 1) * max_batch_size + 1
    end_idx <- min(i * max_batch_size, length(all_files))
    batch_files <- all_files[start_idx:end_idx]
    
    cat("กำลังประมวลผล batch", i, "/", n_batches, "\n")
    
    batch_result <- ocr_batch_handwriting(
      image_paths = batch_files,
      batch_size = 5,
      delay_seconds = 3
    )
    
    # บันทึกผลแต่ละ batch
    write_csv(batch_result, paste0("batch_", i, "_results.csv"))
    all_results[[i]] <- batch_result
    
    # พักระหว่าง batch
    if (i < n_batches) Sys.sleep(10)
  }
  
  return(bind_rows(all_results))
}
```

##### 3. **การ Validate ผล OCR**
```r
# ฟังก์ชันตรวจสอบความถูกต้องของ OCR
validate_ocr_output <- function(ocr_results, min_chars = 20, max_chars = 2000) {
  validation_report <- ocr_results %>%
    mutate(
      char_count = nchar(answer),
      has_thai = grepl("[ก-ฮ]", answer),
      has_numbers = grepl("[0-9]", answer),
      has_english = grepl("[a-zA-Z]", answer),
      too_short = char_count < min_chars,
      too_long = char_count > max_chars,
      suspicious = too_short | too_long | (!has_thai & char_count > 50)
    )
  
  # สรุปผล
  cat("📊 สรุปการ Validate OCR:\n")
  cat("   ทั้งหมด:", nrow(validation_report), "ไฟล์\n")
  cat("   สั้นเกินไป:", sum(validation_report$too_short), "ไฟล์\n")
  cat("   ยาวเกินไป:", sum(validation_report$too_long), "ไฟล์\n")  
  cat("   น่าสงสัย:", sum(validation_report$suspicious), "ไฟล์\n")
  
  # แสดงรายการไฟล์ที่น่าสงสัย
  suspicious_files <- validation_report %>%
    filter(suspicious) %>%
    select(image_file, char_count, answer) %>%
    slice_head(n = 5)
  
  if (nrow(suspicious_files) > 0) {
    cat("\n⚠️  ไฟล์ที่ควรตรวจสอบ:\n")
    for (i in 1:nrow(suspicious_files)) {
      cat("   ", suspicious_files$image_file[i], 
          " (", suspicious_files$char_count[i], " ตัวอักษร)\n")
    }
  }
  
  return(validation_report)
}
```

#### 📚 ตัวอย่างการใช้งานจริง

```r
# ========== ตัวอย่างที่ 1: ข้อสอบคณิตศาสตร์ ==========
math_ocr_workflow <- function() {
  # System prompt เฉพาะสำหรับคณิตศาสตร์
  math_prompt <- "สกัดคำตอบคณิตศาสตร์จากภาพลายมือ ให้ความสำคัญกับ:
  - ตัวเลขและเศษส่วน
  - เครื่องหมายทางคณิตศาสตร์ (+, -, ×, ÷, =, √, ^)
  - สมการและการคำนวณ
  - หน่วยการวัด (ซม., ตร.ม., ลบ.ม.)
  ให้แยกขั้นตอนการทำโจทย์ออกเป็นบรรทัด"
  
  image_files <- list.files("math_exam/", pattern = "\\.jpg$", full.names = TRUE)
  
  results <- ocr_batch_handwriting(
    image_paths = image_files,
    system_prompt = math_prompt,
    model_config = "gpt-4o",  # ใช้โมเดลที่แม่นยำกว่า
    batch_size = 3,
    delay_seconds = 5
  )
  
  return(results)
}

# ========== ตัวอย่างที่ 2: ข้อสอบเรียงความ ==========
essay_ocr_workflow <- function() {
  essay_prompt <- "สกัดเรียงความจากภาพลายมือ ให้ความสำคัญกับ:
  - โครงสร้างย่อหน้า
  - เครื่องหมายวรรคตอน
  - การเชื่อมโยงความคิด
  - ความสมบูรณ์ของข้อความ
  แยกย่อหน้าด้วยการขึ้นบรรทัดใหม่"
  
  essay_files <- list.files("essay_exam/", pattern = "\\.jpg$", full.names = TRUE)
  
  results <- ocr_batch_handwriting(
    image_paths = essay_files,
    system_prompt = essay_prompt,
    max_tokens = 1500,  # เพิ่ม tokens สำหรับเรียงความยาว
    batch_size = 2,
    delay_seconds = 8
  )
  
  return(results)
}

# ========== ตัวอย่างที่ 3: ข้อสอบแบบผสม ==========
mixed_subject_workflow <- function() {
  # ระบุประเภทข้อสอบในชื่อไฟล์
  all_files <- list.files("mixed_exam/", pattern = "\\.jpg$", full.names = TRUE)
  
  results_list <- list()
  
  for (file in all_files) {
    # ระบุประเภทจากชื่อไฟล์
    if (grepl("math", basename(file))) {
      prompt <- create_subject_prompt("math")
      model <- "gpt-4o"
    } else if (grepl("science", basename(file))) {
      prompt <- create_subject_prompt("science")  
      model <- "gpt-4o-mini"
    } else if (grepl("thai", basename(file))) {
      prompt <- create_subject_prompt("thai")
      model <- "gpt-4o-mini"
    } else {
      prompt <- "สกัดคำตอบจากภาพลายมือให้ครบถ้วนและแม่นยำ"
      model <- "gpt-4o-mini"
    }
    
    result <- ocr_handwriting(
      image_path = file,
      system_prompt = prompt,
      model_config = model
    )
    
    results_list[[basename(file)]] <- result
    Sys.sleep(2)  # หน่วงเวลาระหว่างไฟล์
  }
  
  return(bind_rows(results_list))
}
```

### 🏆 สรุป: จาก OCR สู่การตรวจข้อสอบที่สมบูรณ์

ระบบ OCR ใน kruroograder ช่วยให้คุณสามารถ:

1. **แปลงลายมือเป็นข้อความ** ด้วยความแม่นยำสูง
2. **ประมวลผลเป็นชุดใหญ่** ได้อย่างมีประสิทธิภาพ  
3. **ปรับแต่งตามประเภทวิชา** ที่แตกต่างกัน
4. **ตรวจสอบคุณภาพ** และจัดการข้อผิดพลาด
5. **คำนวณต้นทุน** และวางแผนการใช้งาน
6. **เชื่อมต่อกับระบบตรวจข้อสอบ** ได้อย่างไร้รอยต่อ

ทำให้การตรวจข้อสอบลายมือเป็นเรื่องง่ายและรวดเร็วมากขึ้น! 🚀
```
```

### การรวม PDF + OCR + Grading Workflow

ตัวอย่างการใช้งานแบบครบวงจร:

```r
# 1. แยก PDF ตามข้อสอบ
split_result <- split_pdf_by_question("exam_all.pdf", n_questions = 4)

# 2. แปลง PDF เป็นภาพ (ใช้ pdftools หรือ external tools)
# (ขั้นตอนนี้อาจต้องใช้เครื่องมือภายนอก)

# 3. OCR ภาพลายมือ
image_files <- list.files("images/", pattern = "\\.jpg$", full.names = TRUE)
ocr_results <- ocr_batch_handwriting(
  image_paths = image_files,
  system_prompt = "สกัดคำตอบของนักเรียนจากลายมือให้ครบถ้วน"
)

# 4. เตรียมข้อมูลสำหรับการตรวจ
responses_from_ocr <- ocr_results %>%
  mutate(
    student_id = str_extract(image_file, "\\d+"),
    item_id = "item_006",
    response_text = answer
  ) %>%
  select(student_id, item_id, response_text)

# 5. ตรวจข้อสอบ
final_results <- grade_responses(
  responses = responses_from_ocr,
  rubric = rubric,
  key = answer_key,
  .parallel = TRUE
)

# 6. ส่งออกผลลัพธ์
export_results(final_results, "final_grades.xlsx", format = "xlsx")
```

## ⚡ Performance และ Best Practices

### การเลือกใช้ Parallel Processing

```r
# ตัวอย่างการใช้งานที่มีประสิทธิภาพ
n_students <- nrow(responses)

# เลือก system prompt ตามวัตถุประสงค์
assessment_prompt <- "ให้คะแนนอย่างเป็นธรรมและสม่ำเสมอ ตาม rubric ที่กำหนด"

if (n_students < 10) {
  # นักเรียนน้อย: ใช้ sequential
  results <- grade_responses(responses, rubric, key, 
                           system_prompt = assessment_prompt,
                           .parallel = FALSE)
  
} else if (n_students < 100) {
  # นักเรียนปานกลาง: ใช้ parallel แบบจำกัด cores
  results <- grade_responses(responses, rubric, key,
                           system_prompt = assessment_prompt,
                           .parallel = TRUE, .cores = 2)
                           
} else {
  # นักเรียนมาก: ใช้ parallel เต็มที่
  optimal_cores <- get_optimal_cores(n_students, max_cores = 8)
  results <- grade_responses(responses, rubric, key,
                           system_prompt = assessment_prompt,
                           .parallel = TRUE, .cores = optimal_cores)
}
```

### การจัดการ Memory และ Error
```r
# สำหรับข้อมูลขนาดใหญ่
custom_prompt <- "ให้คะแนนอย่างรอบคอบและเป็นธรรม"

results <- tryCatch({
  grade_responses(responses, rubric, key, 
                system_prompt = custom_prompt,
                .parallel = TRUE, .cores = 4)
}, error = function(e) {
  cat("เกิดข้อผิดพลาด ลอง sequential processing\n")
  grade_responses(responses, rubric, key,
                system_prompt = custom_prompt,
                .parallel = FALSE)
})
```

### การเลือก System Prompt ตามวัตถุประสงค์

| วัตถุประสงค์ | System Prompt ที่แนะนำ |
|-------------|----------------------|
| **การสอบกลางภาค/ปลายภาค** | "ให้คะแนนอย่างเข้มงวดตาม rubric เน้นความถูกต้องและครบถ้วน" |
| **การประเมินระหว่างเรียน** | "ให้คะแนนที่เน้นการพัฒนา ให้ feedback สร้างสรรค์" |
| **การประเมินโครงงาน** | "ประเมินความคิดสร้างสรรค์และกระบวนการคิด" |
| **การฝึกหัด** | "ให้คะแนนที่เน้นการเรียนรู้ ชี้จุดดีและจุดที่ต้องปรับปรุง" |
| **การประเมินสำหรับ ESL** | "Use simple English, focus on content over language perfection" |

## 🧪 การทดสอบและพัฒนา

```r
# การทดสอบแพ็กเกจ
devtools::test()

# การตรวจสอบแพ็กเกจ
devtools::check()

# สร้างเอกสาร
devtools::document()

# ทดสอบกับข้อมูลตัวอย่าง
responses_sample <- responses[1:5, ]  # เลือกแค่ 5 คน
test_prompt <- "ทดสอบการให้คะแนน ให้ feedback ที่ละเอียด"
results_test <- grade_responses(responses_sample, rubric, key, 
                              system_prompt = test_prompt)
```

## 📁 โครงสร้างโปรเจกต์

```
kruroograder/
├── R/                          # ซอร์สโค้ดหลัก
│   ├── grading.R              # ฟังก์ชันการตรวจคะแนนหลัก
│   ├── utils.R                # ฟังก์ชันช่วย และ parallel processing
│   ├── 02_load_rubric.R       # การโหลด rubric และ answer key  
│   ├── 03_prompting.R         # การสร้าง prompts สำหรับ LLM
│   └── export.R               # การส่งออกผลลัพธ์
├── inst/                      # ไฟล์ template และตัวอย่าง
│   ├── templates/             # Template สำหรับ prompts
│   ├── rubric/               # ตัวอย่าง rubric files
│   └── extdata/              # ข้อมูลตัวอย่าง
├── tests/                     # การทดสอบ
│   └── testthat/             
├── data/                      # ข้อมูลจริง (สร้างเอง)
│   ├── raw/
│   │   └── responses.csv     # คำตอบนักเรียน
│   └── output/               # ผลลัพธ์
├── rubric/                    # เกณฑ์การให้คะแนน (สร้างเอง)
│   ├── item_006_rubric.md
│   └── item_006_key.md
├── logs/                      # ไฟล์ log (สร้างอัตโนมัติ)
├── DESCRIPTION               # ข้อมูลแพ็กเกจ
├── NAMESPACE                 # Export/Import functions
└── README.md                 # เอกสารนี้
```

## 🔍 การแก้ปัญหาที่พบบ่อย

### ปัญหา API Key
```r
# ตรวจสอบ API key
Sys.getenv("OPENAI_API_KEY")

# ตั้งค่าใน R session
Sys.setenv(OPENAI_API_KEY = "your-key-here")
```

### ปัญหา Memory เมื่อใช้ Parallel
```r
# ลดจำนวน cores
results <- grade_responses(responses, rubric, key, 
                         .parallel = TRUE, .cores = 2)

# หรือแบ่งข้อมูลออกเป็นส่วนย่อย
batch_size <- 50
n_batches <- ceiling(nrow(responses) / batch_size)

for (i in 1:n_batches) {
  start_idx <- (i - 1) * batch_size + 1
  end_idx <- min(i * batch_size, nrow(responses))
  
  batch_responses <- responses[start_idx:end_idx, ]
  batch_results <- grade_responses(batch_responses, rubric, key)
  
  # บันทึกผลลัพธ์แต่ละ batch
  write_csv(batch_results, paste0("output/batch_", i, ".csv"))
}
```

### ปัญหา Model Names
```r
# ใช้ชื่อ model ที่ถูกต้อง
correct_models <- c(
  "gpt-4o-mini",    # ไม่ใช่ "gpt_4o-mini"
  "gpt-4o",
  "gpt-4-turbo",
  "gpt-3.5-turbo"
)

# ตัวอย่างการใช้งาน
results <- grade_responses(
  responses = responses,
  rubric = rubric,
  key = answer_key,
  model_config = "gpt-4o-mini"  # ชื่อที่ถูกต้อง
)
```

### ปัญหา OCR และ PDF
```r
# ตรวจสอบไฟล์ก่อน OCR
if (!file.exists("image.jpg")) {
  stop("ไม่พบไฟล์ภาพ")
}

# ตรวจสอบ API key สำหรับ OCR
if (Sys.getenv("OPENAI_API_KEY") == "") {
  stop("ต้องตั้งค่า OPENAI_API_KEY สำหรับ OCR")
}

# PDF structure ที่ไม่เหมาะสม
inspect_result <- inspect_pdf_structure("exam.pdf", 4)
if (inspect_result$remainder > 0) {
  warning("PDF อาจมีโครงสร้างที่ไม่เหมาะสมสำหรับการแยก")
}
```

### ปัญหา tidyllm
```r
# ตรวจสอบการติดตั้ง tidyllm
tidyllm::chat_test()

# อัปเดต tidyllm
install.packages("tidyllm")
```

### ปัญหา Parallel Processing บน macOS/Linux
```r
# หากมีปัญหากับ multisession
if (.Platform$OS.type == "unix") {
  future::plan(future::multicore)  # ใช้ multicore แทน multisession
} else {
  future::plan(future::multisession)
}
```

## 📄 License และการอ้างอิง

MIT License - ดูรายละเอียดใน [LICENSE](LICENSE) file

### การอ้างอิง
หากคุณใช้ kruroograder ในงานวิจัย กรุณาอ้างอิงดังนี้:

```
Srisuttiyakorn, S. (2024). kruroograder: Automated Essay Grading System Using LLM. 
R package version 0.1.0.
```

## 🤝 การสนับสนุนและติดต่อ

- **Email**: choat.cu@gmail.com
- **Issues**: กรุณารายงานปัญหาผ่าน GitHub Issues  
- **การพัฒนา**: ยินดีรับ Pull Requests และข้อเสนอแนะ

### การพัฒนาต่อ
- [ ] รองรับ multiple items ในครั้งเดียว
- [ ] Web interface สำหรับใช้งานง่าย
- [ ] รองรับ LLM providers เพิ่มเติม
- [ ] ระบบ calibration และ validation
- [ ] Integration กับ Learning Management Systems

---

*พัฒนาโดย Siwachoat Srisuttiyakorn สำหรับการศึกษาที่มีประสิทธิภาพและเป็นธรรม* 🎓

![S__75743284_0 copy](https://github.com/user-attachments/assets/a98be442-add5-44ca-a9f8-81ca63cf2a24)
