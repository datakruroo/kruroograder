
![S__75743284_0 copy](https://github.com/user-attachments/assets/356d3ab2-08ab-4f8e-b8c0-9bd43576f7d9) <img width="300" height="134" alt="KruRoo_logo copy" src="https://github.com/user-attachments/assets/0673db68-5773-46b3-be75-fedeb827fbdb" />



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
# devtools::install_github("datakruroo/kruroograder")
```

### ตั้งค่า API
```r
# สำหรับ OpenAI
Sys.setenv(OPENAI_API_KEY = "your-api-key-here")

# หรือใส่ใน .Renviron
# OPENAI_API_KEY=your-api-key-here
```
หากยังไม่มี OPENAI API ให้เข้าไปที่ [https://openai.com/](https://openai.com/)

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
  provider = "openai",        # ผู้ให้บริการ
  temperature = 0.1           # ความสร้างสรรค์ (0-1)
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

| ฟังก์ชัน | คำอธิบาย | ตัวอย่างการใช้ |
|---------|----------|----------------|
| `load_rubric()` | โหลดเกณฑ์การให้คะแนน | `rubric <- load_rubric("rubric/item_001.md")` |
| `load_answer_key()` | โหลดเฉลยและแนวคำตอบ | `key <- load_answer_key("rubric/item_001_key.md")` |
| `grade_responses()` | ตรวจและให้คะแนน (หลัก) | `results <- grade_responses(responses, rubric, key, .parallel = TRUE)` |
| `should_use_parallel()` | ตรวจสอบว่าควรใช้ parallel | `if(should_use_parallel(n)) { ... }` |
| `get_optimal_cores()` | หาจำนวน cores ที่เหมาะสม | `cores <- get_optimal_cores(nrow(responses))` |
| `export_results()` | ส่งออกผลลัพธ์ | `export_results(results, "output.csv")` |
| `create_grading_report()` | สร้างรายงานสรุป | `create_grading_report(results, "report.md")` |
| `validate_response_data()` | ตรวจสอบข้อมูล input | `validate_response_data(responses)` |

## ⚡ Performance และ Best Practices

### การเลือกใช้ Parallel Processing

```r
# ตัวอย่างการใช้งานที่มีประสิทธิภาพ
n_students <- nrow(responses)

if (n_students < 10) {
  # นักเรียนน้อย: ใช้ sequential
  results <- grade_responses(responses, rubric, key, .parallel = FALSE)
  
} else if (n_students < 100) {
  # นักเรียนปานกลาง: ใช้ parallel แบบจำกัด cores
  results <- grade_responses(responses, rubric, key, 
                           .parallel = TRUE, .cores = 2)
                           
} else {
  # นักเรียนมาก: ใช้ parallel เต็มที่
  optimal_cores <- get_optimal_cores(n_students, max_cores = 8)
  results <- grade_responses(responses, rubric, key,
                           .parallel = TRUE, .cores = optimal_cores)
}
```

### การจัดการ Memory และ Error
```r
# สำหรับข้อมูลขนาดใหญ่
results <- tryCatch({
  grade_responses(responses, rubric, key, .parallel = TRUE, .cores = 4)
}, error = function(e) {
  cat("เกิดข้อผิดพลาด ลอง sequential processing\n")
  grade_responses(responses, rubric, key, .parallel = FALSE)
})
```

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
results_test <- grade_responses(responses_sample, rubric, key)
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

### ปัญหา tidyllm
```r
# ตรวจสอบการติดตั้ง tidyllm
tidyllm::chat_test()

# อัปเดต tidyllm
install.packages("tidyllm")
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
