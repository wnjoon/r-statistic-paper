library(readxl)
library(dplyr)
library(writexl)

# 📁 원시자료 디렉토리 경로
result_dir <- "/Users/wonjoon/Workspace/kyuyoung/ongoing/result/rawdata"
output_dir <- "/Users/wonjoon/Workspace/kyuyoung/ongoing/result/industry_count"

# 🔍 해당 폴더 내 모든 _rawdata_ 파일 목록 가져오기
file_list <- list.files(result_dir, pattern = "_rawdata_.*\\.xlsx$", full.names = TRUE)

# 📦 데이터 병합
all_data <- lapply(file_list, read_excel) %>% bind_rows()

# 📊 업종별 개수 집계 및 내림차순 정렬
industry_summary <- all_data %>%
  group_by(업종) %>%
  summarise(항목수 = n(), .groups = "drop") %>%
  arrange(desc(항목수))  # 내림차순 정렬

# 💾 엑셀로 저장
timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
output_file <- paste0("업종별_항목수_정리_", timestamp, ".xlsx")
write_xlsx(industry_summary, path = file.path(output_dir, output_file))

cat("✅ 엑셀 저장 완료:", output_file, "\n")