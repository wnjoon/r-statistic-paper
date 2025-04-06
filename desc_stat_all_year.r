library(readxl)
library(dplyr)
library(tidyr)
library(writexl)
library(stringr)

# 📁 디렉토리 설정
result_dir <- "/Users/wonjoon/Workspace/kyuyoung/ongoing/result/rawdata"
output_dir <- "/Users/wonjoon/Workspace/kyuyoung/ongoing/result/desc_stat"
file_list <- list.files(result_dir, pattern = "_rawdata_.*\\.xlsx$", full.names = TRUE)

# 📌 필터 조건
target_industry <- ""     # 예: "제조업", 없으면 "" 또는 NULL
target_size <- NULL       # 예: 1~5 (없으면 NULL)
region_filter <- ""       # 예: "광역시", "비광역시", 없으면 "" 또는 NULL

# 📊 근로자수 범위 매핑
size_ranges <- list(
  `1` = c(0, 5),
  `2` = c(5, 50),
  `3` = c(50, 300),
  `4` = c(300, 1000),
  `5` = c(1000, Inf)
)

# 📥 데이터 로딩 및 병합
all_data <- lapply(file_list, read_excel) %>% bind_rows()

# 🧹 전처리
filtered_data <- all_data %>%
  mutate(
    기준년도 = as.numeric(기준년도),
    측정치 = as.numeric(측정치),
    근로자수 = as.numeric(근로자수)
  ) %>%
  filter(!is.na(기준년도), !is.na(측정치))

# 🧪 사용자 필터링
if (!is.null(target_industry) && target_industry != "") {
  filtered_data <- filtered_data %>% filter(업종 == target_industry)
}

if (!is.null(target_size)) {
  range <- size_ranges[[as.character(target_size)]]
  filtered_data <- filtered_data %>%
    filter(근로자수 >= range[1], 근로자수 < range[2])
}

if (!is.null(region_filter) && region_filter != "") {
  if (region_filter == "광역시") {
    filtered_data <- filtered_data %>%
      filter(str_detect(관할지사, "서울|부산|대구|인천|광주|대전|울산"))
  } else if (region_filter == "비광역시") {
    filtered_data <- filtered_data %>%
      filter(!str_detect(관할지사, "서울|부산|대구|인천|광주|대전|울산"))
  }
}

# 📅 연도 필터링
filtered_data <- filtered_data %>%
  filter(기준년도 >= 2019, 기준년도 <= 2024)

# 📈 연도별 기술통계 계산
summary_by_year <- filtered_data %>%
  group_by(기준년도) %>%
  summarise(
    표본수 = n(),
    평균 = round(mean(측정치), 4),
    표준편차 = round(sd(측정치), 4),
    중앙값 = round(median(측정치), 4),
    최소값 = round(min(측정치), 4),
    최대값 = round(max(측정치), 4),
    Q1 = round(quantile(측정치, 0.25), 4),
    Q3 = round(quantile(측정치, 0.75), 4),
    .groups = "drop"
  ) %>%
  mutate(
    IQR = paste0(Q1, " ~ ", Q3),
    `평균(±SD)` = paste0(평균, " ± ", 표준편차)
  ) %>%
  dplyr::select(기준년도, 표본수, 평균, 표준편차, 중앙값, 최소값, 최대값, IQR, `평균(±SD)`)

# 🖨️ 출력 확인
print(summary_by_year)

# 💾 엑셀 저장
timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
output_file <- paste0("연도별_기술통계_", timestamp, ".xlsx")
write_xlsx(summary_by_year, path = file.path(output_dir, output_file))
cat("✅ 파일 저장 완료:", output_file, "\n")