library(readxl)
library(dplyr)
library(tidyr)
library(writexl)

result_dir <- "/Users/wonjoon/Workspace/kyuyoung/ongoing/result/rawdata"
output_dir <- "/Users/wonjoon/Workspace/kyuyoung/ongoing/result/desc_stat"
file_list <- list.files(result_dir, pattern = "_rawdata_.*\\.xlsx$", full.names = TRUE)

# 필터 조건
target_industry <- "협회 및 단체, 수리 및 기타 개인 서비스업"     # 예: "제조업", 없으면 "" 또는 NULL
target_size <- 2       # 예: 1~5 (없으면 NULL)
region_filter <- "비광역시"       # 예: "광역시" 또는 "비광역시", 없으면 "" 또는 NULL

# 근로자수 범위 매핑
size_ranges <- list(
  `1` = c(0, 5),
  `2` = c(5, 50),
  `3` = c(50, 300),
  `4` = c(300, 1000),
  `5` = c(1000, Inf)
)

# 데이터 로딩 및 병합
all_data <- lapply(file_list, read_excel) %>% bind_rows()

# 전처리
filtered_data <- all_data %>%
  mutate(
    기준년도 = as.numeric(기준년도),
    측정치 = as.numeric(측정치),
    근로자수 = as.numeric(근로자수)
  ) %>%
  filter(!is.na(기준년도), !is.na(측정치))

# 사용자 필터링
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

# 법 시행 전후 정의 (2019~2021 vs 2022~2024)
filtered_data <- filtered_data %>%
  mutate(
    시행구분 = case_when(
      기준년도 >= 2019 & 기준년도 <= 2021 ~ "시행 전",
      기준년도 >= 2022 & 기준년도 <= 2024 ~ "시행 후",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(시행구분))

# Mann-Whitney U Test
test_result <- wilcox.test(측정치 ~ 시행구분, data = filtered_data)

p_val <- round(test_result$p.value, 4)
decision <- ifelse(p_val < 0.05, "기각", "기각 안함")

# 기술통계 계산
summary_table <- filtered_data %>%
  group_by(시행구분) %>%
  summarise(
    표본수 = n(),
    평균 = round(mean(측정치), 4),
    표준편차 = round(sd(측정치), 4),
    최소값 = round(min(측정치), 4),
    최대값 = round(max(측정치), 4),
    중앙값 = round(median(측정치), 4),
    Q1 = round(quantile(측정치, 0.25), 4),
    Q3 = round(quantile(측정치, 0.75), 4),
    .groups = "drop"
  ) %>%
  mutate(
    IQR = paste0(Q1, " ~ ", Q3),
    `평균(±SD)` = paste0(평균, " ± ", 표준편차),
    p_value = p_val,
    기각여부 = decision
  ) %>%
  dplyr::select(시행구분, 표본수, 평균, 표준편차, 최소값, 최대값,
                중앙값, IQR, `평균(±SD)`, p_value, 기각여부)

# 출력 확인
print(summary_table)

# 엑셀 저장
timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
output_file <- paste0(chemical_target, "_descriptive_statistics_", timestamp, ".xlsx")
write_xlsx(summary_table, path = file.path(output_dir, output_file))
cat("file saved:", output_file, "\n")