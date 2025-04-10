library(readxl)
library(dplyr)
library(tidyr)
library(writexl)
library(ggplot2)
library(stringr)
library(lubridate)

# 📁 디렉토리 설정
result_dir <- "/Users/wonjoon/Workspace/kyuyoung/ongoing/result/rawdata"
output_dir <- "/Users/wonjoon/Workspace/kyuyoung/ongoing/result/desc_stat"
file_list <- list.files(result_dir, pattern = "_rawdata_.*\\.xlsx$", full.names = TRUE)

# 🔧 필터 조건 설정
target_industry <- "제조업"
target_size <- NULL
region_filter <- "비광역시"

# 👥 근로자수 범위 및 라벨
target_size_ranges <- list(
  `1` = c(0, 5),
  `2` = c(5, 50),
  `3` = c(50, 300),
  `4` = c(300, 1000),
  `5` = c(1000, Inf)
)
target_size_labels <- list(
  `1` = "5인 미만",
  `2` = "5인 이상 50인 미만",
  `3` = "50인 이상 300인 미만",
  `4` = "300인 이상 1000인 미만",
  `5` = "1000인 이상"
)

# 📥 데이터 로딩
all_data <- lapply(file_list, read_excel) %>% bind_rows()

# 🧹 전처리
filtered_data <- all_data %>%
  mutate(
    기준년도 = as.numeric(기준년도),
    측정치 = as.numeric(측정치),
    근로자수 = as.numeric(근로자수)
  ) %>%
  filter(!is.na(기준년도), !is.na(측정치))

# 🎯 필터링 적용
if (!is.null(target_industry) && target_industry != "") {
  filtered_data <- filtered_data %>% filter(업종 == target_industry)
}
if (!is.null(target_size)) {
  range <- target_size_ranges[[as.character(target_size)]]
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

# ⚖️ 법 시행 전후 구분
filtered_data <- filtered_data %>%
  mutate(
    시행구분 = case_when(
      기준년도 >= 2019 & 기준년도 <= 2021 ~ "시행 전",
      기준년도 >= 2022 & 기준년도 <= 2024 ~ "시행 후",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(시행구분))

# 🧪 Mann-Whitney U Test
test_result <- wilcox.test(측정치 ~ 시행구분, data = filtered_data)
p_val <- round(test_result$p.value, 4)
decision <- ifelse(p_val < 0.05, "기각", "기각 안함")

# 📊 기술통계 계산
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
  dplyr::select(
    시행구분, 표본수, 평균, 표준편차, 최소값, 최대값,
    중앙값, IQR, `평균(±SD)`, p_value, 기각여부
  )

# 💾 기술통계 저장
timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
output_file <- paste0("기술통계_", timestamp, ".xlsx")
write_xlsx(summary_table, path = file.path(output_dir, output_file))
cat("✅ 기술통계 저장 완료:", output_file, "\n")

# 📌 동적 타이틀 생성
title_parts <- c()
if (target_industry != "") title_parts <- c(title_parts, paste0("업종: ", target_industry))
if (!is.null(target_size)) title_parts <- c(title_parts, paste0("규모: ", target_size_labels[[as.character(target_size)]]))
if (region_filter != "") title_parts <- c(title_parts, paste0("지역: ", region_filter))
subtitle_text <- paste(title_parts, collapse = ", ")

# 🎨 바이올린 + 박스 플롯
plot <- ggplot(filtered_data, aes(x = 시행구분, y = 측정치, fill = 시행구분)) +
  geom_violin(trim = FALSE, alpha = 0.4, color = NA) +
  geom_boxplot(width = 0.12, outlier.size = 0.8, color = "black", alpha = 0.6) +
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "black", fatten = 2) +
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 4)),
               vjust = 1.5, size = 4, fontface = "bold", color = "black") +
  labs(
    title = "법 시행 전후 측정치 분포 비교",
    subtitle = subtitle_text,
    x = "법 시행 구분",
    y = "측정치"
  ) +
  scale_fill_manual(values = c("시행 전" = "skyblue", "시행 후" = "salmon")) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    legend.position = "none"
  )

print(plot)