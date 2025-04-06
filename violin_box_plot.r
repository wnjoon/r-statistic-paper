library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)

# 📌 설정값
target_industry <- "제조업"     # 예: "제조업", 없으면 "" 또는 NULL
target_size <- NULL      # 1~5 (없으면 NULL)
region_filter <- "광역시"       # "광역시", "비광역시", 없으면 "" 또는 NULL

# 근로자수 범위 매핑
size_ranges <- list(
  `1` = c(0, 5),
  `2` = c(5, 50),
  `3` = c(50, 300),
  `4` = c(300, 1000),
  `5` = c(1000, Inf)
)

# 📂 데이터 경로
result_dir <- "/Users/wonjoon/Workspace/kyuyoung/ongoing/result/rawdata"
file_list <- list.files(path = result_dir, pattern = "_rawdata_.*\\.xlsx$", full.names = TRUE)

# 📊 데이터 불러오기 및 병합
all_data <- lapply(file_list, read_excel) %>% bind_rows()

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

# 시행구분 생성
filtered_data <- filtered_data %>%
  mutate(
    시행구분 = case_when(
      기준년도 >= 2019 & 기준년도 <= 2021 ~ "시행 전",
      기준년도 >= 2022 & 기준년도 <= 2024 ~ "시행 후",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(시행구분))

size_labels <- list(
  `1` = "5인 미만",
  `2` = "5인 이상 50인 미만",
  `3` = "50인 이상 300인 미만",
  `4` = "300인 이상 1000인 미만",
  `5` = "1000인 이상"
)

# 🎨 바이올린 + 박스 플롯
ggplot(filtered_data, aes(x = 시행구분, y = 측정치, fill = 시행구분)) +
  geom_violin(trim = FALSE, alpha = 0.4, color = NA) +
  geom_boxplot(width = 0.12, outlier.size = 0.8, color = "black", alpha = 0.6) +
  stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "black", fatten = 2) +
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 4)),
               vjust = 1.5, size = 4, fontface = "bold", color = "black") +
  labs(
    title = "시행 전후 측정치 분포 비교",
    subtitle = paste0(
      ifelse(target_industry != "", paste0("업종: ", target_industry, ", "), ""),
      ifelse(!is.null(target_size), paste0("규모: ", size_labels[[as.character(target_size)]], ", "), ""),
      ifelse(region_filter != "", paste0("지역: ", region_filter), "")
    ),
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