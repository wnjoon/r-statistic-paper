library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)

# 📌 설정값
target_industry <- ""     # 예: "제조업", 없으면 "" 또는 NULL
target_size <- NULL       # 1~5 (없으면 NULL)
region_filter <- ""       # "광역시", "비광역시", 없으면 "" 또는 NULL

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
gam_df <- lapply(file_list, function(file) {
  df <- read_excel(file)
  df %>%
    mutate(
      측정값 = as.numeric(측정치),
      측정일자 = as.Date(측정일자),
      연도 = year(측정일자),
      근로자수 = as.numeric(근로자수)
    ) %>%
    filter(!is.na(측정일자), !is.na(측정값))
}) %>% bind_rows()

# 🔎 연도 필터링: 2019~2024년
gam_df <- gam_df %>%
  filter(연도 >= 2019 & 연도 <= 2024)

# 🎯 사용자 조건 필터링
if (!is.null(target_industry) && target_industry != "") {
  gam_df <- gam_df %>% filter(업종 == target_industry)
}

if (!is.null(target_size)) {
  range <- size_ranges[[as.character(target_size)]]
  gam_df <- gam_df %>%
    filter(근로자수 >= range[1], 근로자수 < range[2])
}

if (!is.null(region_filter) && region_filter != "") {
  if (region_filter == "광역시") {
    gam_df <- gam_df %>%
      filter(str_detect(관할지사, "서울|부산|대구|인천|광주|대전|울산"))
  } else if (region_filter == "비광역시") {
    gam_df <- gam_df %>%
      filter(!str_detect(관할지사, "서울|부산|대구|인천|광주|대전|울산"))
  }
}

# 📅 x축 연도 라벨 설정
year_breaks <- seq(as.Date("2019-01-01"), as.Date("2024-12-31"), by = "1 year")

# 🧠 시각화
ggplot(gam_df, aes(x = 측정일자, y = 측정값)) +
  geom_point(alpha = 0.03, size = 0.5, color = "black") +
  geom_smooth(method = "gam", formula = y ~ s(x), color = "blue", linewidth = 1) +
  geom_vline(xintercept = as.Date("2022-01-01"), linetype = "dashed", color = "red") +
  annotate("text", x = as.Date("2022-02-01"), y = max(gam_df$측정값, na.rm = TRUE) * 0.95,
           label = "2022년 중대재해처벌법 시행", color = "red", hjust = 0, size = 4) +
  scale_x_date(breaks = year_breaks, date_labels = "%Y") +
  labs(
    title = paste0(
      "GAM 시계열 추세 (",
      ifelse(target_industry == "", "전체 업종", target_industry),
      ifelse(!is.null(target_size), paste0(", 규모 ", target_size, "번"), ""),
      ifelse(region_filter == "", "", paste0(", ", region_filter))
    ),
    subtitle = "2019 ~ 2024년 측정값 기준 | 중대재해처벌법 시행 강조",
    x = "측정일자",
    y = "측정값"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11)
  )