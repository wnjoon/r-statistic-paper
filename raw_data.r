library(readxl)
library(dplyr)
library(stringr)
library(writexl)

# 타임스탬프 함수
get_timestamp <- function() {
  format(Sys.time(), "%Y%m%d%H%M%S")
}

# 디렉토리 경로 설정
data_dir <- "/Users/wonjoon/Workspace/kyuyoung/ongoing/files"
result_dir <- "/Users/wonjoon/Workspace/kyuyoung/ongoing/result"
regions <- c("충북", "충남", "제주", "전북", "전남", "인천", "울산", 
             "서울", "부산", "대전", "대구", "광주", "경북", "경남", 
             "경기", "강원")
timestamp <- get_timestamp()

# 분석할 화학물질명
chemical_target <- "소음"

# 단일 파일 처리 함수
process_file <- function(file_path) {
  df_raw <- tryCatch(
    read_excel(file_path, sheet = "화학물질리스트(측정)", col_types = "text"),
    error = function(e) return(NULL)
  )
  if (is.null(df_raw)) return(NULL)

  df <- df_raw %>%
    filter(화학물질명 == chemical_target) %>%
    mutate(
      근로자수 = as.numeric(근로자수),
      상반기_측정치 = as.numeric(`상반기 측정치`),
      하반기_측정치 = as.numeric(`하반기 측정치`)
    )

  df_long <- bind_rows(
    df %>%
      filter(!is.na(`상반기 측정일`)) %>%
      transmute(
        기준년도 = 기준년도,
        관할지사 = `고용부 관할지사`,
        관리번호 = `사업장관리번호`,
        개시번호 = `사업장개시번호`,
        근로자수 = 근로자수,
        업종 = `업종(대)`,
        취급공정명 = 취급공정명,
        측정일자 = as.Date(as.character(`상반기 측정일`), format = "%Y%m%d"),
        측정치 = 상반기_측정치
      ),
    df %>%
      filter(!is.na(`하반기 측정일`)) %>%
      transmute(
        기준년도 = 기준년도,
        관할지사 = `고용부 관할지사`,
        관리번호 = `사업장관리번호`,
        개시번호 = `사업장개시번호`,
        근로자수 = 근로자수,
        업종 = `업종(대)`,
        취급공정명 = 취급공정명,
        측정일자 = as.Date(as.character(`하반기 측정일`), format = "%Y%m%d"),
        측정치 = 하반기_측정치
      )
  )

  return(df_long)
}

# 지역별 반복 처리 및 개별 파일 저장
for (region in regions) {
  cat("\U0001F4C1 processing:", region, "\n")
  region_path <- file.path(data_dir, region)
  file_list <- list.files(path = region_path, pattern = "\\.xlsx$", full.names = TRUE, recursive = TRUE)

  region_data <- lapply(file_list, process_file)
  combined_df <- bind_rows(region_data)

  if (nrow(combined_df) == 0) {
    cat("no data:", region, "\n")
    next
  }

  output_file <- paste0(chemical_target, "_", region, "_rawdata_", timestamp, ".xlsx")
  output_path <- file.path(result_dir, output_file)
  write_xlsx(combined_df, path = output_path)
  cat("> file saved:", output_file, "\n\n")
}