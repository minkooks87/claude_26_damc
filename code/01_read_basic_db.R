# 01_read_basic_db.R
# 목적: data 폴더의 국민건강영양조사 기본DB(hnXX_all.sas7bdat)를 읽어
#       파일별 행·변수 수, year·sex·age 분포, 설계변수(wt_itvex, kstrata, psu) 유무와 결측 수를 확인한다.
# 주의: 원시자료는 읽기만 한다. 결과는 output 폴더에만 저장한다.
# 분석(가중 추정)은 하지 않는다. 여기서 세는 값은 모두 가중치를 적용하지 않은 표본 수(n)다.

suppressPackageStartupMessages({
  library(haven)
  library(data.table)
})

data_dir <- file.path("data", "국건영 조사결과")
out_dir  <- file.path("output", "01_basic_db_overview")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# 기본DB 파일 목록 (hn07_all ~ hn24_all)
files <- sort(list.files(data_dir, pattern = "^hn[0-9]{2}_all\\.sas7bdat$",
                         ignore.case = TRUE, full.names = TRUE))

key_vars    <- c("year", "sex", "age")
design_vars <- c("wt_itvex", "kstrata", "psu")

file_info <- list()
design_na <- list()
year_tab  <- list()
sex_tab   <- list()
age_tab   <- list()
var_names <- list()

for (f in files) {
  fname <- basename(f)
  cat("읽는 중:", fname, "\n")

  # 1) 변수 이름만 먼저 읽는다 (행은 읽지 않음)
  hdr  <- read_sas(f, n_max = 0)
  nms  <- names(hdr)
  var_names[[fname]] <- tolower(nms)

  # 필요한 변수만 원래 대소문자 그대로 골라서 읽는다
  want <- nms[tolower(nms) %in% c(key_vars, design_vars)]
  d <- as.data.table(zap_labels(read_sas(f, col_select = all_of(want))))
  setnames(d, tolower(names(d)))

  file_info[[fname]] <- data.table(file = fname, n_rows = nrow(d), n_vars = length(nms))

  # 2) 설계변수 유무와 결측(NA) 수
  design_na[[fname]] <- rbindlist(lapply(design_vars, function(v) {
    has <- v %in% names(d)
    data.table(file = fname, variable = v, exists = has,
               n_missing = if (has) sum(is.na(d[[v]])) else NA_integer_)
  }))

  # 3) year, sex, age 분포 (코드값 그대로 센다)
  if ("year" %in% names(d)) year_tab[[fname]] <- d[, .N, by = year][, file := fname]
  if ("sex"  %in% names(d)) sex_tab[[fname]]  <- d[, .N, by = sex][, file := fname]
  if ("age"  %in% names(d)) {
    age_tab[[fname]] <- d[, .(file = fname, n = .N, n_na = sum(is.na(age)),
                              min = min(age, na.rm = TRUE),
                              q1 = quantile(age, .25, na.rm = TRUE),
                              median = median(age, na.rm = TRUE),
                              mean = mean(age, na.rm = TRUE),
                              q3 = quantile(age, .75, na.rm = TRUE),
                              max = max(age, na.rm = TRUE),
                              n_19_39 = sum(age >= 19 & age <= 39, na.rm = TRUE))]
  }
  # 전체 연령대 분포용으로 age만 따로 모아 둔다
  age_tab[[paste0(fname, "_raw")]] <- d[, .(year, age)]
  rm(d, hdr); gc(verbose = FALSE)
}

file_info <- rbindlist(file_info)
design_na <- rbindlist(design_na)
year_tab  <- rbindlist(year_tab)
sex_tab   <- rbindlist(sex_tab)
age_raw   <- rbindlist(age_tab[grepl("_raw$", names(age_tab))])
age_sum   <- rbindlist(age_tab[!grepl("_raw$", names(age_tab))])

# 연령대(10세 간격) × 연도 표
age_raw[, age_group := cut(age, breaks = c(-Inf, 9, 18, 29, 39, 49, 59, 69, 79, Inf),
                           labels = c("0-9", "10-18", "19-29", "30-39", "40-49",
                                      "50-59", "60-69", "70-79", "80+"))]
age_group_tab <- dcast(age_raw[, .N, by = .(year, age_group)], year ~ age_group,
                       value.var = "N", fill = 0)

# 연도별 성별 표 (sex 코드값 그대로)
sex_wide <- dcast(sex_tab, file ~ sex, value.var = "N", fill = 0)

# 변수 개수: 전체 연도 합집합과 공통 변수
all_vars    <- Reduce(union, var_names)
common_vars <- Reduce(intersect, var_names)

# 저장
fwrite(file_info,     file.path(out_dir, "file_rows_vars.csv"), bom = TRUE)
fwrite(design_na,     file.path(out_dir, "design_vars_missing.csv"), bom = TRUE)
fwrite(year_tab,      file.path(out_dir, "year_by_file.csv"), bom = TRUE)
fwrite(sex_wide,      file.path(out_dir, "sex_by_file.csv"), bom = TRUE)
fwrite(age_sum,       file.path(out_dir, "age_summary_by_file.csv"), bom = TRUE)
fwrite(age_group_tab, file.path(out_dir, "age_group_by_year.csv"), bom = TRUE)

# 콘솔 출력
options(width = 200)
cat("\n=== 1. 파일별 행·변수 수 ===\n"); print(file_info)
cat("전체 행 합계:", sum(file_info$n_rows), "\n")
cat("변수 합집합:", length(all_vars), " / 전 연도 공통 변수:", length(common_vars), "\n")
cat("\n=== 2-1. year 값 (파일별) ===\n"); print(year_tab)
cat("\n=== 2-2. sex 코드별 n ===\n"); print(sex_wide)
cat("\n=== 2-3. age 요약 ===\n"); print(age_sum, digits = 3)
cat("\n=== 2-4. 연령대 × year ===\n"); print(age_group_tab)
cat("\n=== 3. 설계변수 유무·결측 ===\n"); print(dcast(design_na, file ~ variable, value.var = "n_missing"))
cat("설계변수가 없는 파일:\n"); print(design_na[exists == FALSE])
cat("\n사용 환경:", R.version.string, "/ haven", as.character(packageVersion("haven")),
    "/ data.table", as.character(packageVersion("data.table")), "\n")
