# analysis_template.R
# 국민건강영양조사 복합표본 분석 템플릿 (knhanes-analysis 스킬)
# 사용법: 이 파일을 code/NN_<주제>.R 로 복사한 뒤 "설정"과 "TODO" 부분만 고친다.
# 실행:   Rscript --vanilla code/NN_<주제>.R   (프로젝트 폴더에서)
# 원칙:
#   - data 폴더는 읽기만 한다. 결과는 out_dir(output 아래)에만 쓴다.
#   - 분석 대상은 행을 지우지 않고 inc 변수(1/0)로 표시한다.
#   - 추정은 모두 svydesign(가중치·층·집락) → subset(inc == 1) 으로 한다.

suppressPackageStartupMessages({
  library(haven)
  library(data.table)
  library(survey)
})

# ==== 0. 설정 ===============================================================
years   <- c(2016)                                   # TODO: 분석 연도
out_dir <- file.path("output", "NN_topic")           # TODO: 결과 폴더
files   <- file.path("data", "국건영 조사결과",
                     sprintf("hn%02d_all.sas7bdat", years %% 100))

# 여러 연도를 합칠 때의 통합비율 (연도별 wt_itvex에 곱함).
# 값은 지침서의 통합가중치 계산식에서 가져오고 쪽수를 주석으로 남긴다.
#   기수 내 통합: 조사구 수 비례 (제9기 지침서 PDF p.46 / 인쇄 p.38)
#   기수 간 통합: 조사기간 비례 (제9기 지침서 PDF p.47 / 인쇄 p.39)
# 예) pool_ratio <- c("2016" = 1/3, "2017" = 1/3, "2018" = 1/3)  # 근거: ...
pool_ratio <- NULL

# ==== 1. 변수 정의 (지침서에서 확인한 내용만 적는다) ========================
# var    : 원시자료 변수명(소문자)
# type   : cat(범주형) / cont(연속형) / design(설계변수) / id
# levels : 범주형 코드=이름 (세미콜론 구분). 분석에 쓰는 코드만 적는다.
# miss   : 결측으로 바꿀 코드 (모름·무응답: 9, 99, 999 등)
# na8    : 비해당 코드(8, 88, 888 등)와 처리 방법. 지침서 의미를 보고 정한다.
#          "결측" | "범주유지" | "코드=값" (예: "888=0") | "" (해당 없음)
# source : 문서명, PDF 쪽 / 인쇄 쪽
var_dict <- rbindlist(list(
  list(var = "year",     type = "id",     levels = "", miss = "", na8 = "",
       label = "조사연도",                source = "제9기 지침서 PDF p.101 / 인쇄 p.93"),
  list(var = "sex",      type = "cat",    levels = "1=남자;2=여자", miss = "", na8 = "",
       label = "성별",                    source = "제9기 지침서 PDF p.101 / 인쇄 p.93"),
  list(var = "age",      type = "cont",   levels = "", miss = "", na8 = "",
       label = "나이(80=80세이상 탑코딩)", source = "제9기 지침서 PDF p.101-102 / 인쇄 p.93-94"),
  list(var = "psu",      type = "design", levels = "", miss = "", na8 = "",
       label = "조사구번호(집락)",        source = "제9기 지침서 PDF p.101 / 인쇄 p.93"),
  list(var = "kstrata",  type = "design", levels = "", miss = "", na8 = "",
       label = "분산추정 층",             source = "제9기 지침서 PDF p.102 / 인쇄 p.94"),
  list(var = "wt_itvex", type = "design", levels = "", miss = "", na8 = "",
       label = "건강설문-검진조사 가중치", source = "제9기 지침서 PDF p.102 / 인쇄 p.94")
  # TODO: 분석 변수 추가. 예)
  # list(var = "xxx", type = "cat", levels = "1=예;2=아니오", miss = "9", na8 = "8=결측",
  #      label = "...", source = "제7기 지침서 PDF p.__ / 인쇄 p.__")
))

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
fwrite(var_dict, file.path(out_dir, "01_variable_definitions.csv"), bom = TRUE)

# ---- 자료 읽기: 필요한 변수만, 대소문자 무시 ----
read_year <- function(f) {
  nms  <- names(read_sas(f, n_max = 0))
  want <- nms[tolower(nms) %in% var_dict$var]
  miss <- setdiff(var_dict$var, tolower(nms))
  if (length(miss)) message(basename(f), "에 없는 변수: ", paste(miss, collapse = ", "))
  x <- as.data.table(zap_labels(read_sas(f, col_select = all_of(want))))
  setnames(x, tolower(names(x)))
  x
}
d <- rbindlist(lapply(files, read_year), use.names = TRUE, fill = TRUE)
n_raw <- nrow(d)

# ---- 결측·비해당 코드 처리 (var_dict 기준) ----
split_codes <- function(s) if (nzchar(s)) trimws(strsplit(s, ";")[[1]]) else character(0)
for (i in seq_len(nrow(var_dict))) {
  v <- var_dict$var[i]
  if (!v %in% names(d)) next
  mc <- split_codes(var_dict$miss[i])
  if (length(mc)) d[get(v) %in% as.numeric(mc), (v) := NA]
  for (rule in split_codes(var_dict$na8[i])) {
    kv <- strsplit(rule, "=")[[1]]           # "888=0" 또는 "8=결측"
    code <- as.numeric(kv[1]); to <- kv[2]
    if (to == "결측")      d[get(v) == code, (v) := NA]
    else if (to != "범주유지") d[get(v) == code, (v) := as.numeric(to)]
  }
}
# 범주형은 지침서 코드 이름으로 factor 변환 (levels에 없는 코드는 NA가 되므로 미리 확인)
for (i in which(var_dict$type == "cat")) {
  v  <- var_dict$var[i]
  kv <- do.call(rbind, strsplit(split_codes(var_dict$levels[i]), "="))
  unknown <- setdiff(unique(na.omit(d[[v]])), as.numeric(kv[, 1]))
  if (length(unknown)) warning(v, ": levels에 없는 코드 ", paste(unknown, collapse = ","),
                               " → 지침서 확인 후 var_dict 수정")
  d[, (v) := factor(get(v), levels = as.numeric(kv[, 1]), labels = kv[, 2])]
}
stopifnot(nrow(d) == n_raw)   # 코드 처리 중에 행이 줄면 안 된다

# ==== 2. 분석 대상 표시 (행 삭제 없음) ======================================
# 조건을 순서대로 적는다. 흐름표(대상자 선정 과정)가 이 순서로 만들어진다.
criteria <- list(                                   # TODO: 연구 대상 조건
  "가중치 있음(검진·설문 참여)" = quote(!is.na(wt_itvex)),
  "만 19-39세"                   = quote(age >= 19 & age <= 39)
  # "주요 변수 결측 없음" = quote(!is.na(xxx) & !is.na(yyy))
)
d[, inc := 1L]
flow <- data.table(step = "기본DB 전체", n = nrow(d))
for (nm in names(criteria)) {
  ok <- d[, eval(criteria[[nm]])]
  d[is.na(ok) | !ok, inc := 0L]
  flow <- rbind(flow, data.table(step = nm, n = d[inc == 1L, .N]))
}
flow[, excluded := shift(n) - n]
fwrite(flow, file.path(out_dir, "02_participant_flow.csv"), bom = TRUE)
stopifnot(nrow(d) == n_raw)

# ==== 3. 복합표본 설계 지정 → subset ========================================
if (length(years) > 1) {
  if (is.null(pool_ratio)) stop("여러 연도를 합칠 때는 지침서 근거로 pool_ratio를 지정하세요.")
  d[, wt_analysis := wt_itvex * pool_ratio[as.character(year)]]
} else {
  d[, wt_analysis := wt_itvex]
}
# survey 패키지는 NA 가중치를 허용하지 않는다(na_weights="fail").
# 가중치 결측자는 행을 지우는 대신 가중치 0으로 두고, 2단계에서 이미 inc = 0 으로 제외했다.
d[is.na(wt_analysis), wt_analysis := 0]

# 층 안에 집락이 1개뿐이면 분산 추정이 안 된다. 먼저 확인하고, 생기면 처리 방법을 정해 보고서에 적는다.
lonely <- d[wt_analysis > 0, uniqueN(psu), by = kstrata][V1 < 2]
if (nrow(lonely)) warning("집락이 1개인 층: ", paste(lonely$kstrata, collapse = ","),
                          " → options(survey.lonely.psu = ...) 선택 근거를 기록하세요.")

des <- svydesign(ids = ~psu, strata = ~kstrata, weights = ~wt_analysis,
                 nest = TRUE, data = d)
sub <- subset(des, inc == 1)
d_sub <- d[inc == 1]          # 비가중 n 계산용 (추정에는 쓰지 않는다)

# ==== 4. Table 1: 비가중 n + 가중 추정치 ====================================
by_var    <- NULL                                   # TODO: 비교 집단 변수(범주형) 또는 NULL
tab1_vars <- c("sex", "age")                        # TODO: Table 1 변수

fmt <- function(x, digits = 1) formatC(x, format = "f", digits = digits)

table1_block <- function(design, dat, v) {
  if (is.factor(dat[[v]])) {
    lv  <- levels(dat[[v]])
    n   <- as.integer(table(factor(dat[[v]], levels = lv)))
    est <- svymean(as.formula(paste0("~", v)), design, na.rm = TRUE)
    p   <- coef(est)[paste0(v, lv)]
    se  <- SE(est)[paste0(v, lv)]
    data.table(variable = v, level = lv, n = n,
               estimate = paste0(fmt(100 * p), " (", fmt(100 * se), ")"),
               stat = "가중 % (SE)")
  } else {
    est <- svymean(as.formula(paste0("~", v)), design, na.rm = TRUE)
    data.table(variable = v, level = "", n = sum(!is.na(dat[[v]])),
               estimate = paste0(fmt(coef(est)), " (", fmt(SE(est)), ")"),
               stat = "가중 평균 (SE)")
  }
}
table1_col <- function(design, dat, label) {
  out <- rbindlist(lapply(tab1_vars, function(v) table1_block(design, dat, v)))
  setnames(out, c("n", "estimate"), paste0(label, c(": n", ": 추정치")))
  out
}

table1 <- table1_col(sub, d_sub, "전체")
if (!is.null(by_var)) {
  for (g in levels(d_sub[[by_var]])) {
    des_g <- subset(sub, get(by_var) == g)          # 부분집단도 subset으로
    col_g <- table1_col(des_g, d_sub[get(by_var) == g], g)
    table1 <- merge(table1, col_g, by = c("variable", "level", "stat"), sort = FALSE)
  }
}
# 변수 라벨 붙이기
table1 <- merge(var_dict[, .(variable = var, label)], table1, by = "variable", sort = FALSE)
fwrite(table1, file.path(out_dir, "04_table1.csv"), bom = TRUE)
cat("\n[Table 1] 분석 대상 비가중 n =", nrow(d_sub), "\n"); print(table1)

# ==== (본 분석) =============================================================
# TODO: 회귀 등은 반드시 sub(또는 그 subset)에 대해 실행한다. 예)
# fit <- svyglm(y ~ x + sex + age, design = sub, family = quasibinomial())

# ==== 5-1. 공식 통계 대조표 =================================================
# 같은 연도·같은 정의의 공식 통계(국민건강통계, KOSIS 등)를 재현해 본다.
# official 값은 기억으로 채우지 않는다. 보고서·KOSIS에서 찾아 출처(표 번호·쪽)와 함께 적는다.
# 공식 통계가 연령표준화 값이면 조율(crude) 값과 섞지 않는다.
official <- list(                                    # TODO
  # list(indicator = "만19세 이상 OO 유병률(%), 2016",
  #      pop  = quote(age >= 19 & !is.na(wt_itvex)),     # 공식 통계의 대상 정의
  #      ind  = quote(as.numeric(xxx == 1)),              # 지표 정의 (지침서 쪽수 주석)
  #      scale = 100, official = NA_real_, source = "국민건강통계 2016, 표 __, p.__")
)
check <- rbindlist(lapply(official, function(o) {
  dd <- copy(d)
  dd[, `:=`(pop_flag = as.integer(eval(o$pop) %in% TRUE), ind_val = eval(o$ind))]
  dz <- svydesign(ids = ~psu, strata = ~kstrata, weights = ~wt_analysis, nest = TRUE, data = dd)
  dz <- subset(dz, pop_flag == 1)
  est <- svymean(~ind_val, dz, na.rm = TRUE)
  ci  <- confint(est)
  data.table(indicator = o$indicator,
             n_unweighted = dd[pop_flag == 1 & !is.na(ind_val), .N],
             estimate = o$scale * unname(coef(est)), se = o$scale * as.numeric(SE(est)),
             ci_low = o$scale * unname(ci[1]), ci_high = o$scale * unname(ci[2]),
             official = o$official, source = o$source)
}))
if (nrow(check)) {
  check[, diff := estimate - official]
  check[, within_ci := official >= ci_low & official <= ci_high]
  print(check)
  if (any(is.na(check$official))) warning("공식 통계값이 비어 있습니다. 출처를 찾아 채우세요.")
  if (any(check$within_ci %in% FALSE)) warning("공식 통계가 95% CI 밖입니다. 정의·가중치·대상을 다시 확인하세요.")
} else {
  warning("공식 통계 대조 항목(official)이 비어 있습니다.")
}
fwrite(check, file.path(out_dir, "05_official_check.csv"), bom = TRUE)

# ==== 5-2. 재현성 기록 ======================================================
# 입력 파일 지문과 실행 환경을 남긴다. 재실행 비교는 check_reproducibility.R 로 한다.
inputs <- data.table(file = files, size = file.size(files),
                     md5 = unname(tools::md5sum(files)))
fwrite(inputs, file.path(out_dir, "_inputs.csv"), bom = TRUE)
writeLines(capture.output(sessionInfo()), file.path(out_dir, "_sessionInfo.txt"))
cat("\n완료:", out_dir, "\n")
