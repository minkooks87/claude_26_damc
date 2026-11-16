# 01_table1_young_adults_2016.R
# 목적: 2016년 국민건강영양조사(제7기 1차년도) 만 19-39세 청년의 성별 Table 1
#       변수: 나이(age), 소득 4분위(개인, incm), 교육수준(edu), 체질량지수(HE_BMI)
# 근거: knhanes-analysis 스킬의 analysis_template.R 을 복사해 수정
# 실행: 프로젝트 폴더에서
#   Rscript --vanilla .claude/skills/knhanes-analysis-workspace/iteration-1/table1-young-adults-2016/with_skill/outputs/01_table1_young_adults_2016.R
# 원칙:
#   - data 폴더는 읽기만 한다. 결과는 out_dir 에만 쓴다.
#   - 분석 대상은 행을 지우지 않고 inc 변수(1/0)로 표시한다.
#   - 추정은 모두 svydesign(가중치·층·집락) → subset(inc == 1) 으로 한다.

suppressPackageStartupMessages({
  library(haven)
  library(data.table)
  library(survey)
})

# ==== 0. 설정 ===============================================================
years   <- c(2016)
out_dir <- file.path(".claude", "skills", "knhanes-analysis-workspace", "iteration-1",
                     "table1-young-adults-2016", "with_skill", "outputs", "table1_2016")
files   <- file.path("data", "국건영 조사결과",
                     sprintf("hn%02d_all.sas7bdat", years %% 100))
pool_ratio <- NULL   # 단일 연도이므로 통합가중치 불필요

# ==== 1. 변수 정의 (제7기 지침서에서 확인한 내용) ===========================
G7 <- "제7기(2016-2018) 원시자료 이용지침서"
var_dict <- rbindlist(list(
  list(var = "year",     type = "id",     levels = "", miss = "", na8 = "",
       label = "조사연도",                 source = paste(G7, "PDF p.93 / 인쇄 p.85")),
  list(var = "sex",      type = "cat",    levels = "1=남자;2=여자", miss = "", na8 = "",
       label = "성별",                     source = paste(G7, "PDF p.93 / 인쇄 p.85")),
  list(var = "age",      type = "cont",   levels = "", miss = "", na8 = "",
       label = "나이(세)",                 source = paste(G7, "PDF p.93 / 인쇄 p.85 (80=80세이상 탑코딩, 각주 1 PDF p.94)")),
  list(var = "incm",     type = "cat",    levels = "1=하;2=중하;3=중상;4=상", miss = "9", na8 = "",
       label = "소득 4분위수(개인, 월평균 가구균등화소득의 성·연령별 4분위)",
       source = paste(G7, "PDF p.93 / 인쇄 p.85; 기준금액 PDF p.95 / 인쇄 p.87")),
  list(var = "edu",      type = "cat",    levels = "1=초졸이하;2=중졸;3=고졸;4=대졸이상", miss = "9", na8 = "",
       label = "교육수준 재분류(졸업=현 학력, 수료·중퇴·재학·휴학=이전 학력)",
       source = paste(G7, "PDF p.94 / 인쇄 p.86")),
  list(var = "he_bmi",   type = "cont",   levels = "", miss = "", na8 = "",
       label = "체질량지수(kg/m2), 임신부 제외",
       source = paste(G7, "PDF p.197 / 인쇄 p.189 (생성변수); 산출 프로그램 PDF p.49 / 인쇄 p.41 (HE_dprg=. 인 경우만 산출)")),
  list(var = "he_prg",   type = "aux",    levels = "0=아니오;1=예", miss = "9", na8 = "8=결측",
       label = "임신여부(8=비해당(남자))", source = paste(G7, "PDF p.192 / 인쇄 p.184")),
  list(var = "he_dprg",  type = "aux",    levels = "", miss = "", na8 = "",
       label = "임신개월수",               source = paste(G7, "PDF p.192 / 인쇄 p.184")),
  list(var = "psu",      type = "design", levels = "", miss = "", na8 = "",
       label = "조사구번호(집락)",         source = paste(G7, "PDF p.93 / 인쇄 p.85; 설계 설명 PDF p.39 / 인쇄 p.31")),
  list(var = "kstrata",  type = "design", levels = "", miss = "", na8 = "",
       label = "분산추정 층",              source = paste(G7, "PDF p.94 / 인쇄 p.86; 설계 설명 PDF p.39 / 인쇄 p.31")),
  list(var = "wt_itvex", type = "design", levels = "", miss = "", na8 = "",
       label = "건강설문-검진조사 가중치", source = paste(G7, "PDF p.94 / 인쇄 p.86; 분석 예시 PDF p.40 / 인쇄 p.32"))
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
    kv <- strsplit(rule, "=")[[1]]
    code <- as.numeric(kv[1]); to <- kv[2]
    if (to == "결측")      d[get(v) == code, (v) := NA]
    else if (to != "범주유지") d[get(v) == code, (v) := as.numeric(to)]
  }
}

# ---- 임신부의 BMI는 결측 처리 ----
# 지침서 PDF p.49 / 인쇄 p.41 비만 지표 프로그램: "IF HE_dprg=. & HE_wt^=. & HE_ht^=. THEN HE_BMI = ..."
# 및 PDF p.197 / 인쇄 p.189 HE_obe 정의 "(비임신자 중)". 2016 원자료에는 임신부도 HE_BMI 값이 있어 따로 지운다.
n_preg_bmi <- d[(he_prg %in% 1 | !is.na(he_dprg)) & !is.na(he_bmi), .N]
d[he_prg %in% 1 | !is.na(he_dprg), he_bmi := NA]

# 범주형은 지침서 코드 이름으로 factor 변환
for (i in which(var_dict$type == "cat")) {
  v  <- var_dict$var[i]
  kv <- do.call(rbind, strsplit(split_codes(var_dict$levels[i]), "="))
  unknown <- setdiff(unique(na.omit(d[[v]])), as.numeric(kv[, 1]))
  if (length(unknown)) warning(v, ": levels에 없는 코드 ", paste(unknown, collapse = ","),
                               " → 지침서 확인 후 var_dict 수정")
  d[, (v) := factor(get(v), levels = as.numeric(kv[, 1]), labels = kv[, 2])]
}
stopifnot(nrow(d) == n_raw)

# ==== 2. 분석 대상 표시 (행 삭제 없음) ======================================
criteria <- list(
  "가중치 있음(검진·설문 참여)" = quote(!is.na(wt_itvex)),
  "만 19-39세"                   = quote(age >= 19 & age <= 39)
)
d[, inc := 1L]
flow <- data.table(step = "2016 기본DB(hn16_all) 전체", n = nrow(d))
for (nm in names(criteria)) {
  ok <- d[, eval(criteria[[nm]])]
  d[is.na(ok) | !ok, inc := 0L]
  flow <- rbind(flow, data.table(step = nm, n = d[inc == 1L, .N]))
}
flow[, excluded := shift(n) - n]
# 참고 행: 분석 대상 중 성별 n, 변수별 결측, 임신부 BMI 결측 처리
flow_note <- data.table(
  step = c("  └ 남자", "  └ 여자",
           "  (참고) 대상 중 incm 결측", "  (참고) 대상 중 edu 결측",
           "  (참고) 대상 중 HE_BMI 결측(임신부 포함)", "  (참고) 대상 중 임신부(BMI 결측 처리)"),
  n = c(d[inc == 1 & sex == "남자", .N], d[inc == 1 & sex == "여자", .N],
        d[inc == 1 & is.na(incm), .N], d[inc == 1 & is.na(edu), .N],
        d[inc == 1 & is.na(he_bmi), .N], d[inc == 1 & (he_prg %in% 1 | !is.na(he_dprg)), .N]),
  excluded = NA_integer_)
flow <- rbind(flow, flow_note)
fwrite(flow, file.path(out_dir, "02_participant_flow.csv"), bom = TRUE)
stopifnot(nrow(d) == n_raw)

# ==== 3. 복합표본 설계 지정 → subset ========================================
d[, wt_analysis := wt_itvex]
d[is.na(wt_analysis), wt_analysis := 0]   # 가중치 결측자는 inc = 0 이고, survey가 NA 가중치를 거부하므로 0

lonely <- d[wt_analysis > 0, uniqueN(psu), by = kstrata][V1 < 2]
if (nrow(lonely)) warning("집락이 1개인 층: ", paste(lonely$kstrata, collapse = ","),
                          " → options(survey.lonely.psu = ...) 선택 근거를 기록하세요.")

des <- svydesign(ids = ~psu, strata = ~kstrata, weights = ~wt_analysis,
                 nest = TRUE, data = d)
sub <- subset(des, inc == 1)
d_sub <- d[inc == 1]          # 비가중 n 계산용 (추정에는 쓰지 않는다)

# ==== 4. Table 1: 비가중 n + 가중 추정치 ====================================
by_var    <- "sex"
tab1_vars <- c("age", "incm", "edu", "he_bmi")

fmt <- function(x, digits = 1) formatC(x, format = "f", digits = digits)

table1_block <- function(design, dat, v) {
  n_miss <- sum(is.na(dat[[v]]))
  if (is.factor(dat[[v]])) {
    lv  <- levels(dat[[v]])
    n   <- as.integer(table(factor(dat[[v]], levels = lv)))
    est <- svymean(as.formula(paste0("~", v)), design, na.rm = TRUE)
    p   <- coef(est)[paste0(v, lv)]
    se  <- SE(est)[paste0(v, lv)]
    out <- data.table(variable = v, level = lv, n = n,
                      estimate = paste0(fmt(100 * p), " (", fmt(100 * se), ")"),
                      stat = "가중 % (SE)")
  } else {
    est <- svymean(as.formula(paste0("~", v)), design, na.rm = TRUE)
    out <- data.table(variable = v, level = "", n = sum(!is.na(dat[[v]])),
                      estimate = paste0(fmt(coef(est)), " (", fmt(SE(est)), ")"),
                      stat = "가중 평균 (SE)")
  }
  # 결측 n(비가중)을 따로 표시한다. %·평균은 결측을 뺀 응답자 기준이다.
  rbind(out, data.table(variable = v, level = "결측", n = n_miss, estimate = "-",
                        stat = "비가중 n만"))
}
table1_col <- function(design, dat, label) {
  out <- rbindlist(lapply(tab1_vars, function(v) table1_block(design, dat, v)))
  setnames(out, c("n", "estimate"), paste0(label, c(": n", ": 추정치")))
  out
}

table1 <- table1_col(sub, d_sub, "전체")
for (g in levels(d_sub[[by_var]])) {
  des_g <- subset(sub, get(by_var) == g)          # 부분집단도 subset으로
  col_g <- table1_col(des_g, d_sub[get(by_var) == g], g)
  table1 <- merge(table1, col_g, by = c("variable", "level", "stat"), sort = FALSE)
}

# ---- 성별 차이 검정 (복합표본) ----
# 범주형: Rao-Scott 보정 카이제곱(svychisq 기본, F 근사)
# 연속형: svyglm(v ~ sex) 의 성별 계수 Wald t 검정 (가중 평균 차이 검정)
pval <- rbindlist(lapply(tab1_vars, function(v) {
  dz <- subset(sub, !is.na(get(v)))
  if (is.factor(d_sub[[v]])) {
    p <- svychisq(as.formula(paste0("~", v, "+", by_var)), dz)$p.value
    m <- "Rao-Scott 카이제곱"
  } else {
    fit <- svyglm(as.formula(paste0(v, "~", by_var)), design = dz)
    p <- summary(fit)$coefficients[2, 4]
    m <- "svyglm Wald t"
  }
  data.table(variable = v, p_value = ifelse(p < 0.001, "<0.001", formatC(p, format = "f", digits = 3)),
             test = m)
}))
table1[, ord := .I]
table1 <- merge(table1, pval, by = "variable", all.x = TRUE, sort = FALSE)
setorder(table1, ord)
table1[table1[, .I[-1], by = variable]$V1, `:=`(p_value = "", test = "")]   # p값은 변수의 첫 줄에만

# 변수 라벨 붙이기
table1 <- merge(var_dict[, .(variable = var, label)], table1, by = "variable", sort = FALSE)
setorder(table1, ord); table1[, ord := NULL]
fwrite(table1, file.path(out_dir, "04_table1.csv"), bom = TRUE)
cat("\n[Table 1] 분석 대상 비가중 n =", nrow(d_sub), "\n"); print(table1)

# ==== 5-1. 공식 통계 대조표 =================================================
# 공식값 출처: 보건복지부·질병관리본부 2016 국민건강통계 발표(2017.11.6) 보도 내용(서울신문 2017-11-06 기사 인용).
#   보고서 원문 표 번호는 확인하지 못했다. 만19세이상 34.8%가 조율인지 표준화인지 기사에 명시되지 않았다.
#   (지표누리 비만율 지표는 연도별 추이를 2005년 추계인구로 표준화해 제시)
# 지표 정의: BMI>=25 (임신부 제외), 지침서 PDF p.49 / 인쇄 p.41
# 대조 항목 선택: 보도된 만19세이상 전체/남/여(34.8/42.3/26.4%)와 만30세이상(37.0/43.3/30.0%)은
#   연령표준화(2005년 추계인구) 값으로 판단되어 조율 추정치와 직접 비교하지 않는다
#   (예비 계산에서 여자 조율값이 공식값보다 2-3%p 높았고, 표준화 시 고령 여성 비중이 줄어드는 방향과 일치).
#   연령군별 값은 연령표준화가 적용되지 않는 조율 값이므로, 남자 연령군별 값 5개로 대조한다.
src_ob <- "2016 국민건강통계 발표(보건복지부·질병관리본부, 2017.11.6), 서울신문 2017-11-06 보도 인용; 연령군별 조율값"
ob_row <- function(lab, lo, hi, val) list(
  indicator = paste0("남자 ", lab, " 비만 유병률(%, BMI>=25), 2016"),
  pop  = bquote(age >= .(lo) & age <= .(hi) & !is.na(wt_itvex) & sex == "남자"),
  ind  = quote(as.numeric(he_bmi >= 25)),
  scale = 100, official = val, source = src_ob)
official <- list(
  ob_row("30대",      30, 39, 45.4),
  ob_row("40대",      40, 49, 49.0),
  ob_row("50대",      50, 59, 39.7),
  ob_row("60대",      60, 69, 39.7),
  ob_row("70세이상",  70, 200, 30.3)
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
check[, diff := estimate - official]
check[, within_ci := official >= ci_low & official <= ci_high]
num_cols <- c("estimate", "se", "ci_low", "ci_high", "diff")
check[, (num_cols) := lapply(.SD, round, 2), .SDcols = num_cols]
print(check)
if (any(check$within_ci %in% FALSE)) warning("공식 통계가 95% CI 밖입니다. 정의·가중치·대상을 다시 확인하세요.")
fwrite(check, file.path(out_dir, "05_official_check.csv"), bom = TRUE)

# ==== 5-2. 재현성 기록 ======================================================
inputs <- data.table(file = files, size = file.size(files),
                     md5 = unname(tools::md5sum(files)))
fwrite(inputs, file.path(out_dir, "_inputs.csv"), bom = TRUE)
writeLines(c(paste("임신부 BMI 결측 처리 건수(전체 DB):", n_preg_bmi),
             capture.output(sessionInfo())), file.path(out_dir, "_sessionInfo.txt"))
cat("\n완료:", out_dir, "\n")
