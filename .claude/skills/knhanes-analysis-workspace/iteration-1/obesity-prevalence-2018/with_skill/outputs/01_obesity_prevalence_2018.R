# 01_obesity_prevalence_2018.R
# 2018년 국민건강영양조사 기본DB(hn18_all) - 만 19세 이상 성인의 성별 비만 유병률(BMI >= 25 kg/m2)
# knhanes-analysis 스킬의 analysis_template.R 을 바탕으로 작성
# 실행 (프로젝트 폴더에서):
#   Rscript --vanilla .claude/skills/knhanes-analysis-workspace/iteration-1/obesity-prevalence-2018/with_skill/outputs/01_obesity_prevalence_2018.R
# 원칙:
#   - data 폴더는 읽기만 한다. 결과는 out_dir 에만 쓴다.
#   - 분석 대상은 행을 지우지 않고 inc 변수(1/0)로 표시한다.
#   - 추정은 모두 svydesign(가중치·층·집락) -> subset(inc == 1) 으로 한다.

suppressPackageStartupMessages({
  library(haven)
  library(data.table)
  library(survey)
})

# ==== 0. 설정 ===============================================================
years   <- c(2018)
out_dir <- file.path(".claude", "skills", "knhanes-analysis-workspace", "iteration-1",
                     "obesity-prevalence-2018", "with_skill", "outputs")
files   <- file.path("data", "국건영 조사결과",
                     sprintf("hn%02d_all.sas7bdat", years %% 100))
pool_ratio <- NULL   # 단일 연도이므로 통합가중치 불필요

G7 <- "제7기(2016-2018) 원시자료 이용지침서"

# ==== 1. 변수 정의 (지침서에서 확인한 내용만 적는다) ========================
var_dict <- rbindlist(list(
  list(var = "year",     type = "id",     levels = "", miss = "", na8 = "",
       label = "조사연도",                 source = paste(G7, "PDF p.93 / 인쇄 p.85")),
  list(var = "sex",      type = "cat",    levels = "1=남자;2=여자", miss = "", na8 = "",
       label = "성별",                     source = paste(G7, "PDF p.93 / 인쇄 p.85")),
  list(var = "age",      type = "cont",   levels = "", miss = "", na8 = "",
       label = "만나이(80=80세이상 탑코딩)", source = paste(G7, "PDF p.93 / 인쇄 p.85 (각주1 탑코딩: PDF p.94 / 인쇄 p.86)")),
  list(var = "psu",      type = "design", levels = "", miss = "", na8 = "",
       label = "조사구번호(집락)",         source = paste(G7, "PDF p.93 / 인쇄 p.85; 집락 설명 PDF p.39 / 인쇄 p.31")),
  list(var = "kstrata",  type = "design", levels = "", miss = "", na8 = "",
       label = "분산추정 층",              source = paste(G7, "PDF p.94 / 인쇄 p.86; 층 설명 PDF p.39 / 인쇄 p.31")),
  list(var = "wt_itvex", type = "design", levels = "", miss = "", na8 = "",
       label = "건강설문-검진조사 가중치",  source = paste(G7, "PDF p.94 / 인쇄 p.86")),
  list(var = "he_prg",   type = "cat",    levels = "0=아니오;1=예;8=비해당(남자)", miss = "", na8 = "8=범주유지",
       label = "임신여부",                 source = paste(G7, "PDF p.192 / 인쇄 p.184")),
  list(var = "he_dprg",  type = "cont",   levels = "", miss = "", na8 = "",
       label = "임신개월수(값 있으면 임신부)", source = paste(G7, "PDF p.192 / 인쇄 p.184; 비만 지표 산출 조건 HE_dprg=. : PDF p.49 / 인쇄 p.41")),
  list(var = "he_bmi",   type = "cont",   levels = "", miss = "", na8 = "",
       label = "체질량지수(kg/m2, 생성변수)", source = paste(G7, "PDF p.197 / 인쇄 p.189")),
  list(var = "he_obe",   type = "cat",
       levels = "1=저체중(18.5미만);2=정상(18.5-23미만);3=비만전단계(23-25미만);4=1단계비만(25-30미만);5=2단계비만(30-35미만);6=3단계비만(35이상)",   # 라벨에 '=' 쓰지 않음(코드 분리자)
       miss = "", na8 = "",
       label = "비만 유병여부(19세이상, 비임신자, 2017-2018 6범주)",
       source = paste(G7, "PDF p.197 / 인쇄 p.189 (각주2: 2017-2018 해당); 산출 프로그램 PDF p.49 / 인쇄 p.41"))
))

# 파생변수 정의 (원시자료에 없음. 01 정의표에 함께 기록)
derived_dict <- rbindlist(list(
  list(var = "agegrp", type = "cat", levels = "19-29;30-39;40-49;50-59;60-69;70+", miss = "", na8 = "",
       label = "연령군(파생)", source = paste(G7, "연령구간 예제 cage, PDF p.44 / 인쇄 p.36")),
  list(var = "obe", type = "cat", levels = "BMI 25 미만;BMI 25 이상(비만)", miss = "", na8 = "",
       label = "비만(BMI>=25; he_obe 4,5,6) (파생)",
       source = paste(G7, "PDF p.49 / 인쇄 p.41; 기수내 통합 예제 OBE=(HE_obe in (4,5,6)) PDF p.42 / 인쇄 p.34"))
))

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
fwrite(rbind(var_dict, derived_dict), file.path(out_dir, "01_variable_definitions.csv"), bom = TRUE)

# ---- 자료 읽기: 필요한 변수만, 대소문자 무시 ----
read_year <- function(f) {
  nms  <- names(read_sas(f, n_max = 0))
  want <- nms[tolower(nms) %in% var_dict$var]
  miss <- setdiff(var_dict$var, tolower(nms))
  if (length(miss)) message(basename(f), "에 없는 변수: ", paste(miss, collapse = ", "))
  x <- as.data.table(zap_labels(read_sas(f, col_select = tidyselect::all_of(want))))
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
# 비만 지표(숫자)는 factor 변환 전에 만든다: he_obe 4,5,6 = BMI 25 이상 (PDF p.49 / 인쇄 p.41)
d[, obe_num := fifelse(he_obe %in% 4:6, 1, fifelse(he_obe %in% 1:3, 0, NA_real_))]

for (i in which(var_dict$type == "cat")) {
  v  <- var_dict$var[i]
  kv <- do.call(rbind, strsplit(split_codes(var_dict$levels[i]), "="))
  unknown <- setdiff(unique(na.omit(d[[v]])), as.numeric(kv[, 1]))
  if (length(unknown)) warning(v, ": levels에 없는 코드 ", paste(unknown, collapse = ","),
                               " -> 지침서 확인 후 var_dict 수정")
  d[, (v) := factor(get(v), levels = as.numeric(kv[, 1]), labels = kv[, 2])]
}
# 파생변수
d[, agegrp := cut(age, breaks = c(18, 29, 39, 49, 59, 69, Inf),
                  labels = c("19-29", "30-39", "40-49", "50-59", "60-69", "70+"))]
d[, obe := factor(obe_num, levels = c(0, 1), labels = c("BMI 25 미만", "BMI 25 이상(비만)"))]
stopifnot(nrow(d) == n_raw)

# 점검: he_obe 4-6 과 he_bmi >= 25 가 일치하는지 (생성변수 정의 확인)
chk_bmi <- d[!is.na(obe_num), all((he_bmi >= 25) == (obe_num == 1))]
cat("he_obe(4-6) 와 he_bmi>=25 일치:", chk_bmi, "\n")
stopifnot(isTRUE(chk_bmi))

# ==== 2. 분석 대상 표시 (행 삭제 없음) ======================================
# 공식 지표 정의(PDF p.49 / 인쇄 p.41): 비임신자(HE_dprg=.) 중 키·몸무게 있는 사람의 BMI로 산출
criteria <- list(
  "가중치 있음(검진·설문 참여)"        = quote(!is.na(wt_itvex)),
  "만 19세 이상"                        = quote(age >= 19),
  "임신부 제외(HE_dprg 결측)"           = quote(is.na(he_dprg)),
  "BMI·비만지표 결측 없음(키·몸무게 측정)" = quote(!is.na(obe_num))
)
d[, inc := 1L]
flow <- data.table(step = "2018 기본DB 전체", n = nrow(d))
for (nm in names(criteria)) {
  ok <- d[, eval(criteria[[nm]])]
  d[is.na(ok) | !ok, inc := 0L]
  flow <- rbind(flow, data.table(step = nm, n = d[inc == 1L, .N]))
}
flow[, excluded := shift(n) - n]
fwrite(flow, file.path(out_dir, "02_participant_flow.csv"), bom = TRUE)
print(flow)
stopifnot(nrow(d) == n_raw)

# ==== 3. 복합표본 설계 지정 -> subset ========================================
d[, wt_analysis := wt_itvex]
d[is.na(wt_analysis), wt_analysis := 0]   # 가중치 결측자는 inc = 0 (행은 유지)

lonely <- d[wt_analysis > 0, uniqueN(psu), by = kstrata][V1 < 2]
if (nrow(lonely)) warning("집락이 1개인 층: ", paste(lonely$kstrata, collapse = ","),
                          " -> options(survey.lonely.psu = ...) 선택 근거를 기록하세요.")
cat("집락이 1개인 층 수:", nrow(lonely), "\n")

des <- svydesign(ids = ~psu, strata = ~kstrata, weights = ~wt_analysis,
                 nest = TRUE, data = d)
sub <- subset(des, inc == 1)
d_sub <- d[inc == 1]

# ==== 4. Table 1: 비가중 n + 가중 추정치 (성별 열) ===========================
by_var    <- "sex"
tab1_vars <- c("age", "agegrp", "he_bmi", "he_obe", "obe")

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
for (g in levels(d_sub[[by_var]])) {
  des_g <- subset(sub, get(by_var) == g)          # 부분집단도 subset으로
  col_g <- table1_col(des_g, d_sub[get(by_var) == g], g)
  table1 <- merge(table1, col_g, by = c("variable", "level", "stat"), sort = FALSE)
}
all_dict <- rbind(var_dict, derived_dict)
table1 <- merge(all_dict[, .(variable = var, label)], table1, by = "variable", sort = FALSE)
table1[, variable := factor(variable, levels = tab1_vars)]
setorder(table1, variable)   # 행 순서를 tab1_vars 순으로 고정 (재현성)
fwrite(table1, file.path(out_dir, "04_table1.csv"), bom = TRUE)
cat("\n[Table 1] 분석 대상 비가중 n =", nrow(d_sub), "\n"); print(table1)

# ==== (본 분석) 성별 비만 유병률 =============================================
prev_row <- function(design, dat, label) {
  est <- svymean(~obe_num, design, na.rm = TRUE)
  ci  <- confint(est)
  data.table(group = label,
             n_unweighted = nrow(dat),
             n_obese_unweighted = dat[obe_num == 1, .N],
             prevalence_pct = round(100 * unname(coef(est)), 2),
             se_pct = round(100 * as.numeric(SE(est)), 2),
             ci95_low = round(100 * unname(ci[1]), 2),
             ci95_high = round(100 * unname(ci[2]), 2))
}
main <- rbind(
  prev_row(sub, d_sub, "전체"),
  prev_row(subset(sub, sex == "남자"), d_sub[sex == "남자"], "남자"),
  prev_row(subset(sub, sex == "여자"), d_sub[sex == "여자"], "여자")
)
# 성별 차이 검정 (Rao-Scott 보정 카이제곱)
tst <- svychisq(~obe + sex, sub, statistic = "F")
main[, p_sex_diff := c(signif(unname(tst$p.value), 3), NA, NA)]
fwrite(main, file.path(out_dir, "03_obesity_prevalence_by_sex.csv"), bom = TRUE)
cat("\n[본 분석] 성별 비만 유병률 (조율, %)\n"); print(main)

# 참고: 성·연령군별 유병률 (조율)
age_tab <- rbindlist(lapply(levels(d_sub$sex), function(s) {
  rbindlist(lapply(levels(d_sub$agegrp), function(a) {
    r <- prev_row(subset(sub, sex == s & agegrp == a), d_sub[sex == s & agegrp == a], a)
    r[, sex := s]
  }))
}))
setcolorder(age_tab, "sex")
fwrite(age_tab, file.path(out_dir, "03b_obesity_prevalence_by_sex_agegroup.csv"), bom = TRUE)
print(age_tab)

# ==== 5-1. 공식 통계 대조표 =================================================
# 공식값 출처: e-나라지표 '비만 유병률'(idxCd=2705), 국민건강증진종합계획 기준값 '18 남자 42.8, 여자 25.5
#   (자료: 국민건강통계 / 질병관리청 국민건강영양조사; 지표 해석에 '만19세이상, 표준화'로 표기,
#    2005년 추계인구로 연령표준화).
# 주의: 공식값은 연령표준화율, 본 추정치는 조율(crude). 정의가 완전히 같지 않으므로 '참고 대조'로만 해석한다.
official <- list(
  list(indicator = "만19세이상 비만 유병률(%), 2018, 남자 [본 추정=조율 / 공식=연령표준화]",
       pop  = quote(inc == 1 & sex == "남자"),
       ind  = quote(obe_num),
       scale = 100, official = 42.8,
       source = "e-나라지표 비만 유병률(idxCd=2705), 국민건강통계(질병관리청) - 2005년 추계인구 연령표준화"),
  list(indicator = "만19세이상 비만 유병률(%), 2018, 여자 [본 추정=조율 / 공식=연령표준화]",
       pop  = quote(inc == 1 & sex == "여자"),
       ind  = quote(obe_num),
       scale = 100, official = 25.5,
       source = "e-나라지표 비만 유병률(idxCd=2705), 국민건강통계(질병관리청) - 2005년 추계인구 연령표준화"),
  # 연령군별 값은 표준화의 영향을 받지 않으므로 조율끼리 직접 대조할 수 있다.
  # 단, 출처가 2차 자료(언론 보도)이므로 원 통계집의 표·쪽은 확인하지 못했다.
  list(indicator = "비만 유병률(%), 2018, 남자 30-49세 [조율]",
       pop  = quote(inc == 1 & sex == "남자" & age >= 30 & age <= 49),
       ind  = quote(obe_num),
       scale = 100, official = 49.3,
       source = "질병관리본부 통계집 '국민건강영양조사(1998-2018)' 인용 보도(코메디닷컴 2020.09.10) - 2차 자료"),
  list(indicator = "비만 유병률(%), 2018, 여자 65세 이상 [조율]",
       pop  = quote(inc == 1 & sex == "여자" & age >= 65),
       ind  = quote(obe_num),
       scale = 100, official = 41.8,
       source = "질병관리본부 통계집 '국민건강영양조사(1998-2018)' 인용 보도(코메디닷컴 2020.09.10) - 2차 자료")
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
if (any(check$within_ci %in% FALSE)) warning("공식 통계가 95% CI 밖인 항목이 있습니다. 정의(조율/표준화)·가중치·대상을 확인하세요.")
fwrite(check, file.path(out_dir, "05_official_check.csv"), bom = TRUE)

# ==== 5-2. 재현성 기록 ======================================================
inputs <- data.table(file = files, size = file.size(files),
                     md5 = unname(tools::md5sum(files)))
fwrite(inputs, file.path(out_dir, "_inputs.csv"), bom = TRUE)
writeLines(capture.output(sessionInfo()), file.path(out_dir, "_sessionInfo.txt"))
cat("\n완료:", out_dir, "\n")
