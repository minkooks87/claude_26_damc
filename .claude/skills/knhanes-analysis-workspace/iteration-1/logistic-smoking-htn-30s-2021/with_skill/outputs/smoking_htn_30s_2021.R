# smoking_htn_30s_2021.R
# 주제: 2021년 국민건강영양조사, 30-39세 성인에서 현재흡연과 고혈압 유병의 관련성
#       (복합표본 로지스틱 회귀, 성별·나이·소득 보정)
# 기반: knhanes-analysis 스킬 analysis_template.R (1~5단계)
# 실행: 프로젝트 폴더(knhanes)에서
#   Rscript --vanilla .claude/skills/knhanes-analysis-workspace/iteration-1/logistic-smoking-htn-30s-2021/with_skill/outputs/smoking_htn_30s_2021.R
# 원칙:
#   - data 폴더는 읽기만 한다. 결과는 out_dir에만 쓴다.
#   - 분석 대상은 행을 지우지 않고 inc 변수(1/0)로 표시한다.
#   - 추정은 모두 svydesign(가중치·층·집락) → subset(inc == 1) 으로 한다.

suppressPackageStartupMessages({
  library(haven)
  library(data.table)
  library(survey)
})

# ==== 0. 설정 ===============================================================
years   <- c(2021)
out_dir <- file.path(".claude", "skills", "knhanes-analysis-workspace", "iteration-1",
                     "logistic-smoking-htn-30s-2021", "with_skill", "outputs", "results")
files   <- file.path("data", "국건영 조사결과",
                     sprintf("hn%02d_all.sas7bdat", years %% 100))
pool_ratio <- NULL   # 단일 연도이므로 통합가중치 불필요

G8 <- "제8기(2019-2021) 지침서"

# ==== 1. 변수 정의 (제8기 지침서에서 확인한 내용) ===========================
# type: cat(범주형, factor 변환) / cont(연속형) / raw(원코드 유지: 생성변수 재현 확인용) / design / id
var_dict <- rbindlist(list(
  list(var = "year",     type = "id",     levels = "", miss = "", na8 = "",
       label = "조사연도", source = paste(G8, "PDF p.91 / 인쇄 p.83")),
  list(var = "sex",      type = "cat",    levels = "1=남자;2=여자", miss = "", na8 = "",
       label = "성별", source = paste(G8, "PDF p.91 / 인쇄 p.83")),
  list(var = "age",      type = "cont",   levels = "", miss = "", na8 = "",
       label = "만나이(80=80세이상 탑코딩)", source = paste(G8, "PDF p.91-92 / 인쇄 p.83-84 (각주1 탑코딩)")),
  list(var = "incm",     type = "cat",    levels = "1=하;2=중하;3=중상;4=상", miss = "", na8 = "",
       label = "소득 4분위수(개인)", source = paste(G8, "PDF p.91 / 인쇄 p.83")),
  list(var = "psu",      type = "design", levels = "", miss = "", na8 = "",
       label = "조사구번호(집락)", source = paste(G8, "PDF p.91 / 인쇄 p.83; 설계 설명 PDF p.41 / 인쇄 p.33")),
  list(var = "kstrata",  type = "design", levels = "", miss = "", na8 = "",
       label = "분산추정 층", source = paste(G8, "PDF p.92 / 인쇄 p.84; 설계 설명 PDF p.41 / 인쇄 p.33")),
  list(var = "wt_itvex", type = "design", levels = "", miss = "", na8 = "",
       label = "건강설문-검진조사 가중치", source = paste(G8, "PDF p.92 / 인쇄 p.84; 분석 예시 PDF p.42-43 / 인쇄 p.34-35")),
  list(var = "sm_presnt", type = "cont",  levels = "", miss = "", na8 = "",
       label = "현재흡연(생성변수: 0=과거흡연·비흡연, 1=현재흡연). 평생 궐련 5갑 이상 & 현재 매일/가끔 흡연",
       source = paste(G8, "PDF p.182 / 인쇄 p.174; 산출 프로그램 PDF p.48 / 인쇄 p.40")),
  list(var = "bs1_1",    type = "raw",    levels = "1=5갑미만;2=5갑이상;3=피운적없음;8=비해당;9=모름무응답",
       miss = "", na8 = "범주유지",
       label = "평생 일반담배(궐련) 흡연 여부 (sm_presnt 재현 확인용, 원코드 유지)",
       source = paste(G8, "PDF p.175 / 인쇄 p.167")),
  list(var = "bs3_1",    type = "raw",    levels = "1=매일;2=가끔;3=과거흡연;8=비해당(문항1-③⑧);9=모름무응답",
       miss = "", na8 = "범주유지",
       label = "현재 일반담배(궐련) 흡연 여부 (sm_presnt 재현 확인용, 원코드 유지)",
       source = paste(G8, "PDF p.175 / 인쇄 p.167")),
  list(var = "he_hp",    type = "cont",   levels = "1=정상;2=고혈압전단계;3=고혈압", miss = "", na8 = "",
       label = "고혈압 유병여부(19세이상, 생성변수). 3=SBP>=140 또는 DBP>=90 또는 고혈압약 복용",
       source = paste(G8, "PDF p.196 / 인쇄 p.188; 산출 프로그램 PDF p.51 / 인쇄 p.43")),
  list(var = "he_sbp",   type = "cont",   levels = "", miss = "", na8 = "",
       label = "최종 수축기 혈압(2,3차 평균, mmHg). 2021년 19세이상은 진동형 혈압계",
       source = paste(G8, "PDF p.196 / 인쇄 p.188")),
  list(var = "he_dbp",   type = "cont",   levels = "", miss = "", na8 = "",
       label = "최종 이완기 혈압(2,3차 평균, mmHg)",
       source = paste(G8, "PDF p.196 / 인쇄 p.188")),
  list(var = "di1_2",    type = "raw",    levels = "1=매일;2=월20일이상;3=월15일이상;4=월15일미만;5=복용안함;8=비해당;9=모름무응답",
       miss = "", na8 = "범주유지",
       label = "혈압조절제 복용 (HE_HP 재현·민감도분석용, 원코드 유지)",
       source = paste(G8, "PDF p.100 / 인쇄 p.92; 사용법 PDF p.51 / 인쇄 p.43"))
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
    if (length(kv) < 2) next            # "범주유지"
    code <- as.numeric(kv[1]); to <- kv[2]
    if (to == "결측")      d[get(v) == code, (v) := NA]
    else if (to != "범주유지") d[get(v) == code, (v) := as.numeric(to)]
  }
}

# ---- 생성변수 재현 확인 (지침서 산출 프로그램과 제공 변수 일치 여부) ----
# sm_presnt: IF (BS1_1 in (1,2) & BS3_1 in (1,2,3)) or BS1_1=3 THEN sm_presnt=(BS1_1=2 & BS3_1 in (1,2))  [PDF p.48]
d[, sm_re := NA_real_]
d[(bs1_1 %in% 1:2 & bs3_1 %in% 1:3) | bs1_1 == 3, sm_re := as.numeric(bs1_1 == 2 & bs3_1 %in% 1:2)]
# HE_hp: IF HE_SBP^=. & HE_DBP^=. & DI1_2 in (1,2,3,4,5,8) THEN ... [PDF p.51]
d[, hp_re := NA_real_]
d[!is.na(he_sbp) & !is.na(he_dbp) & di1_2 %in% c(1:5, 8), hp_re := fifelse(
  he_sbp >= 140 | he_dbp >= 90 | di1_2 %in% 1:4, 3,
  fifelse(he_sbp >= 120 | he_dbp >= 80, 2,
          fifelse(he_sbp > 0 & he_sbp < 120 & he_dbp > 0 & he_dbp < 80, 1, NA_real_)))]
recode_check <- rbind(
  data.table(variable = "sm_presnt",
             scope = "만19세 이상 (sm_presnt 산출 대상)",
             n_both_nonmissing = d[age >= 19 & !is.na(sm_presnt) & !is.na(sm_re), .N],
             n_agree = d[age >= 19 & !is.na(sm_presnt) & !is.na(sm_re) & sm_presnt == sm_re, .N],
             n_provided_only = d[age >= 19 & !is.na(sm_presnt) & is.na(sm_re), .N],
             n_recomputed_only = d[age >= 19 & is.na(sm_presnt) & !is.na(sm_re), .N]),
  data.table(variable = "HE_HP",
             scope = "만19세 이상 (HE_HP 산출 대상)",
             n_both_nonmissing = d[age >= 19 & !is.na(he_hp) & !is.na(hp_re), .N],
             n_agree = d[age >= 19 & !is.na(he_hp) & !is.na(hp_re) & he_hp == hp_re, .N],
             n_provided_only = d[age >= 19 & !is.na(he_hp) & is.na(hp_re), .N],
             n_recomputed_only = d[age >= 19 & is.na(he_hp) & !is.na(hp_re), .N])
)
print(recode_check)

# ---- 분석 변수 생성 ----
# 노출: 현재흡연 (지침서 생성변수 sm_presnt 그대로 사용; 0에는 과거흡연자와 비흡연자가 함께 포함)
d[, smk := factor(sm_presnt, levels = c(0, 1), labels = c("비흡연·과거흡연", "현재흡연"))]
# 결과: 고혈압 유병 (HE_HP=3) vs 정상·전단계 (HE_HP=1,2)
d[, htn := factor(fifelse(he_hp == 3, 1, 0), levels = c(0, 1), labels = c("고혈압 아님", "고혈압"))]
d[, htn01 := as.numeric(he_hp == 3)]
# 민감도분석: 2021년 진동형 혈압계 → 청진형(Greenlight) 전환식 적용 혈압으로 고혈압 재정의
#   전환식 출처: 제8기 지침서 PDF p.196 / 인쇄 p.188 각주 ‡ (공식통계 산출 시에는 사용하지 않음)
#   SBP_G = 10.773 + 0.771*SBP + 0.039*age + 0.374*(SBP-DBP)
#   DBP_G = 13.480 + 0.952*DBP - 0.051*age - 0.098*(SBP-DBP)
d[, `:=`(sbp_g = 10.773 + 0.771 * he_sbp + 0.039 * age + 0.374 * (he_sbp - he_dbp),
         dbp_g = 13.480 + 0.952 * he_dbp - 0.051 * age - 0.098 * (he_sbp - he_dbp))]
d[, htn_conv := NA_real_]
d[age >= 19 & !is.na(sbp_g) & !is.na(dbp_g) & di1_2 %in% c(1:5, 8),
  htn_conv := as.numeric(sbp_g >= 140 | dbp_g >= 90 | di1_2 %in% 1:4)]

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
  "가중치 있음(건강설문·검진 참여)"      = quote(!is.na(wt_itvex)),
  "만 30-39세"                           = quote(age >= 30 & age <= 39),
  "고혈압 유병여부(HE_HP) 결측 없음"      = quote(!is.na(he_hp)),
  "현재흡연(sm_presnt) 결측 없음"         = quote(!is.na(sm_presnt)),
  "소득 4분위(incm) 결측 없음"            = quote(!is.na(incm))
)
d[, inc := 1L]
flow <- data.table(step = "2021 기본DB(hn21_all) 전체", n = nrow(d))
for (nm in names(criteria)) {
  ok <- d[, eval(criteria[[nm]])]
  d[is.na(ok) | !ok, inc := 0L]
  flow <- rbind(flow, data.table(step = nm, n = d[inc == 1L, .N]))
}
flow[, excluded := shift(n) - n]
fwrite(flow, file.path(out_dir, "02_participant_flow.csv"), bom = TRUE)
print(flow)
stopifnot(nrow(d) == n_raw)

# ==== 3. 복합표본 설계 지정 → subset ========================================
d[, wt_analysis := wt_itvex]
d[is.na(wt_analysis), wt_analysis := 0]

lonely <- d[wt_analysis > 0, uniqueN(psu), by = kstrata][V1 < 2]
if (nrow(lonely)) warning("집락이 1개인 층: ", paste(lonely$kstrata, collapse = ","),
                          " → options(survey.lonely.psu = ...) 선택 근거를 기록하세요.")
# 분석 영역(30대) 안에서 집락이 1개뿐인 층 수 (subset 방식에서는 전체 설계로 분산을 계산하므로 참고용)
lonely_dom <- d[inc == 1L, uniqueN(psu), by = kstrata][V1 < 2]
cat("분석 영역 내 집락 1개인 층 수(참고):", nrow(lonely_dom), "\n")

des <- svydesign(ids = ~psu, strata = ~kstrata, weights = ~wt_analysis,
                 nest = TRUE, data = d)
sub <- subset(des, inc == 1)
d_sub <- d[inc == 1]

# ==== 4. Table 1: 비가중 n + 가중 추정치 (현재흡연 여부별) ==================
by_var    <- "smk"
tab1_vars <- c("sex", "age", "incm", "htn", "he_sbp", "he_dbp")
lab_tab <- rbind(var_dict[, .(variable = var, label)],
                 data.table(variable = c("htn", "smk"),
                            label = c("고혈압 유병(HE_HP=3)", "현재흡연(sm_presnt)")))

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
  des_g <- subset(sub, smk == g)
  col_g <- table1_col(des_g, d_sub[smk == g], g)
  table1 <- merge(table1, col_g, by = c("variable", "level", "stat"), sort = FALSE)
}
# 집단 간 차이 검정 (Rao-Scott 카이제곱 / 설계기반 t 검정)
pvals <- vapply(tab1_vars, function(v) {
  if (is.factor(d_sub[[v]])) {
    unname(as.numeric(svychisq(as.formula(paste0("~", v, "+smk")), sub)$p.value))
  } else {
    unname(summary(svyglm(as.formula(paste0(v, "~smk")), sub))$coefficients[2, 4])
  }
}, numeric(1))
names(pvals) <- tab1_vars
table1[, p_value := ""]
for (v in tab1_vars) table1[variable == v, p_value := c(fifelse(pvals[[v]] < 0.001, "<0.001", formatC(pvals[[v]], format = "f", digits = 3)), rep("", .N - 1))]
table1 <- merge(lab_tab, table1, by = "variable", sort = FALSE)
table1[, ord := match(variable, tab1_vars)]
setorder(table1, ord); table1[, ord := NULL]
fwrite(table1, file.path(out_dir, "04_table1.csv"), bom = TRUE)
cat("\n[Table 1] 분석 대상 비가중 n =", nrow(d_sub), "\n"); print(table1)

# ==== (본 분석) 복합표본 로지스틱 회귀 ======================================
# 모든 모형은 sub(또는 그 subset)에 대해 실행한다.
or_table <- function(fit, model, keep = NULL) {
  cf <- summary(fit)$coefficients
  dfr <- fit$df.residual
  out <- data.table(term = rownames(cf), b = cf[, 1], se = cf[, 2], p = cf[, 4])
  out <- out[term != "(Intercept)"]
  if (!is.null(keep)) out <- out[term %in% keep]
  tq <- qt(0.975, dfr)
  out[, `:=`(OR = exp(b), CI_low = exp(b - tq * se), CI_high = exp(b + tq * se))]
  out[, model := model]
  out[, df_resid := dfr]
  out[, .(model, term, OR = round(OR, 3), CI_low = round(CI_low, 3), CI_high = round(CI_high, 3),
          p = signif(p, 3), df_resid)]
}

fit1 <- svyglm(htn01 ~ smk, design = sub, family = quasibinomial())
fit2 <- svyglm(htn01 ~ smk + sex + age + incm, design = sub, family = quasibinomial())
res <- rbind(or_table(fit1, "모형1: 비보정"),
             or_table(fit2, "모형2: 성별·나이·소득 보정"))

# 성별 상호작용 검정 (현재흡연 × 성별)
fit_int <- svyglm(htn01 ~ smk * sex + age + incm, design = sub, family = quasibinomial())
int_test <- regTermTest(fit_int, ~smk:sex)

# 성별 층화 (보조분석: 나이·소득 보정)
fit_m <- svyglm(htn01 ~ smk + age + incm, design = subset(sub, sex == "남자"), family = quasibinomial())
fit_f <- svyglm(htn01 ~ smk + age + incm, design = subset(sub, sex == "여자"), family = quasibinomial())
res <- rbind(res,
             or_table(fit_m, "보조: 남자(나이·소득 보정)", keep = "smk현재흡연"),
             or_table(fit_f, "보조: 여자(나이·소득 보정)", keep = "smk현재흡연"))

# 민감도분석: 전환식 적용 혈압 기준 고혈압
fit_s <- svyglm(htn_conv ~ smk + sex + age + incm, design = subset(sub, !is.na(htn_conv)),
                family = quasibinomial())
res <- rbind(res, or_table(fit_s, "민감도: 전환식 혈압 기준 고혈압(성별·나이·소득 보정)", keep = "smk현재흡연"))

# 모형별 비가중 n, 사건 수
nsum <- function(dd, y) dd[!is.na(get(y)), .(n_unweighted = .N, n_htn = sum(get(y) == 1),
                                             n_smoker = sum(smk == "현재흡연"),
                                             n_htn_smoker = sum(get(y) == 1 & smk == "현재흡연"))]
model_n <- rbind(
  cbind(model = "모형1: 비보정", nsum(d_sub, "htn01")),
  cbind(model = "모형2: 성별·나이·소득 보정", nsum(d_sub, "htn01")),
  cbind(model = "보조: 남자(나이·소득 보정)", nsum(d_sub[sex == "남자"], "htn01")),
  cbind(model = "보조: 여자(나이·소득 보정)", nsum(d_sub[sex == "여자"], "htn01")),
  cbind(model = "민감도: 전환식 혈압 기준 고혈압(성별·나이·소득 보정)", nsum(d_sub, "htn_conv"))
)
res <- merge(res, model_n, by = "model", sort = FALSE)
fwrite(res, file.path(out_dir, "06_logistic_OR.csv"), bom = TRUE)
print(res)

inter <- data.table(test = "현재흡연 × 성별 상호작용 (Wald F, regTermTest)",
                    F = round(as.numeric(int_test$Ftest), 3), df1 = int_test$df,
                    df2 = int_test$ddf, p = signif(as.numeric(int_test$p), 3))
fwrite(inter, file.path(out_dir, "07_interaction_test.csv"), bom = TRUE)
print(inter)

# 흡연 여부별 가중 고혈압 유병률 (전체, 성별)
prev_one <- function(design, dat, grp) {
  rbindlist(lapply(levels(dat$smk), function(g) {
    dg <- subset(design, smk == g)
    e  <- svymean(~htn01, dg, na.rm = TRUE)
    # 사건 수가 적은 칸에서 Wald CI가 0 아래로 내려가지 않도록 logit 변환 CI 사용
    ci <- attr(svyciprop(~htn01, dg, method = "logit", na.rm = TRUE), "ci")
    data.table(group = grp, smk = g, n_unweighted = dat[smk == g, .N],
               n_htn = dat[smk == g, sum(htn01 == 1)],
               prev_pct = round(100 * coef(e)[[1]], 1), se = round(100 * SE(e)[[1]], 1),
               ci_low = round(100 * ci[1], 1), ci_high = round(100 * ci[2], 1))
  }))
}
prev <- rbind(prev_one(sub, d_sub, "전체"),
              prev_one(subset(sub, sex == "남자"), d_sub[sex == "남자"], "남자"),
              prev_one(subset(sub, sex == "여자"), d_sub[sex == "여자"], "여자"))
fwrite(prev, file.path(out_dir, "08_htn_prevalence_by_smoking.csv"), bom = TRUE)
print(prev)

# ==== 5-1. 공식 통계 대조표 =================================================
# (a) 가중치 합계 = 2021 추계인구수 (만1세이상) 51,531천명
#     출처: 제8기 지침서 표 11 '가중치 합계 비교', PDF p.40 / 인쇄 p.32 (검진조사 기본 가중치 합)
# (b) 생성변수 재현: 지침서 산출 프로그램(PDF p.48, p.51)으로 sm_presnt·HE_HP를 다시 만들어 일치율 확인
# (c) 참고: 질병관리청 발표 2021 현재흡연율·고혈압 유병률은 2005년 추계인구 연령표준화 값으로 보도되었고
#     표 번호·대상 연령을 원문에서 확인하지 못했으므로 CI 비교 판정에 쓰지 않는다(보고서에 기술).
wsum <- d[, sum(wt_itvex, na.rm = TRUE)] / 1000
check <- rbind(
  data.table(indicator = "wt_itvex 가중치 합(천명), 2021",
             n_unweighted = d[!is.na(wt_itvex), .N],
             estimate = round(wsum, 1), se = NA_real_, ci_low = NA_real_, ci_high = NA_real_,
             official = 51531,
             source = "제8기 지침서 표 11, PDF p.40 / 인쇄 p.32 (2021 추계인구 만1세이상, 천명)"),
  recode_check[, .(indicator = paste0(variable, " 지침서 프로그램 재현 일치율(%)"),
                   n_unweighted = n_both_nonmissing,
                   estimate = round(100 * n_agree / n_both_nonmissing, 2),
                   se = NA_real_, ci_low = NA_real_, ci_high = NA_real_, official = 100,
                   source = fifelse(variable == "sm_presnt",
                                    "제8기 지침서 표 18, PDF p.48 / 인쇄 p.40",
                                    "제8기 지침서 표 21, PDF p.51 / 인쇄 p.43"))]
)
check[, diff := estimate - official]
check[, match := abs(diff) < 0.5]   # 천명 단위 반올림·100% 일치 기준
fwrite(check, file.path(out_dir, "05_official_check.csv"), bom = TRUE)
fwrite(recode_check, file.path(out_dir, "05b_derived_variable_recompute.csv"), bom = TRUE)
print(check)

# 참고용: 만19세 이상 성별 조율(crude) 현재흡연율·고혈압 유병률 (공식 표준화 값과 직접 비교 불가)
d[, adult := as.integer(age >= 19 & wt_analysis > 0)]
des_ad <- subset(svydesign(ids = ~psu, strata = ~kstrata, weights = ~wt_analysis, nest = TRUE, data = d),
                 adult == 1)
ref_rows <- list()
for (s in c("남자", "여자")) {
  dz <- subset(des_ad, sex == s)
  for (v in c("sm_presnt", "htn01")) {
    e <- svymean(as.formula(paste0("~", v)), dz, na.rm = TRUE); ci <- confint(e)
    ref_rows[[length(ref_rows) + 1]] <- data.table(
      indicator = paste0(ifelse(v == "sm_presnt", "현재흡연율", "고혈압 유병률"), " 만19세이상 조율, ", s),
      n_unweighted = d[adult == 1 & sex == s & !is.na(get(v)), .N],
      estimate = round(100 * coef(e)[[1]], 1), se = round(100 * SE(e)[[1]], 1),
      ci_low = round(100 * ci[1], 1), ci_high = round(100 * ci[2], 1))
  }
}
ref_crude <- rbindlist(ref_rows)
fwrite(ref_crude, file.path(out_dir, "05c_reference_crude_19plus.csv"), bom = TRUE)
print(ref_crude)

# ==== 5-2. 재현성 기록 ======================================================
inputs <- data.table(file = files, size = file.size(files),
                     md5 = unname(tools::md5sum(files)))
fwrite(inputs, file.path(out_dir, "_inputs.csv"), bom = TRUE)
writeLines(capture.output(sessionInfo()), file.path(out_dir, "_sessionInfo.txt"))
cat("\n완료:", out_dir, "\n")
