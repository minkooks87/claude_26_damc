# =====================================================================
# 2021년 국민건강영양조사: 30대(만 30~39세) 남녀에서 현재흡연과 고혈압 유병의 관련성
# 복합표본 로지스틱 회귀 (보정: 성별, 나이, 소득)
#
# 변수 정의 근거: 국민건강영양조사 제8기(2019-2021) 원시자료 이용지침서 (이하 "8기 지침서")
#   - sex, age, incm             : 8기 지침서 p.83 (기본변수)
#   - wt_itvex, kstrata, psu     : 8기 지침서 p.83-84 (기본변수), p.33-34 (복합표본 분석)
#   - BS1_1, BS3_1               : 8기 지침서 p.167-168 (흡연)
#   - sm_presnt (현재흡연 생성변수): 8기 지침서 p.174, 산출 프로그램 p.40 (표 18)
#   - HE_HP (고혈압 유병여부)       : 8기 지침서 p.188, 산출 프로그램 p.43 (표 21)
#   - DI1_2 (혈압조절제 복용)       : 8기 지침서 p.92
#   - 2021년 혈압계 전환식          : 8기 지침서 p.188
# =====================================================================

suppressPackageStartupMessages({
  library(haven)
  library(survey)
})

# 결과 저장 폴더 (평가 실행 규칙에 따라 outputs 폴더에만 저장)
out_dir <- "C:/Users/kjh07/OneDrive/바탕 화면/knhanes/.claude/skills/knhanes-analysis-workspace/iteration-1/logistic-smoking-htn-30s-2021/without_skill/outputs"
data_file <- "C:/Users/kjh07/OneDrive/바탕 화면/knhanes/data/국건영 조사결과/hn21_all.sas7bdat"

# 한 PSU만 남는 층이 생길 경우 대비
options(survey.lonely.psu = "adjust")

# ---------------------------------------------------------------------
# 1. 자료 읽기 (data/ 폴더는 읽기만 함)
# ---------------------------------------------------------------------
vars <- c("year", "ID", "sex", "age", "incm", "BS1_1", "BS3_1", "sm_presnt",
          "HE_HP", "HE_sbp", "HE_dbp", "DI1_2", "wt_itvex", "kstrata", "psu")
raw <- read_sas(data_file, col_select = any_of(vars))
raw <- zap_labels(raw)
cat("2021 전체 레코드 수:", nrow(raw), "\n")

d <- as.data.frame(raw)

# ---------------------------------------------------------------------
# 2. 변수 정리
# ---------------------------------------------------------------------
# (1) 현재흡연: 생성변수 sm_presnt 사용 (0=과거흡연·비흡연, 1=현재흡연; p.174)
#     정의(p.40): (BS1_1 in (1,2) & BS3_1 in (1,2,3)) or BS1_1=3 일 때
#                 sm_presnt = (BS1_1=2 & BS3_1 in (1,2))
#     -> 원변수로 직접 재산출하여 제공 변수와 일치하는지 확인
#     BS1_1/BS3_1의 9(모름·무응답)는 결측. BS3_1=8(비해당: 문항1-③ 피운 적 없음)은
#     정의상 비흡연(0)으로 처리됨(p.167, p.40).
d$smk_chk <- ifelse((d$BS1_1 %in% c(1, 2) & d$BS3_1 %in% c(1, 2, 3)) | d$BS1_1 %in% 3,
                    as.numeric(d$BS1_1 %in% 2 & d$BS3_1 %in% c(1, 2)), NA)
cat("sm_presnt 재산출 일치 여부:",
    identical(is.na(d$smk_chk), is.na(d$sm_presnt)) &&
      all(d$smk_chk == d$sm_presnt, na.rm = TRUE), "\n")
d$smoke <- d$sm_presnt

# (2) 고혈압 유병: HE_HP (1=정상, 2=고혈압전단계, 3=고혈압; p.188)
#     고혈압(3) = 1, 정상·전단계(1,2) = 0
d$htn <- ifelse(d$HE_HP %in% 3, 1, ifelse(d$HE_HP %in% c(1, 2), 0, NA))

# (3) 보정변수
#     sex: 1=남자, 2=여자 / age: 만나이 / incm: 소득 4분위수(개인) 1=하 ~ 4=상 (p.83)
d$sex_f  <- factor(d$sex, levels = c(1, 2), labels = c("남자", "여자"))
d$incm_f <- factor(d$incm, levels = 1:4, labels = c("하", "중하", "중상", "상"))

# (4) 민감도 분석용: 2021년 진동형 혈압계 -> 청진형 혈압계 전환식 적용 고혈압 (p.188)
#     SBP(Greenlight) = 10.773 + 0.771*SBP + 0.039*age + 0.374*(SBP-DBP)
#     DBP(Greenlight) = 13.480 + 0.952*DBP - 0.051*age - 0.098*(SBP-DBP)
#     약 복용(DI1_2 in 1:4)은 p.43 프로그램 정의 그대로. DI1_2=9 는 결측.
pp <- d$HE_sbp - d$HE_dbp
d$sbp_c <- 10.773 + 0.771 * d$HE_sbp + 0.039 * d$age + 0.374 * pp
d$dbp_c <- 13.480 + 0.952 * d$HE_dbp - 0.051 * d$age - 0.098 * pp
d$htn_c <- ifelse(!is.na(d$HE_sbp) & !is.na(d$HE_dbp) & d$DI1_2 %in% c(1, 2, 3, 4, 5, 8),
                  as.numeric(d$sbp_c >= 140 | d$dbp_c >= 90 | d$DI1_2 %in% 1:4), NA)
# 공식 정의(전환식 미적용)로 HE_HP 재현 확인 (19세 이상)
d$htn_chk <- ifelse(!is.na(d$HE_sbp) & !is.na(d$HE_dbp) & d$DI1_2 %in% c(1, 2, 3, 4, 5, 8),
                    as.numeric(d$HE_sbp >= 140 | d$HE_dbp >= 90 | d$DI1_2 %in% 1:4), NA)
a19 <- d$age >= 19
cat("HE_HP 재산출 일치(19세 이상, 둘 다 비결측):",
    all(d$htn_chk[a19] == d$htn[a19], na.rm = TRUE), "\n")

# ---------------------------------------------------------------------
# 3. 분석대상 정의
# ---------------------------------------------------------------------
d$age30s <- d$age >= 30 & d$age <= 39
d$has_wt <- !is.na(d$wt_itvex) & d$wt_itvex > 0
d$cc     <- d$age30s & d$has_wt & !is.na(d$smoke) & !is.na(d$htn) &
            !is.na(d$sex_f) & !is.na(d$age) & !is.na(d$incm_f)

flow <- data.frame(
  단계 = c("2021 전체", "만 30~39세", "건강설문-검진 가중치(wt_itvex) 있음",
         "현재흡연(sm_presnt) 비결측", "고혈압(HE_HP) 비결측", "소득(incm) 비결측 = 최종 분석대상"),
  n = c(nrow(d), sum(d$age30s), sum(d$age30s & d$has_wt),
        sum(d$age30s & d$has_wt & !is.na(d$smoke)),
        sum(d$age30s & d$has_wt & !is.na(d$smoke) & !is.na(d$htn)),
        sum(d$cc))
)
print(flow)
write.csv(flow, file.path(out_dir, "table0_sample_flow.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# ---------------------------------------------------------------------
# 4. 복합표본 설계 객체
#    - 가중치 wt_itvex, 층 kstrata, 집락 psu
#    - 가중치가 없는(검진 미참여) 레코드만 제외한 전체 설계에서 subset()으로
#      30대 분석대상을 지정(부분모집단 분석) -> 분산추정 시 층·집락 구조 유지
# ---------------------------------------------------------------------
dw <- d[d$has_wt, ]
des_all <- svydesign(ids = ~psu, strata = ~kstrata, weights = ~wt_itvex,
                     data = dw, nest = TRUE)
des <- subset(des_all, cc)

# ---------------------------------------------------------------------
# 5. 기술통계: 현재흡연 여부별 특성 (비가중 n, 가중 % 또는 평균)
# ---------------------------------------------------------------------
fmt <- function(x, k = 1) formatC(x, format = "f", digits = k)
dc <- dw[dw$cc, ]

desc_rows <- list()
add_row <- function(var, lev, n_all, est_all, n0, est0, n1, est1) {
  desc_rows[[length(desc_rows) + 1]] <<- data.frame(
    변수 = var, 범주 = lev,
    전체_n = n_all, 전체_가중 = est_all,
    비흡연_과거흡연_n = n0, 비흡연_과거흡연_가중 = est0,
    현재흡연_n = n1, 현재흡연_가중 = est1, stringsAsFactors = FALSE)
}
# 전체 수
add_row("대상자 수", "", nrow(dc), "", sum(dc$smoke == 0), "", sum(dc$smoke == 1), "")
# 가중 % (SE)
wpct <- function(form, sub_expr) {
  dd <- if (is.null(sub_expr)) des else subset(des, eval(sub_expr))
  r <- svymean(form, dd, na.rm = TRUE)
  list(p = coef(r) * 100, se = SE(r) * 100)
}
cat_vars <- list(성별 = "sex_f", 소득4분위 = "incm_f")
for (nm in names(cat_vars)) {
  v <- cat_vars[[nm]]
  form <- as.formula(paste0("~", v))
  ra <- wpct(form, NULL); r0 <- wpct(form, quote(smoke == 0)); r1 <- wpct(form, quote(smoke == 1))
  for (lv in levels(dc[[v]])) {
    key <- paste0(v, lv)
    add_row(nm, lv,
            sum(dc[[v]] == lv), paste0(fmt(ra$p[key]), " (", fmt(ra$se[key]), ")"),
            sum(dc[[v]] == lv & dc$smoke == 0), paste0(fmt(r0$p[key]), " (", fmt(r0$se[key]), ")"),
            sum(dc[[v]] == lv & dc$smoke == 1), paste0(fmt(r1$p[key]), " (", fmt(r1$se[key]), ")"))
  }
}
# 나이 평균 (SE)
ma <- svymean(~age, des); m0 <- svymean(~age, subset(des, smoke == 0)); m1 <- svymean(~age, subset(des, smoke == 1))
add_row("나이(세), 평균(SE)", "", nrow(dc), paste0(fmt(coef(ma)), " (", fmt(SE(ma), 2), ")"),
        sum(dc$smoke == 0), paste0(fmt(coef(m0)), " (", fmt(SE(m0), 2), ")"),
        sum(dc$smoke == 1), paste0(fmt(coef(m1)), " (", fmt(SE(m1), 2), ")"))
# 고혈압 유병률 % (SE)
ha <- svymean(~htn, des); h0 <- svymean(~htn, subset(des, smoke == 0)); h1 <- svymean(~htn, subset(des, smoke == 1))
add_row("고혈압 유병", "있음",
        sum(dc$htn == 1), paste0(fmt(coef(ha) * 100), " (", fmt(SE(ha) * 100), ")"),
        sum(dc$htn == 1 & dc$smoke == 0), paste0(fmt(coef(h0) * 100), " (", fmt(SE(h0) * 100), ")"),
        sum(dc$htn == 1 & dc$smoke == 1), paste0(fmt(coef(h1) * 100), " (", fmt(SE(h1) * 100), ")"))
desc <- do.call(rbind, desc_rows)

# 흡연군 간 차이 검정 (Rao-Scott 카이제곱 / 가중 t검정)
p_sex  <- svychisq(~sex_f + smoke, des)$p.value
p_inc  <- svychisq(~incm_f + smoke, des)$p.value
p_age  <- svyttest(age ~ smoke, des)$p.value
p_htn  <- svychisq(~htn + smoke, des)$p.value
desc$p값 <- ""
desc$p값[desc$변수 == "성별"][1]   <- fmt(p_sex, 3)
desc$p값[desc$변수 == "소득4분위"][1] <- fmt(p_inc, 3)
desc$p값[desc$변수 == "나이(세), 평균(SE)"] <- fmt(p_age, 3)
desc$p값[desc$변수 == "고혈압 유병"] <- fmt(p_htn, 3)
print(desc)
write.csv(desc, file.path(out_dir, "table1_characteristics_by_smoking.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# 성별 층화 고혈압 유병률(참고)
sexstrat <- do.call(rbind, lapply(levels(dc$sex_f), function(s) {
  do.call(rbind, lapply(0:1, function(k) {
    sub <- subset(des, sex_f == s & smoke == k)
    r <- svymean(~htn, sub)
    data.frame(성별 = s, 현재흡연 = ifelse(k == 1, "예", "아니오"),
               n = sum(dc$sex_f == s & dc$smoke == k),
               고혈압_n = sum(dc$sex_f == s & dc$smoke == k & dc$htn == 1),
               고혈압_가중유병률_pct = unname(round(coef(r) * 100, 1)),
               SE = unname(round(as.numeric(SE(r)) * 100, 1)), row.names = NULL)
  }))
}))
print(sexstrat)
write.csv(sexstrat, file.path(out_dir, "table1b_htn_prev_by_sex_smoking.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# ---------------------------------------------------------------------
# 6. 복합표본 로지스틱 회귀
# ---------------------------------------------------------------------
or_tab <- function(fit, model_name) {
  b  <- coef(fit)
  ci <- confint(fit, df = degf(fit$survey.design))   # t분포(설계 자유도) 기반 95% CI
  p  <- summary(fit)$coefficients[, 4]
  data.frame(모형 = model_name, 항 = names(b),
             OR = round(exp(b), 2),
             CI_하한 = round(exp(ci[, 1]), 2), CI_상한 = round(exp(ci[, 2]), 2),
             p값 = signif(p, 3), row.names = NULL)
}

# 모형 1: 비보정
m1 <- svyglm(htn ~ smoke, design = des, family = quasibinomial())
# 모형 2: 성별·나이·소득 보정 (주분석)
m2 <- svyglm(htn ~ smoke + sex_f + age + incm_f, design = des, family = quasibinomial())

res <- rbind(or_tab(m1, "모형1_비보정"), or_tab(m2, "모형2_성별나이소득보정"))
res <- res[res$항 != "(Intercept)", ]
res$항 <- sub("^smoke$", "현재흡연(vs 비흡연·과거흡연)", res$항)
res$항 <- sub("^sex_f여자$", "여자(vs 남자)", res$항)
res$항 <- sub("^age$", "나이(1세 증가)", res$항)
res$항 <- sub("^incm_f(.*)$", "소득 \\1(vs 하)", res$항)
print(res)
write.csv(res, file.path(out_dir, "table2_logistic_OR.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# 소득 전체 효과 Wald 검정
wt_inc <- regTermTest(m2, ~incm_f)
cat("소득(incm) 전체 Wald F 검정 p =", signif(wt_inc$p, 3), "\n")

# ---------------------------------------------------------------------
# 7. 추가/민감도 분석
# ---------------------------------------------------------------------
# (a) 흡연×성별 상호작용 (효과수정 여부 탐색)
m_int <- svyglm(htn ~ smoke * sex_f + age + incm_f, design = des, family = quasibinomial())
p_int <- summary(m_int)$coefficients["smoke:sex_f여자", 4]
cat("흡연×성별 상호작용 p =", signif(p_int, 3), "\n")

# (b) 성별 층화 모형 (나이·소득 보정)
m_m <- svyglm(htn ~ smoke + age + incm_f, design = subset(des, sex_f == "남자"), family = quasibinomial())
m_f <- svyglm(htn ~ smoke + age + incm_f, design = subset(des, sex_f == "여자"), family = quasibinomial())

# (c) 2021년 혈압계 전환식 적용 고혈압 정의
des_c <- subset(des_all, cc & !is.na(htn_c))
m_c <- svyglm(htn_c ~ smoke + sex_f + age + incm_f, design = des_c, family = quasibinomial())

# (d) 흡연 3범주(비흡연/과거흡연/현재흡연)
#     BS1_1=3 -> 비흡연, BS1_1 in (1,2) & BS3_1=3 -> 과거흡연, sm_presnt=1 -> 현재흡연
#     BS1_1=1(평생 5갑 미만) & BS3_1 in (1,2,3)은 공식 정의상 현재흡연 아님 -> 비흡연으로 분류
dw$smk3 <- with(dw, ifelse(sm_presnt %in% 1, "현재흡연",
                     ifelse(BS1_1 %in% 2 & BS3_1 %in% 3, "과거흡연",
                     ifelse(BS1_1 %in% c(1, 3), "비흡연", NA))))
des_all3 <- svydesign(ids = ~psu, strata = ~kstrata, weights = ~wt_itvex, data = dw, nest = TRUE)
des3 <- subset(des_all3, cc)
des3 <- update(des3, smk3 = factor(smk3, levels = c("비흡연", "과거흡연", "현재흡연")))
m3 <- svyglm(htn ~ smk3 + sex_f + age + incm_f, design = des3, family = quasibinomial())
cat("흡연 3범주 n:\n"); print(table(dw$smk3[dw$cc], useNA = "always"))

sens <- rbind(
  or_tab(m_int, "추가_흡연x성별상호작용모형"),
  or_tab(m_m, "추가_남자층화(나이소득보정)"),
  or_tab(m_f, "추가_여자층화(나이소득보정)"),
  or_tab(m_c, "민감도_혈압계전환식적용고혈압"),
  or_tab(m3, "추가_흡연3범주")
)
sens <- sens[grepl("smoke|smk3", sens$항), ]
sens$n <- c(rep(nrow(dc), 2), sum(dc$sex_f == "남자"), sum(dc$sex_f == "여자"),
            sum(!is.na(dc$htn_c)), rep(nrow(dc), 2))
sens$고혈압_n <- c(rep(sum(dc$htn), 2), sum(dc$htn[dc$sex_f == "남자"]), sum(dc$htn[dc$sex_f == "여자"]),
                sum(dc$htn_c, na.rm = TRUE), rep(sum(dc$htn), 2))
print(sens)
write.csv(sens, file.path(out_dir, "table3_sensitivity.csv"), row.names = FALSE, fileEncoding = "UTF-8")

cat("전환식 적용 고혈압 n (분석대상):", sum(dc$htn_c, na.rm = TRUE), "/ 공식 HE_HP 고혈압 n:", sum(dc$htn), "\n")
cat("설계 자유도:", degf(des), "\n")

sink(file.path(out_dir, "model_summaries.txt"))
cat("### 모형1 비보정\n"); print(summary(m1))
cat("\n### 모형2 성별·나이·소득 보정\n"); print(summary(m2))
cat("\n소득 전체 Wald 검정\n"); print(wt_inc)
cat("\n### 흡연x성별 상호작용\n"); print(summary(m_int))
cat("\n### 남자 층화\n"); print(summary(m_m))
cat("\n### 여자 층화\n"); print(summary(m_f))
cat("\n### 전환식 적용 고혈압\n"); print(summary(m_c))
cat("\n### 흡연 3범주\n"); print(summary(m3))
sink()
