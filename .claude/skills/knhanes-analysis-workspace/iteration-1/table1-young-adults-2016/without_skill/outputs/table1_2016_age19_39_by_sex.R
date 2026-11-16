# table1_2016_age19_39_by_sex.R
# 목적: 국민건강영양조사 2016년(제7기 1차년도) 기본DB에서 만 19~39세를 대상으로
#       성별(남/여) Table 1을 만든다. 변수: 나이(age), 소득 4분위(incm), 교육수준(edu), BMI(HE_BMI)
#
# 변수 정의 근거 (국민건강영양조사 제7기(2016-2018) 원시자료 이용지침서, 인쇄 쪽수 기준)
#   - sex  : 1 남자, 2 여자                                   (p.85)
#   - age  : 만나이, 80세 이상 탑코딩                          (p.85)
#   - incm : 소득 4분위수(개인) 1 하, 2 중하, 3 중상, 4 상     (p.85)
#   - edu  : 교육수준 재분류 1 초졸이하, 2 중졸, 3 고졸, 4 대졸이상
#            (졸업은 현 학력, 수료·중퇴·재학/휴학은 이전 학력으로 분류) (p.86)
#   - educ : 원 문항, 99 = 모름·무응답 (p.145) -> 이 경우 edu가 결측(NA)으로 제공됨
#   - HE_BMI : 체질량지수 kg/m2, 생성변수 (p.189)
#              비만 유병(HE_obe) 정의는 '비임신자 중'으로 산출 (p.189)
#   - HE_prg : 임신여부 0 아니오, 1 예, 8 비해당(남자) (p.184)
#   - 설계변수: wt_itvex(건강설문-검진조사 가중치, p.86), kstrata(분산추정 층, p.86), psu(조사구번호, p.85)
#              복합표본 분석 방법 (p.31-33)
#
# 분석 결정
#   1) 복합표본설계 객체는 2016년 전체 자료(가중치가 있는 사람)로 만든 뒤 subset()으로
#      19~39세 하위집단(domain)을 잡는다. (하위집단만 잘라서 설계를 만들면 분산이 잘못 추정됨)
#   2) wt_itvex가 결측인 사람(건강설문·검진 미참여자)은 가중 추정에 쓸 수 없으므로 분석대상에서 제외한다.
#   3) BMI는 임신부(HE_prg = 1)를 제외하고 산출한다. 지침서의 비만 정의가 비임신자 기준이고(p.189),
#      임신 중 체중은 BMI 해석이 어렵기 때문이다. 임신부는 나이·소득·교육 행에는 포함한다.
#      (민감도 분석: 임신부 포함 BMI도 별도로 저장)
#   4) 9/99(모름·무응답): incm, edu 모두 원자료에서 이미 NA로 제공되어 별도 9 코드는 없음(확인함).
#      결측은 각 변수의 %에서 분모에서 빼고, 결측 n을 따로 적는다.
#   5) 비율(%)은 가중 비율, 연속형은 가중 평균(SE). n은 가중치 미적용 표본 수.
#   6) p값: 연속형은 설계기반 t검정(svyttest), 범주형은 Rao-Scott 보정 카이제곱(svychisq, F 통계량).
#
# 원시자료(data/)는 읽기만 한다.

suppressPackageStartupMessages({
  library(haven)
  library(data.table)
  library(survey)
})

options(survey.lonely.psu = "adjust")

data_file <- "C:/Users/kjh07/OneDrive/바탕 화면/knhanes/data/국건영 조사결과/hn16_all.sas7bdat"
out_dir   <- "C:/Users/kjh07/OneDrive/바탕 화면/knhanes/.claude/skills/knhanes-analysis-workspace/iteration-1/table1-young-adults-2016/without_skill/outputs"

# ---- 1. 자료 읽기 (필요한 변수만) ----
need <- c("year", "sex", "age", "incm", "edu", "educ", "HE_BMI", "HE_prg",
          "wt_itvex", "kstrata", "psu")
hdr  <- names(read_sas(data_file, n_max = 0))
want <- hdr[tolower(hdr) %in% tolower(need)]
d <- as.data.table(zap_labels(read_sas(data_file, col_select = all_of(want))))
setnames(d, tolower(names(d)))
stopifnot(all(d$year == 2016))

# ---- 2. 결측·코드 점검 ----
# 혹시 9/99 코드가 남아 있으면 결측 처리 (2016 자료에서는 없음을 확인함)
d[incm %in% 9,  incm := NA]
d[edu  %in% 9,  edu  := NA]
d[educ %in% 99, educ := NA]

# 표본 흐름 기록
flow <- data.table(
  step = c("2016 기본DB 전체", "만 19~39세", "만 19~39세 중 wt_itvex 있음 (분석대상)"),
  n    = c(nrow(d), d[age >= 19 & age <= 39, .N],
           d[age >= 19 & age <= 39 & !is.na(wt_itvex), .N])
)

# ---- 3. 분석용 변수 ----
d[, sex_f  := factor(sex, levels = 1:2, labels = c("남자", "여자"))]
d[, incm_f := factor(incm, levels = 1:4, labels = c("하", "중하", "중상", "상"))]
d[, edu_f  := factor(edu,  levels = 1:4, labels = c("초졸이하", "중졸", "고졸", "대졸이상"))]
d[, pregnant := (sex == 2 & he_prg %in% 1)]
d[, bmi      := fifelse(pregnant, NA_real_, he_bmi)]   # 주 분석: 임신부 제외 BMI
d[, bmi_all  := he_bmi]                                # 민감도: 임신부 포함
d[, young := age >= 19 & age <= 39]

# ---- 4. 복합표본 설계 (전체 자료 -> 하위집단) ----
dw  <- d[!is.na(wt_itvex)]
des <- svydesign(ids = ~psu, strata = ~kstrata, weights = ~wt_itvex,
                 data = dw, nest = TRUE)
sub <- subset(des, young)
dy  <- dw[young == TRUE]

# ---- 5. 표 계산 함수 ----
fmt_mean <- function(m, se, digits = 1) sprintf(paste0("%.", digits, "f (%.", digits + 1, "f)"), m, se)
fmt_pct  <- function(n, p, se) sprintf("%d (%.1f, SE %.1f)", n, 100 * p, 100 * se)
fmt_p    <- function(p) ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))

long <- list()   # 상세 결과(긴 형식)
rows <- list()   # 보고용 표

cont_row <- function(var, label, design, data, digits = 1) {
  f <- as.formula(paste0("~", var))
  tot <- svymean(f, design, na.rm = TRUE)
  by  <- svyby(f, ~sex_f, design, svymean, na.rm = TRUE)
  ci_t <- confint(tot); ci_b <- confint(by)
  tt  <- svyttest(as.formula(paste0(var, "~sex_f")), design)
  n_t <- sum(!is.na(data[[var]]))
  n_b <- data[!is.na(get(var)), .N, keyby = sex_f]$N
  miss <- c(sum(is.na(data[[var]])), data[, sum(is.na(get(var))), keyby = sex_f]$V1)
  long[[length(long) + 1]] <<- data.table(
    variable = var, level = "mean", group = c("전체", "남자", "여자"),
    n_unweighted = c(n_t, n_b), n_missing = miss,
    estimate = c(coef(tot), coef(by)), se = c(SE(tot), SE(by)),
    ci_low = c(ci_t[1], ci_b[, 1]), ci_high = c(ci_t[2], ci_b[, 2]),
    p_value = unname(tt$p.value))
  data.table(특성 = label,
             전체 = fmt_mean(coef(tot), SE(tot), digits),
             남자 = fmt_mean(coef(by)[1], SE(by)[1], digits),
             여자 = fmt_mean(coef(by)[2], SE(by)[2], digits),
             p값 = fmt_p(tt$p.value),
             결측n = sprintf("%d / %d / %d", miss[1], miss[2], miss[3]))
}

cat_rows <- function(var, label, design, data) {
  f <- as.formula(paste0("~", var))
  dsg <- subset(design, !is.na(get(var)))
  tot <- svymean(f, dsg)
  by  <- svyby(f, ~sex_f, dsg, svymean)
  ci_t <- confint(tot)
  ch  <- svychisq(as.formula(paste0("~", var, "+sex_f")), dsg, statistic = "F")
  lv  <- levels(data[[var]])
  nt  <- table(data[[var]])
  nb  <- table(data[[var]], data$sex_f)
  miss <- c(sum(is.na(data[[var]])), data[, sum(is.na(get(var))), keyby = sex_f]$V1)
  # svyby 결과(data.frame)에서 열 이름으로 추정치와 SE를 꺼낸다 (행: 남자, 여자 / 열: 범주)
  est_cols <- paste0(var, lv)
  est_b <- as.matrix(as.data.frame(by)[, est_cols])
  se_b  <- as.matrix(as.data.frame(by)[, paste0("se.", est_cols)])
  stopifnot(identical(as.character(by$sex_f), c("남자", "여자")))
  ci_b  <- confint(by)   # 행 이름: "남자:incm_f하" 형식
  for (j in seq_along(lv)) {
    rn <- paste0(c("남자", "여자"), ":", est_cols[j])
    long[[length(long) + 1]] <<- data.table(
      variable = var, level = lv[j], group = c("전체", "남자", "여자"),
      n_unweighted = c(nt[j], nb[j, 1], nb[j, 2]), n_missing = miss,
      estimate = c(coef(tot)[j], est_b[, j]), se = c(SE(tot)[j], se_b[, j]),
      ci_low = c(ci_t[j, 1], ci_b[rn, 1]),
      ci_high = c(ci_t[j, 2], ci_b[rn, 2]),
      p_value = unname(ch$p.value))
  }
  head <- data.table(특성 = label, 전체 = "", 남자 = "", 여자 = "",
                     p값 = fmt_p(ch$p.value),
                     결측n = sprintf("%d / %d / %d", miss[1], miss[2], miss[3]))
  body <- rbindlist(lapply(seq_along(lv), function(j) data.table(
    특성 = paste0("  ", lv[j]),
    전체 = fmt_pct(as.integer(nt[j]), coef(tot)[j], SE(tot)[j]),
    남자 = fmt_pct(as.integer(nb[j, 1]), est_b[1, j], se_b[1, j]),
    여자 = fmt_pct(as.integer(nb[j, 2]), est_b[2, j], se_b[2, j]),
    p값 = "", 결측n = "")))
  rbind(head, body)
}

# ---- 6. Table 1 ----
n_row <- data.table(특성 = "표본 수, n (가중치 미적용)",
                    전체 = as.character(nrow(dy)),
                    남자 = as.character(dy[sex == 1, .N]),
                    여자 = as.character(dy[sex == 2, .N]),
                    p값 = "", 결측n = "")
pop_tot <- svytotal(~sex_f, sub)
popw_row <- data.table(특성 = "추정 모집단 수 (가중 합, 천 명)",
                       전체 = sprintf("%.0f", sum(coef(pop_tot)) / 1000),
                       남자 = sprintf("%.0f", coef(pop_tot)[1] / 1000),
                       여자 = sprintf("%.0f", coef(pop_tot)[2] / 1000),
                       p값 = "", 결측n = "")
pct_sex <- svymean(~sex_f, sub)
pctw_row <- data.table(특성 = "가중 비율, %", 전체 = "100.0",
                       남자 = sprintf("%.1f", 100 * coef(pct_sex)[1]),
                       여자 = sprintf("%.1f", 100 * coef(pct_sex)[2]),
                       p값 = "", 결측n = "")

table1 <- rbind(
  n_row, popw_row, pctw_row,
  cont_row("age", "나이, 세, 가중평균 (SE)", sub, dy),
  cat_rows("incm_f", "소득 4분위(개인, incm), n (가중 %, SE)", sub, dy),
  cat_rows("edu_f", "교육수준(edu), n (가중 %, SE)", sub, dy),
  cont_row("bmi", "BMI, kg/m², 가중평균 (SE) [임신부 제외]", sub, dy)
)

# 민감도: 임신부 포함 BMI
sens <- cont_row("bmi_all", "BMI, kg/m², 가중평균 (SE) [임신부 포함]", sub, dy)

long_dt <- rbindlist(long)
long_dt[, `:=`(estimate = round(estimate, 4), se = round(se, 4),
               ci_low = round(ci_low, 4), ci_high = round(ci_high, 4),
               p_value = signif(p_value, 4))]

# ---- 7. 저장 ----
fwrite(table1,  file.path(out_dir, "table1_2016_age19_39_by_sex.csv"), bom = TRUE)
fwrite(long_dt, file.path(out_dir, "table1_2016_age19_39_by_sex_detail.csv"), bom = TRUE)
fwrite(rbind(flow,
             data.table(step = "분석대상 중 임신부 (BMI에서 제외)", n = dy[pregnant == TRUE, .N])),
       file.path(out_dir, "sample_flow.csv"), bom = TRUE)
fwrite(sens, file.path(out_dir, "sensitivity_bmi_incl_pregnant.csv"), bom = TRUE)

# ---- 8. 콘솔 출력 ----
options(width = 200)
cat("\n=== 표본 흐름 ===\n"); print(flow)
cat("임신부 수(분석대상 중):", dy[pregnant == TRUE, .N], "\n")
cat("\n=== Table 1 ===\n"); print(table1)
cat("\n=== 민감도: 임신부 포함 BMI ===\n"); print(sens)
cat("\n=== educ x edu 교차 (분석대상, 가중치 미적용) ===\n"); print(table(educ = dy$educ, edu = dy$edu, useNA = "ifany"))
cat("\n사용 환경:", R.version.string, "/ survey", as.character(packageVersion("survey")),
    "/ haven", as.character(packageVersion("haven")),
    "/ data.table", as.character(packageVersion("data.table")), "\n")
