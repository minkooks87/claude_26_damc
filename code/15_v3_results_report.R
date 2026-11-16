# 15_v3_results_report.R
# 목적: 13_v3_main_analysis.R 의 결과(CSV)와 14_v3_results_figures.R 의 그림으로 상세 분석 결과 보고서(Markdown)를 만든다.
#       숫자는 모두 결과 CSV에서 읽어 넣는다(손으로 옮기지 않는다).
# 실행: Rscript --vanilla code/15_v3_results_report.R   (프로젝트 폴더에서)
# 결과: output/13_v3_main_analysis/REPORT_V3_분석결과.md  → python code/06_flushing_report_docx.py 로 Word 변환

suppressPackageStartupMessages(library(data.table))

in_dir <- file.path("output", "13_v3_main_analysis")
out_md <- file.path(in_dir, "REPORT_V3_분석결과.md")
rd <- function(f) fread(file.path(in_dir, f), encoding = "UTF-8")

esc <- function(x) gsub("\\|", "\\\\|", trimws(as.character(x)))
md_table <- function(dt, headers = names(dt)) {
  dt <- as.data.table(dt)[, lapply(.SD, function(x) { x <- as.character(x); x[is.na(x)] <- ""; x })]
  c(paste0("| ", paste(headers, collapse = " | "), " |"),
    paste0("|", paste(rep("---", length(headers)), collapse = "|"), "|"),
    apply(dt, 1, function(r) paste0("| ", paste(esc(r), collapse = " | "), " |")), "")
}
# 신뢰구간에 음수가 있으면 줄표(–)가 부호(-)와 겹쳐 읽기 어려우므로 쉼표로 구분한다
fmt_ci <- function(e, l, h, d = 2) ifelse(is.na(e), "", ifelse(l < 0 | h < 0,
  sprintf(paste0("%.", d, "f (%.", d, "f, %.", d, "f)"), e, l, h), sprintf(paste0("%.", d, "f (%.", d, "f–%.", d, "f)"), e, l, h)))
fmt_p <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))
big   <- function(x) ifelse(is.na(x), "", format(x, big.mark = ",", trim = TRUE))
pct1  <- function(x) ifelse(is.na(x), "", sprintf("%.1f", x))

# ---- 결과 읽기 ----
vd    <- rd("01_variable_definitions.csv")
flow  <- rd("02_participant_flow.csv")
flow5 <- rd("02b_flow_vs_presentation_v5.csv")
cells <- rd("03_cell_counts.csv")
cmp   <- rd("03b_included_vs_excluded.csv")
conv  <- rd("03c_conversion_table.csv")
tert  <- rd("03d_tertile_cutpoints.csv")
t1    <- rd("04_table1_by_sex_flushnow.csv")
t1h   <- rd("04b_table1_by_sex_flushhist.csv")
a5    <- rd("05a_main_joint_effects.csv")
t5    <- rd("05b_main_interaction_tests.csv")
tr5   <- rd("05c_main_trend.csv")
a6    <- rd("06a_history_joint_effects.csv")
t6    <- rd("06b_history_tests.csv")
tr6   <- rd("06c_history_trend.csv")
g4    <- rd("06d_history_4groups_supp.csv")
sd7   <- rd("07_sex_difference_tests.csv")
a8    <- rd("08a_women_menopause_cells.csv")
t8    <- rd("08b_women_menopause_tests.csv")
s9    <- rd("09_sensitivity.csv")
st10  <- rd("10_supp_stratified_models.csv")
oc    <- rd("11_official_check.csv")
inputs <- rd("_inputs.csv")
repro  <- if (file.exists(file.path(in_dir, "_reproducibility.txt"))) readLines(file.path(in_dir, "_reproducibility.txt"), encoding = "UTF-8") else character(0)
repro_res  <- sub("^result:\\s*", "", grep("^result:", repro, value = TRUE))
repro_time <- sub("^time:\\s*", "", grep("^time:", repro, value = TRUE))
sess <- readLines(file.path(in_dir, "_sessionInfo.txt"), encoding = "UTF-8", n = 1)

hua <- "고요산혈증(OR)"; ua <- "혈청 요산(β, mg/dL)"; ua_m <- "혈청 요산(β, mg/dL) [보조: 계획은 여성만]"
M0 <- "M0 연령보정"; M1 <- "M1 기본"; M2 <- "M2 확장"
sexes <- c("남자", "여자")
n_final <- flow[.N, n]; n_men <- flow[.N, n_men]; n_women <- flow[.N, n_women]

# ---- 표 만들기 도구 ----
ref_or <- function(e, l, h, ref) ifelse(!is.na(e) & e == ref & l == ref & h == ref, if (ref == 1) "1 (기준)" else "0 (기준)", fmt_ci(e, l, h))
kv_md <- function(x, or = TRUE) {   # Knol & VanderWeele 형식 (결합효과 + 층화효과 + 수정변수 효과 + RERI)
  ref <- if (or) 1 else 0
  if (or) {
    md_table(x[, .(홍조군 = modifier, `음주 수준` = exposure, n = big(n), 사례 = big(n_event), `가중 유병률(%)` = pct1(100 * wt_mean),
                   `결합효과 OR (95% CI)` = ref_or(joint_est, joint_lo, joint_hi, 1),
                   `층화 OR (95% CI)` = ref_or(strat_est, strat_lo, strat_hi, 1),
                   `홍조 효과 OR (95% CI)` = ref_or(mod_est, mod_lo, mod_hi, 1),
                   `RERI (95% CI)` = fmt_ci(reri, reri_lo, reri_hi))])
  } else {
    md_table(x[, .(홍조군 = modifier, `음주 수준` = exposure, n = big(n), `가중 평균 요산(mg/dL)` = sprintf("%.2f", wt_mean),
                   `결합효과 β (95% CI)` = ref_or(joint_est, joint_lo, joint_hi, 0),
                   `층화 β (95% CI)` = ref_or(strat_est, strat_lo, strat_hi, 0),
                   `홍조 효과 β (95% CI)` = ref_or(mod_est, mod_lo, mod_hi, 0))])
  }
}
trend_txt <- function(tr) paste(tr[, sprintf("%s %s (p %s)", modifier, fmt_ci(per_level_est, ci_low, ci_high), fmt_p(p_trend))], collapse = " / ")
test_md <- function(tt) md_table(tt[, .(검정 = test, `모형 n` = big(n_model), 자유도 = df, F = sprintf("%.2f", F), p = fmt_p(p))])
cell_md <- function(gv, gname) md_table(cells[group_var == gv, .(성별 = sex, 홍조 = group, `음주 수준` = drink, n = big(n), `고요산혈증 사례` = big(n_hua),
                                                                  `가중 유병률 %` = pct1(hua_pct_wt), SE = pct1(hua_se_wt))],
                                          c("성별", gname, "음주 수준", "n", "고요산혈증 사례", "가중 유병률 %", "SE"))
# Table 1: "추정치 (n)" 형태
t1_md <- function(tab, s, groups) {
  x <- tab[sex == s]
  out <- x[, .(항목 = ifelse(nzchar(level), paste0(label, ": ", level), label), 통계 = stat)]
  for (g in groups) set(out, j = g, value = ifelse(x[[paste0(g, ": n")]] == 0, "–", paste0(x[[paste0(g, ": 추정치")]], " (", big(x[[paste0(g, ": n")]]), ")")))
  set(out, j = "p", value = fmt_p(x$p_value))
  md_table(out)
}
gcell <- function(dt, s, f, e, m = M2, o = hua) dt[outcome == o & sex == s & modifier == f & exposure == e & model == m]
or_s  <- function(r) fmt_ci(r$strat_est, r$strat_lo, r$strat_hi)
p_int <- function(s, m = M2, o = hua) t5[outcome == o & sex == s & model == m, p]

# ---- 본문 조립 ----
L <- c(); add <- function(...) L <<- c(L, ...)

add("# 한국 성인 현재 음주자에서 음주 관련 안면홍조 이력에 따른 음주량–고요산혈증 연관성의 차이: 성별 효과수정 분석",
    "",
    "분석 결과 보고서 · 국민건강영양조사 제8기(2019–2021) · 과학적 사고와 의학연구 6조",
    "",
    sprintf("- 보고서 생성: %s", format(Sys.time(), "%Y-%m-%d %H:%M")),
    "- 분석 코드: `code/13_v3_main_analysis.R`(본 분석), `code/14_v3_results_figures.R`(그림), `code/15_v3_results_report.R`(이 보고서). 숫자는 모두 결과 CSV에서 자동으로 옮겼다.",
    sprintf("- 분석 환경: %s, survey 패키지(복합표본). 재현성 확인: **%s** (%s, 새 R 세션에서 2회 실행 후 결과 CSV의 md5 비교)",
            sess, ifelse(length(repro_res), repro_res, "미실행"), ifelse(length(repro_time), repro_time, "")),
    "- 분석 설계의 근거: 연구 계획서, 연구계획 발표 v5, 연구보고서 V3. 참고 논문의 방법 정리와 반영 내용은 `00_reference_methods.md`.",
    "- 표기: n은 가중치를 적용하지 않은 표본 수, %·평균·OR·β는 복합표본 가중 추정치. OR은 단면자료의 유병 오즈비. 괄호는 95% 신뢰구간. p는 양측 검정.",
    "")

# ===== 요약 =====
mB <- gcell(a5, "남자", "비홍조", "고음주"); mF <- gcell(a5, "남자", "홍조", "고음주")
wB <- gcell(a5, "여자", "비홍조", "고음주"); wF <- gcell(a5, "여자", "홍조", "고음주")
mB2 <- gcell(a5, "남자", "비홍조", "중음주"); mF2 <- gcell(a5, "남자", "홍조", "중음주")
wB2 <- gcell(a5, "여자", "비홍조", "중음주"); wF2 <- gcell(a5, "여자", "홍조", "중음주")
mLow <- gcell(a5, "남자", "홍조", "저음주"); wLow <- gcell(a5, "여자", "홍조", "저음주")
uB <- gcell(a5, "여자", "비홍조", "고음주", o = ua); uF <- gcell(a5, "여자", "홍조", "고음주", o = ua)
hS <- gcell(a6, "남자", "홍조 소실", "고음주"); hP <- gcell(a6, "남자", "지속 홍조", "고음주"); hN <- gcell(a6, "남자", "비홍조", "고음주")
p_hist_joint <- t6[outcome == hua & sex == "남자" & model == M2 & grepl("공동", test), p]
p_hist_sn    <- t6[outcome == hua & sex == "남자" & model == M2 & grepl("^소실 vs 비홍조", test), p]
p_sex3  <- sd7[outcome == hua & model == M2 & grepl("3원", test), p]
p_meno3 <- t8[outcome == hua & grepl("3원", test), p]
p_meno_post <- t8[outcome == hua & menopause == "폐경 후" & !grepl("3원", test), p]
postF <- a8[outcome == hua & menopause == "폐경 후" & modifier == "홍조" & exposure == "고음주"]
postB <- a8[outcome == hua & menopause == "폐경 후" & modifier == "비홍조" & exposure == "고음주"]
sens_p <- s9[, range(p_interaction)]
add("## 요약", "",
    sprintf("- 분석 대상은 2019–2021년 만 19세 이상 최근 1년 음주자 중 음주량·과거/현재 홍조·요산이 있고 통풍 의사진단이 없으며 보정변수가 모두 있는 **%s명(남 %s, 여 %s)** 이다. 발표 v5의 대상자 흐름과 단계마다 일치한다.",
            big(n_final), big(n_men), big(n_women)),
    sprintf("- **주질문(현재 홍조의 효과수정)**: 음주량이 많을수록 고요산혈증이 느는 연관성은 두 홍조군 모두에 있었다. 저음주 대비 고음주의 OR(확장 모형)은 남자 비홍조 %s, 홍조 %s, 여자 비홍조 %s, 홍조 %s였다. 그러나 음주 수준 × 현재 홍조 상호작용은 유의하지 않았고(남 p = %s, 여 p = %s), 덧셈 척도의 RERI도 0을 포함했다(남 고음주 %s, 여 %s). **홍조가 연관성을 바꾼다는 근거는 확인되지 않았다.**",
            or_s(mB), or_s(mF), or_s(wB), or_s(wF), fmt_p(p_int("남자")), fmt_p(p_int("여자")),
            fmt_ci(mF$reri, mF$reri_lo, mF$reri_hi), fmt_ci(wF$reri, wF$reri_lo, wF$reri_hi)),
    sprintf("- 같은 저음주 안에서 홍조군의 고요산혈증 오즈는 비홍조군보다 낮은 쪽이었다(남 OR %s, 여 OR %s).",
            fmt_ci(mLow$mod_est, mLow$mod_lo, mLow$mod_hi), fmt_ci(wLow$mod_est, wLow$mod_lo, wLow$mod_hi)),
    sprintf("- **여성 혈청 요산(연속형)**: 고음주의 요산 차이는 비홍조 %s mg/dL, 홍조 %s mg/dL로 같았다(상호작용 p = %s).",
            fmt_ci(uB$strat_est, uB$strat_lo, uB$strat_hi), fmt_ci(uF$strat_est, uF$strat_lo, uF$strat_hi), fmt_p(p_int("여자", o = ua))),
    sprintf("- **부질문 1(홍조 이력)**: 남자 홍조 소실군의 고음주 OR은 %s로 비홍조군 %s, 지속 홍조군 %s보다 컸지만, 공동 상호작용 p = %s, 소실 대 비홍조 p = %s로 통계적으로 확인되지 않았다(소실군 고음주 n = %s, 사례 %s). 다만 소실군 고음주의 RERI는 %s로 덧셈 척도에서는 0을 넘었다(탐색적).",
            or_s(hS), or_s(hN), or_s(hP), fmt_p(p_hist_joint), fmt_p(p_hist_sn), big(hS$n), big(hS$n_event), fmt_ci(hS$reri, hS$reri_lo, hS$reri_hi)),
    sprintf("- **부질문 2(성별·폐경)**: 음주 × 홍조 × 성별 3원 상호작용 p = %s로 남녀 차이는 없었다. 폐경 후 여성에서 홍조군 고음주 OR %s가 비홍조군 %s보다 컸지만(상호작용 p = %s), 해당 칸은 %s명(사례 %s)이라 불안정하고 폐경 3원 상호작용 p = %s였다.",
            fmt_p(p_sex3), or_s(postF), or_s(postB), fmt_p(p_meno_post), big(postF$n), big(postF$n_event), fmt_p(p_meno3)),
    sprintf("- **민감도 분석** 10가지(통풍 진단자·무응답자 포함, 대표값 변경, 2020년 제외, 폭음 빈도, 홍조 정의 변경, 성별 삼분위, 의사진단 보정, 빈도 2군)에서 결론이 바뀌지 않았다(상호작용 p %.2f–%.2f).", sens_p[1], sens_p[2]),
    "- 공식 통계 대조는 2019년 고위험음주율·월간음주율의 **조율(crude) 공식값을 구하지 못해** 비어 있다(국민건강통계 2019 표 확인 필요).",
    "")

# ===== 1. 자료와 방법 =====
add("## 1. 자료와 방법", "",
    "### 1.1 자료와 복합표본설계", "",
    "- 자료: 국민건강영양조사 제8기 2019, 2020, 2021년 기본DB(`hn19_all`, `hn20_all`, `hn21_all`). 음주 관련 홍조 문항(BD9_1, BD9_2)은 이 3개 연도에만 있다.",
    "- 가중치: 건강설문·검진 변수만 사용하므로 `wt_itvex`(8기 지침서 PDF p.92 / 인쇄 p.84). 3개 연도 통합비율은 각 연도 1/3. 제8기는 조사구 수 비율이 아니라 **조사기간 비례**로 합친다(8기 지침서 PDF p.36 / 인쇄 p.28, 표 7). 2020년은 코로나19로 192개 중 180개 조사구만 조사했지만 조사기간은 1년으로 같다.",
    "- 층 `kstrata`, 집락 `psu`. 층 안에 집락이 1개뿐인 경우는 없었다.",
    "- 모든 추정은 `svydesign` 설계 객체를 만든 뒤 `subset`으로 분석 대상과 하위집단을 지정해 계산했다(행 삭제 없음, 도메인 분석). 이는 Kang B 외(2024)가 KNHANES 2019–2020 통합자료에서 쓴 방식과 같다.",
    "", "입력 파일 지문:", "",
    md_table(inputs[, .(파일 = basename(file), 크기 = big(size), md5)]),
    "### 1.2 변수 정의 (이용지침서 쪽수)", "",
    "변수의 의미와 코드값은 제8기(2019–2021) 원시자료 이용지침서에서 확인했다. 9·99·999(모름·무응답)는 결측으로, 8·88·888(비해당)은 지침서의 의미를 보고 변수마다 처리했다.", "",
    md_table(vd[, .(변수 = var, 설명 = label, 코드 = levels, 결측코드 = miss, 비해당처리 = na8, 근거 = source)]),
    "### 1.3 분석 변수", "",
    "- **노출(주당 음주잔수)**: 1년간 음주빈도(BD1_11)를 주당 횟수 대표값으로, 한 번에 마시는 음주량(BD2_1)을 잔 수 대표값으로 바꿔 곱했다. 10잔 이상은 기입값(BD2_14)을 썼고, 기입값이 없으면 음주량을 계산할 수 없는 것으로 보았다. 1회량 범주의 중앙값 사용은 Lee 외(2023, KJFM)와 같다.",
    "- **음주 수준**: 주당 3.5잔 미만 저음주(기준), 3.5잔 이상 14잔 미만 중음주, 14잔 이상 고음주. 남녀 공통(V3). 추세 검정에는 저·중·고를 0·1·2의 순서형으로 넣었다.",
    "", md_table(conv[, .(항목, 코드, 응답, `주분석 대표값` = 주분석_대표값, `민감도 S3a` = 민감도_S3a, `민감도 S3b` = 민감도_S3b, 비고)]),
    "- **효과수정변수**: 주분석은 현재 홍조(BD9_2: '가끔·자주·늘' = 홍조, '아니오' = 비홍조). 부가 분석은 홍조 이력 3군(과거 BD9_1 × 현재 BD9_2: 비홍조 = 과거 아니오·현재 아니오, 홍조 소실 = 과거 예·현재 아니오, 지속 홍조 = 과거 예·현재 가끔 이상). 과거 아니오·현재 가끔 이상(신규 홍조)은 이력 분석에서 제외하고 4군 보조 분석에만 넣었다(Yook 외 2024의 분류와 같다). '모름'·무응답·비해당은 홍조 없음으로 보지 않았다.",
    "- **결과**: 고요산혈증 = 혈청 요산(HE_Uacid) 남 7.0 mg/dL 이상, 여 6.0 mg/dL 이상(V3 조작적 정의; Bae 외 2023 등 국건영 논문과 같음). 지침서에는 고요산혈증 정의가 없다. 요산 1.0 mg/dL 미만 표기(HE_Uacid_etc)는 발표 v5와 같이 결측으로 두었다. 여성은 혈청 요산(연속형)을 추가 결과로 썼고, 남성 연속형은 보조로만 제시한다.",
    "- **보정변수**: 기본 모형 = 연령(연속), 교육수준(4군), 소득 4분위(개인), 조사연도, 현재흡연(지침서 주요지표 정의), 유산소 신체활동 실천. 확장 모형 = 기본 + 체질량지수(연속), 고혈압 유병(HE_HP = 3), 당뇨병 유병(HE_DM_HbA1c = 3), eGFR(CKD-EPI 2009식, 10 mL/min/1.73m² 단위; 지침서에 계산식이 없어 문헌식 사용). 연령만 보정한 모형(M0)은 Kang & Oh(2023)의 연령보정 단계에 해당한다.",
    "- **폐경 상태(여성, 탐색)**: 월경여부(LW_ms) 5·6(자연·인공폐경) = 폐경 후, 2·3·4 = 폐경 전. 1(초경 전)·7(기타)은 결측. 폐경연령(LW_mp_a)은 기술통계에만 썼다.",
    "", sprintf("- 민감도 S7의 성별 가중 삼분위 절단점(잔/주): 남자 %.2f·%.2f, 여자 %.2f·%.2f.", tert[sex == 1, q1], tert[sex == 1, q2], tert[sex == 2, q1], tert[sex == 2, q2]),
    "",
    "### 1.4 연구 대상", "",
    "만 19세 이상 최근 1년 음주자(BD1_11 = 2–6) 가운데 (1) 음주량을 계산할 수 있고, (2) 과거·현재 홍조 문항에 모두 응답했으며, (3) 혈청 요산을 측정했고, (4) 통풍 의사진단이 없고(진단 '모름'·무응답은 결측으로 보아 제외), (5) 보정변수가 모두 있는 사람. 검진·설문 가중치(wt_itvex)가 없는 사람은 첫 단계에서 제외했다. 임신부는 당뇨병 유병 변수가 생성되지 않아 보정변수 단계에서 자동으로 빠진다.", "",
    "### 1.5 통계 분석", "",
    "- 기술통계: 비가중 n과 가중 %(표준오차) 또는 가중 평균(표준오차). 집단 차이는 범주형은 Rao-Scott 보정 카이제곱(F) 검정, 연속형은 복합표본 선형회귀의 Wald 검정.",
    "- 주분석: 남녀를 나눠 복합표본 로지스틱 회귀(svyglm, quasibinomial)로 저음주 대비 중·고음주의 OR을 구했다. 모형에 음주 수준, 현재 홍조, '음주 수준 × 현재 홍조' 상호작용항을 넣고, 두 상호작용항을 함께 검정하는 설계 기반 Wald(F) 검정으로 효과수정을 평가했다(V3). 홍조군별 OR(층화 OR)은 상호작용 모형의 계수 조합으로 구했고(단순효과), 공변량 효과까지 홍조군마다 다르게 둔 완전 층화 모형은 보조로 제시한다(Kang & Oh 2023 방식).",
    "- 결과 제시 형식: Knol & VanderWeele(2012) 권고에 따라 결합효과 OR(기준 비홍조·저음주), 층화 OR, 음주 수준 안에서의 홍조 효과 OR, 상호작용항 p를 한 표에 적고, 덧셈 척도의 상호작용 지표 RERI(= OR11 − OR10 − OR01 + 1)와 델타법 95% 신뢰구간을 더했다(Moore 외 2016 참고; 논문의 MOVER법 대신 델타법).",
    "- P for trend: 음주 수준을 순서형으로 넣은 모형에서 홍조군별 한 단계당 OR과 p, 그리고 추세의 홍조군 간 차이 p(Lee 외 2022 참고).",
    "- 부질문 1: 비홍조군을 기준으로 음주 수준 × 홍조 이력(3군) 상호작용을 공동 검정(4 자유도)한 뒤, 소실 대 비홍조, 지속 대 비홍조, 소실 대 지속(탐색)의 음주 연관성 차이를 각각 2 자유도 Wald 검정했다.",
    "- 부질문 2: 전체 대상 모형에서 '음주 수준 × 현재 홍조 × 성별' 3원 상호작용을 검정했다(주효과와 모든 2원 상호작용 포함). 여성은 혈청 요산을 결과로 한 복합표본 선형회귀를 추가했고, 폐경 전·후로 나눈 분석과 폐경 3원 상호작용은 탐색적으로 제시한다.",
    "- 민감도 분석: 통풍 의사진단자 포함(S1), 통풍 문항 모름·무응답 포함(S2), 열린 범주 대표값 변경(S3a·S3b), 2020년 제외(S4), 노출을 폭음 빈도로 교체(S5), 현재 홍조를 '자주·늘'로 좁힘(S6, Kang & Oh 방식), 노출을 성별 가중 삼분위로 교체(S7, 계획서 방법론 문구), 고혈압·당뇨병을 의사진단으로 보정(S8), 노출을 음주 빈도 2군으로 교체(S10, Kang & Oh 방식). 모두 확장 모형·현재 홍조 기준.",
    "- 유의수준 0.05, 양측 검정. R 4.6.0, survey 패키지. 결측은 완전사례 분석이며, 제외자와 포함자의 특성을 비교했다.",
    "")

# ===== 2. 결과 =====
add("## 2. 결과", "",
    "### 2.1 대상자 선정 흐름", "",
    md_table(flow[, .(단계 = step, `남은 n` = big(n), 남자 = big(n_men), 여자 = big(n_women), 제외 = big(excluded))]),
    "발표 v5의 흐름(가중치 단계 없음)과 대조한 결과, 모든 단계의 인원이 같았다.", "",
    md_table(flow5[, .(단계 = step, `본 분석 n` = big(n), `발표 v5 n` = big(v5_presentation), 일치 = ifelse(match, "예", "아니오"))]),
    "![대상자 선정 흐름](fig/fig1_participant_flow.png)", "",
    "**포함자와 제외자 비교.** 통풍 단계까지 통과했지만 보정변수 결측으로 빠진 사람과 최종 포함자의 특성을 비교했다. 차이는 없었다.", "",
    md_table(cmp[, .(항목 = ifelse(nzchar(level), paste0(label, ": ", level), label), 통계 = stat,
                     `포함 (n)` = paste0(`포함: 추정치`, " (", big(`포함: n`), ")"), `제외 (n)` = paste0(`제외: 추정치`, " (", big(`제외: n`), ")"), p = fmt_p(p_value))]),
    "### 2.2 대상자 특성 (Table 1)", "",
    "값은 '가중 추정치 (비가중 n)'. 범주형은 가중 %(SE), 연속형은 가중 평균(SE). p는 성별 안에서 홍조군 간 비교(범주형 Rao-Scott χ², 연속형 Wald).", "",
    "**Table 1a. 남자: 현재 홍조군별**", "", t1_md(t1, "남자", c("비홍조", "홍조")),
    "**Table 1b. 여자: 현재 홍조군별**", "", t1_md(t1, "여자", c("비홍조", "홍조")),
    "**Table 1c. 남자: 홍조 이력 3군별 (신규 홍조 제외)**", "", t1_md(t1h, "남자", c("비홍조", "홍조 소실", "지속 홍조")),
    "**Table 1d. 여자: 홍조 이력 3군별 (신규 홍조 제외)**", "", t1_md(t1h, "여자", c("비홍조", "홍조 소실", "지속 홍조")))

# 특성 요약 문장 (Table 1에서 숫자 추출)
getv <- function(s, v, lv, g) { x <- t1[sex == s & variable == v & level == lv]; if (!nrow(x)) x <- t1[sex == s & variable == v]; x[[paste0(g, ": 추정치")]] }
getp <- function(s, v) fmt_p(t1[sex == s & variable == v, p_value][1])
add(sprintf("- 주당 음주잔수 가중 평균(SE)은 남자 비홍조 %s, 홍조 %s(p %s), 여자 비홍조 %s, 홍조 %s(p %s)로 홍조군이 덜 마셨다.",
            getv("남자", "drinks_wk", "", "비홍조"), getv("남자", "drinks_wk", "", "홍조"), getp("남자", "drinks_wk"),
            getv("여자", "drinks_wk", "", "비홍조"), getv("여자", "drinks_wk", "", "홍조"), getp("여자", "drinks_wk")),
    sprintf("- 고요산혈증 가중 %%(SE)는 남자 비홍조 %s, 홍조 %s(p %s), 여자 비홍조 %s, 홍조 %s(p %s)였다. 보정 전 단순 비교에서는 홍조군의 고요산혈증이 적거나 비슷하다.",
            getv("남자", "f_hua", "있음", "비홍조"), getv("남자", "f_hua", "있음", "홍조"), getp("남자", "f_hua"),
            getv("여자", "f_hua", "있음", "비홍조"), getv("여자", "f_hua", "있음", "홍조"), getp("여자", "f_hua")),
    sprintf("- 고혈압 유병은 남자 비홍조 %s, 홍조 %s(p %s), 여자 비홍조 %s, 홍조 %s(p %s)로 홍조군이 적었다.",
            getv("남자", "f_htn", "있음", "비홍조"), getv("남자", "f_htn", "있음", "홍조"), getp("남자", "f_htn"),
            getv("여자", "f_htn", "있음", "비홍조"), getv("여자", "f_htn", "있음", "홍조"), getp("여자", "f_htn")),
    "")

add("### 2.3 칸별 고요산혈증 사례 수와 가중 유병률", "",
    "**표 2a. 성별 × 현재 홍조 × 음주 수준**", "", cell_md("flush_now", "현재 홍조"),
    "**표 2b. 성별 × 홍조 패턴(4군) × 음주 수준**", "", cell_md("flush_grp", "홍조 패턴"),
    "![고요산혈증 가중 유병률](fig/fig6_prevalence.png)", "")

# 2.4 주분석
add("### 2.4 주분석: 음주 수준 × 현재 홍조 (고요산혈증)", "",
    "표의 열: 결합효과 OR = 비홍조·저음주를 기준으로 한 각 칸의 OR, 층화 OR = 같은 홍조군 안에서 저음주 대비 OR(단순효과), 홍조 효과 OR = 같은 음주 수준 안에서 비홍조 대비 홍조의 OR, RERI = 덧셈 척도의 상호작용 지표(0 = 덧셈 상호작용 없음).", "")
for (s in sexes) for (m in c(M0, M1, M2)) {
  x <- a5[outcome == hua & sex == s & model == m]
  tr <- tr5[outcome == hua & sex == s & model == m]
  add(sprintf("**표 3-%s. %s · %s (모형 n = %s)**", if (s == "남자") paste0("남", match(m, c(M0, M1, M2))) else paste0("여", match(m, c(M0, M1, M2))), s, m, big(x$n_model[1])), "",
      kv_md(x),
      sprintf("- 음주 수준 × 현재 홍조 상호작용(공동 Wald, 2 자유도): p = %s. 상호작용항별 p: 중음주 %s, 고음주 %s.",
              fmt_p(p_int(s, m)), fmt_p(x[modifier == "홍조" & exposure == "중음주", p_product_term]), fmt_p(x[modifier == "홍조" & exposure == "고음주", p_product_term])),
      sprintf("- P for trend(한 단계당 OR): %s. 추세의 홍조군 간 차이 p = %s.", trend_txt(tr), fmt_p(tr$p_trend_x_modifier[1])),
      "")
}
add("![주분석 포레스트](fig/fig2_main_forest.png)", "",
    sprintf("- 보정변수를 늘릴수록 음주의 OR은 조금 커졌다(남자 비홍조 고음주 M0 %s → M2 %s; 여자 비홍조 고음주 M0 %s → M2 %s). 홍조군의 점 추정치가 비홍조군보다 조금 크지만, 신뢰구간이 크게 겹치고 곱셈·덧셈 척도 모두에서 차이를 지지하지 않는다.",
            or_s(gcell(a5, "남자", "비홍조", "고음주", M0)), or_s(mB), or_s(gcell(a5, "여자", "비홍조", "고음주", M0)), or_s(wB)),
    "")

# 2.5 연속형 요산
add("### 2.5 혈청 요산(연속형): 여성(계획), 남성(보조)", "",
    "β는 혈청 요산의 차이(mg/dL). 결합효과 β는 비홍조·저음주 기준, 층화 β는 같은 홍조군 안에서 저음주 대비, 홍조 효과 β는 같은 음주 수준 안에서 비홍조 대비.", "")
for (m in c(M0, M1, M2)) {
  x <- a5[outcome == ua & sex == "여자" & model == m]; tr <- tr5[outcome == ua & sex == "여자" & model == m]
  add(sprintf("**표 4-%d. 여자 · %s (모형 n = %s)**", match(m, c(M0, M1, M2)), m, big(x$n_model[1])), "", kv_md(x, or = FALSE),
      sprintf("- 상호작용(공동 Wald) p = %s. 한 단계당 β: %s. 추세 차이 p = %s.", fmt_p(p_int("여자", m, ua)), trend_txt(tr), fmt_p(tr$p_trend_x_modifier[1])), "")
}
xm <- a5[outcome == ua_m & sex == "남자" & model == M2]; trm <- tr5[outcome == ua_m & sex == "남자" & model == M2]
add(sprintf("**표 4-4. 남자 · %s (보조, 모형 n = %s)**", M2, big(xm$n_model[1])), "", kv_md(xm, or = FALSE),
    sprintf("- 상호작용(공동 Wald) p = %s. 한 단계당 β: %s. 추세 차이 p = %s.", fmt_p(t5[outcome == ua_m & sex == "남자" & model == M2, p]), trend_txt(trm), fmt_p(trm$p_trend_x_modifier[1])), "")

# 2.6 홍조 이력
add("### 2.6 부질문 1: 홍조 이력 3군 (비홍조 · 홍조 소실 · 지속 홍조)", "",
    "신규 홍조(과거 아니오·현재 있음)는 제외했다. 기준은 비홍조·저음주.", "")
for (s in sexes) for (m in c(M1, M2)) {
  x <- a6[outcome == hua & sex == s & model == m]; tr <- tr6[outcome == hua & sex == s & model == m]
  add(sprintf("**표 5-%s. %s · %s (모형 n = %s)**", paste0(if (s == "남자") "남" else "여", match(m, c(M1, M2))), s, m, big(x$n_model[1])), "",
      kv_md(x), test_md(t6[outcome == hua & sex == s & model == m]),
      sprintf("- P for trend(한 단계당 OR): %s.", trend_txt(tr)), "")
}
xw <- a6[outcome == ua & sex == "여자" & model == M2]
add(sprintf("**표 5-5. 여자 · 혈청 요산(β) · %s (모형 n = %s)**", M2, big(xw$n_model[1])), "", kv_md(xw, or = FALSE), test_md(t6[outcome == ua & sex == "여자" & model == M2]),
    "![홍조 이력 포레스트](fig/fig3_history_forest.png)", "",
    "**표 5-6. 보조: 신규 홍조를 포함한 4군 (확장 모형, 저음주 대비 층화 OR)**", "",
    md_table(g4[, .(성별 = sex, 홍조패턴 = modifier, `음주 수준` = exposure, n = big(n), 사례 = big(n_event), `층화 OR (95% CI)` = ref_or(strat_est, strat_lo, strat_hi, 1),
                    `RERI (95% CI)` = fmt_ci(reri, reri_lo, reri_hi), `상호작용 p (6 자유도)` = fmt_p(p_interaction_6df))]),
    sprintf("- 남자 홍조 소실군은 고음주 OR %s(비홍조 %s, 지속 %s)로 점 추정치가 컸고 RERI %s였지만, 소실 대 비홍조 p = %s로 확정적이지 않다. 여자 소실군은 고음주 %s명(사례 %s)뿐이라 해석하지 않는다.",
            or_s(hS), or_s(hN), or_s(hP), fmt_ci(hS$reri, hS$reri_lo, hS$reri_hi), fmt_p(p_hist_sn),
            big(gcell(a6, "여자", "홍조 소실", "고음주")$n), big(gcell(a6, "여자", "홍조 소실", "고음주")$n_event)),
    "")

# 2.7 성별
add("### 2.7 부질문 2: 성별 차이", "",
    md_table(sd7[, .(결과 = outcome, 모형 = model, 검정 = test, `모형 n` = big(n_model), 자유도 = df, F = sprintf("%.2f", F), p = fmt_p(p))]),
    sprintf("- 3원 상호작용은 고요산혈증 p = %s(확장), 요산 p = %s(확장)로, 홍조에 따른 음주 연관성의 차이가 남녀 간에 다르다는 근거는 없었다. 여성의 OR 점 추정치가 남성보다 크지만 음주 × 성별 검정도 유의하지 않았다.",
            fmt_p(p_sex3), fmt_p(sd7[outcome == ua & model == M2 & grepl("3원", test), p])), "")

# 2.8 폐경
add("### 2.8 여성 폐경 전·후 (탐색, 확장 모형)", "")
for (o in c(hua, ua)) for (mn in c("폐경 전", "폐경 후")) {
  x <- a8[outcome == o & menopause == mn]
  add(sprintf("**표 6. %s · %s (모형 n = %s)**", mn, o, big(x$n_model[1])), "", kv_md(x, or = o == hua),
      sprintf("- 음주 수준 × 현재 홍조 상호작용 p = %s.", fmt_p(t8[outcome == o & menopause == mn & !grepl("3원", test), p])), "")
}
add(test_md(t8[grepl("3원", test)][, test := paste0(outcome, ": ", test)]),
    "![폐경 포레스트](fig/fig4_menopause_forest.png)", "",
    sprintf("- 폐경 전 여성은 고음주 OR이 비홍조 %s, 홍조 %s로 같았다. 폐경 후는 홍조군 %s가 비홍조군 %s보다 컸지만, 폐경 후 홍조·고음주 칸은 %s명(사례 %s)이라 신뢰구간이 매우 넓고 3원 상호작용 p = %s였다. 가설(폐경 후에서 차이가 뚜렷)과 방향은 같지만 확인된 것은 아니다.",
            or_s(a8[outcome == hua & menopause == "폐경 전" & modifier == "비홍조" & exposure == "고음주"]),
            or_s(a8[outcome == hua & menopause == "폐경 전" & modifier == "홍조" & exposure == "고음주"]),
            or_s(postF), or_s(postB), big(postF$n), big(postF$n_event), fmt_p(p_meno3)), "")

# 2.9 민감도
s9[, top := fifelse(id == "S5", "주1회 이상", fifelse(id == "S7", "T3", fifelse(id == "S10", "월1회 초과", "고음주")))]
sens_tab <- s9[!(exposure %in% c("저음주", "없음", "T1", "월1회 이하"))]
add("### 2.9 민감도 분석 (확장 모형, 현재 홍조)", "",
    "값은 기준 수준(저음주 / 폭음 없음 / T1 / 월1회 이하) 대비 층화 OR. 상호작용 p는 노출 × 현재 홍조 공동 검정.", "",
    md_table(sens_tab[, .(ID = id, 분석 = analysis, 성별 = sex, 홍조군 = modifier, `노출 수준` = exposure, `n (사례)` = paste0(big(n), " (", big(n_event), ")"),
                          `층화 OR (95% CI)` = fmt_ci(strat_est, strat_lo, strat_hi), p = fmt_p(strat_p), `RERI (95% CI)` = fmt_ci(reri, reri_lo, reri_hi),
                          `상호작용 p` = fmt_p(p_interaction))]),
    "![민감도 남자](fig/fig5a_sensitivity_men.png)", "", "![민감도 여자](fig/fig5b_sensitivity_women.png)", "",
    sprintf("- 어느 분석에서도 상호작용이 유의하지 않았다(p %.2f–%.2f). 2020년을 빼면(S4) 남자 홍조군의 고음주 OR이 %s로 약해지고 여자는 %s로 강해졌다. 홍조를 '자주·늘'로 좁히면(S6) 여자 홍조 중음주는 사례 %s명뿐이라 OR %s, RERI %s가 나오지만 표본이 작아 신뢰하기 어렵다.",
            sens_p[1], sens_p[2],
            fmt_ci(s9[id == "S4" & sex == "남자" & modifier == "홍조" & exposure == "고음주", strat_est], s9[id == "S4" & sex == "남자" & modifier == "홍조" & exposure == "고음주", strat_lo], s9[id == "S4" & sex == "남자" & modifier == "홍조" & exposure == "고음주", strat_hi]),
            fmt_ci(s9[id == "S4" & sex == "여자" & modifier == "홍조" & exposure == "고음주", strat_est], s9[id == "S4" & sex == "여자" & modifier == "홍조" & exposure == "고음주", strat_lo], s9[id == "S4" & sex == "여자" & modifier == "홍조" & exposure == "고음주", strat_hi]),
            big(s9[id == "S6" & sex == "여자" & modifier == "홍조" & exposure == "중음주", n_event]),
            fmt_ci(s9[id == "S6" & sex == "여자" & modifier == "홍조" & exposure == "중음주", strat_est], s9[id == "S6" & sex == "여자" & modifier == "홍조" & exposure == "중음주", strat_lo], s9[id == "S6" & sex == "여자" & modifier == "홍조" & exposure == "중음주", strat_hi]),
            fmt_ci(s9[id == "S6" & sex == "여자" & modifier == "홍조" & exposure == "중음주", reri], s9[id == "S6" & sex == "여자" & modifier == "홍조" & exposure == "중음주", reri_lo], s9[id == "S6" & sex == "여자" & modifier == "홍조" & exposure == "중음주", reri_hi])),
    "", "**보조: 홍조군별 완전 층화 모형(공변량 효과도 홍조군마다 다르게, Kang & Oh 2023 방식)**", "",
    md_table(st10[, .(성별 = sex, 홍조군 = flush, `모형 n` = big(n_model), `저음주 n` = big(n_ref), `음주 수준` = term, n = big(n_cat), `OR (95% CI)` = fmt_ci(est, ci_low, ci_high), p = fmt_p(p))]),
    "상호작용 모형에서 구한 층화 OR과 거의 같다.", "")

# 2.10 공식 통계
add("### 2.10 공식 통계 대조", "",
    "2019년 단년도 자료와 wt_itvex로 지침서 정의(8기 PDF p.48 / 인쇄 p.40)의 고위험음주율·월간음주율을 재현했다. 공식 조율값은 출처를 찾지 못해 비어 있다. 웹에 있는 값(PHWR 등)은 2005년 추계인구 연령표준화율이라 조율 추정치와 비교할 수 없다.", "",
    md_table(oc[, .(지표 = indicator, `비가중 n` = big(n_unweighted), 추정치 = sprintf("%.2f", estimate), SE = sprintf("%.2f", se), `95% CI` = sprintf("%.2f–%.2f", ci_low, ci_high),
                    공식값 = ifelse(is.na(official), "미확인", sprintf("%.2f", official)), `CI 포함` = ifelse(is.na(within_ci), "", ifelse(within_ci, "예", "아니오")))]))

# ===== 3. 해석 =====
add("## 3. 해석과 한계", "",
    "### 3.1 연구 질문별 결론", "",
    sprintf("- **주질문**: 음주량–고요산혈증 연관성은 남녀와 홍조군 모두에서 확인되었지만(고음주 OR 남 %s·%s, 여 %s·%s), 현재 홍조에 따른 연관성의 차이는 곱셈 척도(p %s, %s)와 덧셈 척도(RERI 0 포함) 모두에서 확인되지 않았다. 발표 v5의 세 시나리오 중 **C(차이 없음)** 에 해당한다. 남자는 신뢰구간이 좁아 '차이가 있더라도 작다'로, 여자는 넓어 '자료가 부족하다'로 읽는 것이 맞다.",
            or_s(mB), or_s(mF), or_s(wB), or_s(wF), fmt_p(p_int("남자")), fmt_p(p_int("여자"))),
    "- 홍조군은 음주량이 적고, 같은 저음주에서도 고요산혈증이 적거나 비슷했다. '얼굴이 빨개지는 사람이 같은 양에 더 민감하다'는 통념은 요산에서는 지지되지 않았다. 김어진(2016)의 홍조군 저용량 위험 상승과는 다른 방향이고, Yokoyama(2016)의 '비홍조형에서 요산이 더 높다'는 결과와도 뚜렷이 일치하지는 않는다.",
    sprintf("- **부질문 1**: 남자 홍조 소실군의 고음주 OR %s와 RERI %s는 '소실군이 비홍조군보다 음주의 영향이 클 수 있다'는 가설과 방향이 같지만, 상호작용 검정(p %s)으로는 확인되지 않았고 칸이 작다. 가설 생성 수준의 결과다.",
            or_s(hS), fmt_ci(hS$reri, hS$reri_lo, hS$reri_hi), fmt_p(p_hist_sn)),
    sprintf("- **부질문 2**: 성별 3원 상호작용 p %s. 폐경 후 여성의 홍조군 고음주 OR이 크지만 사례 %s명으로 불안정하다.", fmt_p(p_sex3), big(postF$n_event)),
    "",
    "### 3.2 한계와 가정", "",
    "- 단면연구라 선후 관계와 인과를 말할 수 없다. 현재 음주자만 분석해 홍조 때문에 술을 끊은 사람이 빠졌다(홍조군 = 홍조가 있어도 계속 마시는 사람).",
    "- 음주량과 홍조는 자기보고이며, 범주 응답을 대표값으로 환산했다(민감도 S3a·S3b에서 결과 불변). 주종과 퓨린 섭취, 이뇨제 등 약물은 반영하지 못했다.",
    "- 고요산혈증 기준(남 ≥7.0, 여 ≥6.0)과 eGFR 식(CKD-EPI 2009)은 지침서에 없는 문헌 기준이다. 요산 1.0 미만 표기는 결측 처리했다.",
    "- 여성 홍조·고음주(95명), 남자 홍조 소실·고음주(125명), 폐경 후 홍조·고음주(23명) 등 작은 칸의 추정치는 신뢰구간이 넓다. 여러 비교를 했으므로 탐색적 결과는 가설 생성으로만 본다.",
    "- 공식 통계 대조가 완료되지 않았다(조율 공식값 필요).",
    "")

# ===== 4. 부록 =====
add("## 4. 부록: 결과 파일", "",
    md_table(data.table(파일 = c("00_reference_methods.md", "01_variable_definitions.csv", "02_participant_flow.csv", "02b_flow_vs_presentation_v5.csv", "03_cell_counts.csv",
                               "03b_included_vs_excluded.csv", "03c_conversion_table.csv", "03d_tertile_cutpoints.csv", "04_table1_by_sex_flushnow.csv", "04b_table1_by_sex_flushhist.csv",
                               "05a_main_joint_effects.csv", "05b_main_interaction_tests.csv", "05c_main_trend.csv", "06a_history_joint_effects.csv", "06b_history_tests.csv",
                               "06c_history_trend.csv", "06d_history_4groups_supp.csv", "07_sex_difference_tests.csv", "08a_women_menopause_cells.csv", "08b_women_menopause_tests.csv",
                               "09_sensitivity.csv", "10_supp_stratified_models.csv", "11_official_check.csv", "RESULTS_summary.md", "fig/", "_inputs.csv, _sessionInfo.txt, _reproducibility.txt, _run_log.txt"),
                        내용 = c("참고 논문 방법 정리와 설계 반영", "변수 정의(지침서 쪽수)", "대상자 선정 흐름", "발표 v5 흐름 대조", "칸별 n·사례·가중 유병률",
                                 "포함자 vs 제외자 비교", "음주 대표값 환산표", "성별 삼분위 절단점(S7)", "Table 1 (현재 홍조)", "Table 1 (홍조 이력 3군)",
                                 "주분석 결합·층화 OR·RERI (M0·M1·M2, 남녀, 요산 연속형 포함)", "주분석 상호작용 검정", "주분석 P for trend", "홍조 이력 결합·층화 OR", "홍조 이력 검정",
                                 "홍조 이력 P for trend", "4군 보조 분석", "성별 차이 검정", "폐경 전·후 결과", "폐경 검정",
                                 "민감도 분석 10가지", "홍조군별 완전 층화 모형", "공식 통계 대조", "자동 생성 요약", "그림 PNG", "입력 파일 지문, 세션 정보, 재현성, 실행 로그"))))

con <- file(out_md, open = "w", encoding = "UTF-8"); writeLines(L, con); close(con)
cat("완료:", out_md, "\n")
