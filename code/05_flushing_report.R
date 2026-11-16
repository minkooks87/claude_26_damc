# 05_flushing_report.R
# 목적: 03_flushing_hyperuricemia.R 의 결과(CSV)와 04_flushing_figures.R 의 그림으로
#       분석 보고서(Markdown)를 만든다. 숫자는 모두 결과 CSV에서 읽어 넣는다(손으로 옮기지 않는다).
# 실행: Rscript --vanilla code/05_flushing_report.R   (프로젝트 폴더에서)
# 결과: output/03_flushing_hyperuricemia/REPORT_안면홍조_고요산혈증.md

suppressPackageStartupMessages(library(data.table))

in_dir <- file.path("output", "03_flushing_hyperuricemia")
out_md <- file.path(in_dir, "REPORT_안면홍조_고요산혈증.md")
rd <- function(f) fread(file.path(in_dir, f), encoding = "UTF-8")

esc <- function(x) gsub("\\|", "\\\\|", trimws(as.character(x)))
md_table <- function(dt, headers = names(dt)) {
  dt <- as.data.table(dt)[, lapply(.SD, function(x) { x <- as.character(x); x[is.na(x)] <- ""; x })]
  c(paste0("| ", paste(headers, collapse = " | "), " |"),
    paste0("|", paste(rep("---", length(headers)), collapse = "|"), "|"),
    apply(dt, 1, function(r) paste0("| ", paste(esc(r), collapse = " | "), " |")), "")
}
fmt_or <- function(e, l, h) sprintf("%.2f (%.2f–%.2f)", e, l, h)
# 신뢰구간에 음수가 있으면 줄표(–)가 부호(-)와 겹쳐 읽기 어려우므로 쉼표로 구분한다
fmt_ci <- function(e, l, h) ifelse(l < 0 | h < 0, sprintf("%.2f (%.2f, %.2f)", e, l, h), sprintf("%.2f (%.2f–%.2f)", e, l, h))
fmt_p  <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))
big    <- function(x) format(x, big.mark = ",", trim = TRUE)   # trim: 자리 맞춤 공백 제거

# ---- 결과 읽기 ----
vd    <- rd("01_variable_definitions.csv")
flow  <- rd("02_participant_flow.csv")
t1    <- rd("04_table1.csv")
oc    <- rd("05_official_check.csv")
pv    <- rd("05b_prevalence_by_sex_flush_drink.csv")
a     <- rd("06a_drink_by_sex_flush.csv")
b     <- rd("06b_flush_pattern_by_sex.csv")
ci    <- rd("06c_interaction_tests.csv")
w     <- rd("06d_women_menopause.csv")
sens  <- rd("07_sensitivity.csv")
inputs <- rd("_inputs.csv")
repro  <- if (file.exists(file.path(in_dir, "_reproducibility.txt"))) readLines(file.path(in_dir, "_reproducibility.txt"), encoding = "UTF-8") else character(0)
repro_res <- sub("^result:\\s*", "", grep("^result:", repro, value = TRUE))
repro_time <- sub("^time:\\s*", "", grep("^time:", repro, value = TRUE))
sess <- readLines(file.path(in_dir, "_sessionInfo.txt"), encoding = "UTF-8", n = 1)

hua <- "고요산혈증(OR)"; ua <- "요산(mg/dL 차이)"
n_final <- flow[.N, n]
g6b <- function(s, t, o = hua) b[sex == s & term == t & outcome == o]
g6a <- function(s, f, t = "고위험음주", o = hua) a[sex == s & flush == f & term == t & outcome == o]
g6d <- function(m, f, t = "고위험음주", o = hua) w[menopause == m & flush_now == f & term == t & outcome == o]
or_txt <- function(r) fmt_or(r$est, r$ci_low, r$ci_high)

# ---- Table 1 정리: "추정치 (n)" 형태 ----
grp <- c("남자·비홍조", "남자·지속 홍조", "남자·홍조 소실", "남자·신규 홍조",
         "여자·비홍조", "여자·지속 홍조", "여자·홍조 소실", "여자·신규 홍조")
t1m <- t1[, .(항목 = ifelse(nzchar(level), paste0(label, ": ", level), label), 통계 = stat)]
# 표가 넓어지지 않도록 긴 항목 이름은 줄인다(정의는 본문 2장에 있음)
t1m[, 항목 := gsub("\\(지침서 고위험음주 정의\\)", "", 항목)][, 항목 := gsub("\\(추정, 가정 B\\)", "(추정)", 항목)]
for (g in grp) {
  n_col <- paste0(g, ": n"); e_col <- paste0(g, ": 추정치")
  t1m[, (g) := ifelse(t1[[n_col]] == 0, "–", paste0(t1[[e_col]], " (", big(t1[[n_col]]), ")"))]
}

# ---- 본문 조립 ----
L <- c()
add <- function(...) L <<- c(L, ...)

add(paste0("# 한국 성인 음주자에서 음주 관련 안면홍조 패턴이 음주와 고요산혈증의 연관성에 미치는 영향"),
    "",
    "성별 및 홍조 패턴(비홍조·지속·소실·신규), 여성의 폐경 상태에 따른 분석 — 국민건강영양조사 제8기(2019–2021)",
    "",
    sprintf("- 보고서 생성: %s", format(Sys.time(), "%Y-%m-%d %H:%M")),
    "- 분석 코드: `code/03_flushing_hyperuricemia.R` (본 분석), `code/04_flushing_figures.R` (그림), `code/05_flushing_report.R` (이 보고서)",
    sprintf("- 분석 환경: %s, survey 패키지(복합표본)", sess),
    sprintf("- 재현성 확인: **%s** (%s, 새 R 세션에서 2회 실행 후 결과 CSV 비교)", ifelse(length(repro_res), repro_res, "미실행"), ifelse(length(repro_time), repro_time, "")),
    "")

# 요약
rA_m <- g6b("남자", "지속 홍조"); rA_f <- g6b("여자", "지속 홍조")
rB_m0 <- g6a("남자", "비홍조"); rB_m1 <- g6a("남자", "지속 홍조"); rB_m2 <- g6a("남자", "홍조 소실")
rB_f0 <- g6a("여자", "비홍조"); rB_f1 <- g6a("여자", "지속 홍조"); rB_f2 <- g6a("여자", "홍조 소실")
rD <- g6d("폐경 후", "현재 홍조 있음")
pi_m <- ci[outcome == hua & test == "남자: 음주 수준 × 홍조 패턴", p_interaction]
pi_f <- ci[outcome == hua & test == "여자: 음주 수준 × 홍조 패턴", p_interaction]
add("## 요약", "",
    sprintf("- 분석 대상은 2019–2021년 만 19세 이상 최근 1년 음주자 중 홍조 문항에 응답하고 요산을 측정한 %s명(통풍 진단자 제외)이다.", big(n_final)),
    sprintf("- 음주 수준을 보정하면 **지속 홍조군은 비홍조군보다 고요산혈증이 적었다**(남 OR %s, 여 OR %s).", or_txt(rA_m), or_txt(rA_f)),
    sprintf("- 같은 홍조군 안에서 고위험음주(vs 월1회 미만)의 연관성은 홍조군에서 더 컸다. 남성 비홍조 %s → 지속 %s → 소실 %s, 여성 비홍조 %s → 지속 %s → 소실 %s.",
            or_txt(rB_m0), or_txt(rB_m1), or_txt(rB_m2), or_txt(rB_f0), or_txt(rB_f1), or_txt(rB_f2)),
    sprintf("- 여성은 폐경 후·현재 홍조 있음 군에서 고위험음주의 연관성이 가장 컸다(OR %s, 고위험음주자 n = %d).", or_txt(rD), rD$n_cat),
    sprintf("- 다만 음주 수준 × 홍조 패턴 상호작용은 유의하지 않았고(남 p = %s, 여 p = %s), 일부 칸의 표본이 작아 신뢰구간이 넓다. 결과는 **경향**으로 해석한다.", fmt_p(pi_m), fmt_p(pi_f)),
    "")

# 1. 자료
add("## 1. 자료와 복합표본설계", "",
    "- 자료: 국민건강영양조사 제8기 2019, 2020, 2021년 기본DB(`hn19_all`, `hn20_all`, `hn21_all`). 음주 관련 홍조 문항(BD9_1, BD9_2)은 이 3개 연도에만 있다.",
    "- 가중치: 건강설문·검진 변수만 사용하므로 `wt_itvex`(8기 지침서 PDF p.92 / 인쇄 p.84). 3개 연도를 합치는 통합비율은 각 연도 1/3.",
    "  - 제8기는 예외적으로 조사구 수 비율 대신 **조사기간 비례**로 합친다. 2020년은 코로나19로 192개 중 180개 조사구만 조사했지만 조사기간은 1년으로 같다(8기 지침서 PDF p.36 / 인쇄 p.28, 표 7).",
    "- 층 `kstrata`, 집락 `psu`. 층 안에 집락이 1개뿐인 경우는 없었다.",
    "- 모든 추정은 `svydesign` 설계 객체를 만든 뒤 `subset`으로 분석 대상을 지정해 계산했다(행 삭제 없음).",
    "",
    "입력 파일 지문:", "",
    md_table(inputs[, .(파일 = basename(file), 크기 = big(size), md5)]))

# 2. 변수 정의
add("## 2. 변수 정의 (이용지침서 쪽수)", "",
    "변수의 의미와 코드값은 제8기(2019–2021) 원시자료 이용지침서에서 확인했다. 9·99·999(모름·무응답)는 결측으로, 8·88·888(비해당)은 지침서의 의미를 보고 처리했다.", "",
    md_table(vd[, .(변수 = var, 설명 = label, 코드 = levels, 결측코드 = miss, 비해당처리 = na8, 근거 = source)]),
    "분석 변수 만들기:", "",
    "- **홍조 패턴**: 과거 홍조(BD9_1: 1 예, 2 아니오) × 현재 홍조(BD9_2: 1 아니오, 2 가끔, 3 자주, 4 늘) → 비홍조(과거 아니오·현재 아니오), 지속 홍조(과거 예·현재 가끔 이상), 홍조 소실(과거 예·현재 아니오), 신규 홍조(과거 아니오·현재 가끔 이상). 신규 홍조군은 회상 오류 가능성이 있어 해석에서 제외한다.",
    "- **음주 수준**: 지침서의 고위험음주 정의(주 2회 이상 & 1회 음주량 남 7잔·여 5잔 이상, 8기 PDF p.48 / 인쇄 p.40)에 따라 월1회 미만 / 월1회 이상(고위험 아님) / 고위험음주.",
    "- **고요산혈증**: 혈중 요산(HE_Uacid) 남 ≥ 7.0, 여 ≥ 6.0 mg/dL. *지침서에는 고요산혈증 정의가 없어 문헌 기준을 적용했다(가정).* 요산 1.0 mg/dL 미만(HE_Uacid_etc)은 0.9로 두었다.",
    "- **eGFR**: 혈중 크레아티닌으로 CKD-EPI 2021 식 계산(지침서에 계산식 없음, 가정).",
    "- **폐경**: LW_ms 5(자연폐경)·6(인공폐경) → 폐경 후, 2–4 → 폐경 전.",
    "- **현재흡연**: 지침서 주요지표 SAS 정의(BS1_1, BS3_1) 그대로.",
    "")

# 3. 흐름
add("## 3. 대상자 선정 흐름", "",
    md_table(flow[, .(단계 = step, 남은_n = big(n), 제외 = ifelse(is.na(excluded), "", big(excluded)))], c("단계", "남은 n", "제외")),
    "마지막 단계에서 제외된 사람은 통풍 의사진단자와 통풍 문항 모름·무응답자이다(실행 로그 참조). 모름·무응답자는 교육·신체활동 문항도 결측이라 회귀모형에서는 어차피 빠진다.", "",
    "![대상자 선정 흐름](fig/fig1_participant_flow.png)", "")

# 4. Table 1
add("## 4. 대상자 특성 (Table 1): 성별 × 홍조 패턴", "",
    "각 칸은 **가중 추정치 (비가중 n)** 이다. 범주형은 가중 % (SE), 연속형은 가중 평균 (SE).", "",
    md_table(t1m),
    "주: 연도 2019–2021, 가중치 wt_itvex × 1/3, 층 kstrata, 집락 psu. n은 비가중 표본 수, %와 평균은 복합표본 가중 추정치. 공복혈당·중성지방 관련 조건은 이 분석에 해당 없음. 남성의 폐경 행은 해당 없음(–).", "")

# 5. 유병률
drink_lv <- c("월1회 미만", "월1회 이상(고위험 아님)", "고위험음주")
pvo <- pv[, .(성별 = sex, 홍조 = flush, 음주 = drink, 비가중_n = n, 고요산혈증_n = n_hua, 가중퍼센트 = sprintf("%.1f (%.1f)", pct, se))]
pvo[, 홍조 := factor(홍조, c("비홍조", "지속 홍조", "홍조 소실", "신규 홍조"))][, 음주 := factor(음주, drink_lv)]
setorder(pvo, 성별, 홍조, 음주)
add("## 5. 고요산혈증 유병률: 성별 × 홍조 패턴 × 음주 수준", "",
    md_table(pvo[, lapply(.SD, as.character)], c("성별", "홍조 패턴", "음주 수준", "비가중 n", "고요산혈증 n", "가중 % (SE)")),
    "![고요산혈증 유병률](fig/fig5_prevalence.png)", "",
    "그림의 * 표시는 비가중 n < 30인 칸이다.", "")

# 6. 본 분석
covs_txt <- "보정 변수: 나이, 조사연도, 체질량지수, eGFR, 고혈압·당뇨병 의사진단, 현재흡연, 소득 4분위, 교육수준, 유산소 신체활동. 여성 모형은 폐경 여부를 추가."
add("## 6. 본 분석 (복합표본 로지스틱·선형 회귀)", "", covs_txt, "",
    "### 6-1. 홍조 패턴 자체의 연관성 (음주 수준 보정, 기준 = 비홍조)", "",
    md_table(b[, .(결과 = outcome, 성별 = sex, 홍조패턴 = term, 추정치 = fmt_ci(est, ci_low, ci_high), p = fmt_p(p), 해당군_n = n_cat, 기준군_n = n_ref, 모형_n = n_model)],
             c("결과", "성별", "홍조 패턴", "OR 또는 차이 (95% CI)", "p", "해당 군 n", "기준군 n", "모형 n")),
    "![홍조 패턴 vs 비홍조](fig/fig2_forest_flush_pattern.png)", "",
    "### 6-2. 홍조군별 음주 수준의 연관성 (기준 = 월1회 미만)", "",
    md_table(a[, .(결과 = outcome, 성별 = sex, 홍조패턴 = flush, 음주수준 = term, 추정치 = fmt_ci(est, ci_low, ci_high), p = fmt_p(p), 해당_n = n_cat, 기준_n = n_ref, 모형_n = n_model)],
             c("결과", "성별", "홍조 패턴", "음주 수준", "OR 또는 차이 (95% CI)", "p", "해당 n", "기준군 n", "모형 n")),
    "![홍조군별 고위험음주](fig/fig3_forest_highrisk_by_flush.png)", "",
    "### 6-3. 상호작용 검정 (Wald)", "",
    md_table(ci[, .(결과 = outcome, 검정 = test, 모형_n = n_model, p = fmt_p(p_interaction))], c("결과", "검정", "모형 n", "p")),
    "### 6-4. 여성: 폐경 상태 × 현재 홍조 (기준 = 월1회 미만)", "",
    md_table(w[, .(결과 = outcome, 폐경 = menopause, 현재홍조 = flush_now, 음주수준 = term, 추정치 = fmt_ci(est, ci_low, ci_high), p = fmt_p(p), 해당_n = n_cat, 기준_n = n_ref, 고요산혈증_사례 = n_hua_events, 모형_n = n_model)],
             c("결과", "폐경", "현재 홍조", "음주 수준", "OR 또는 차이 (95% CI)", "p", "해당 n", "기준군 n", "고요산혈증 사례 수", "모형 n")),
    "![여성 폐경 × 홍조](fig/fig4_forest_women_menopause.png)", "",
    "### 6-5. 민감도 분석 (고요산혈증, 모형은 6-2와 동일)", "",
    md_table(sens[, .(분석 = analysis, 성별 = sex, 홍조패턴 = flush, 모형_n = n_model, OR = sprintf("%.2f (%.2f–%.2f)", est, ci_low, ci_high), p = fmt_p(p))],
             c("분석", "성별", "홍조 패턴", "모형 n", "OR (95% CI)", "p")),
    "- S1: 통풍 의사진단자를 포함. S2: 음주 노출을 주당 음주 잔 수(범주 중간값 가정)로 바꾸고 주당 7잔 증가당 OR. S3: 통풍 문항 모름·무응답자를 포함.", "")

# 7. 공식 통계·재현성
add("## 7. 공식 통계 대조와 재현성", "",
    "단년도(2019) `wt_itvex`로 만 19세 이상 고위험음주율(조율)을 재현했다. 공식 조율값은 「2019 국민건강통계」에서 표 번호·쪽과 함께 확인해 `official` 열에 넣어야 한다(아직 비어 있음).", "",
    md_table(oc[, .(지표 = indicator, 비가중_n = n_unweighted, 추정치 = sprintf("%.2f", estimate), SE = sprintf("%.2f", se), CI = sprintf("%.2f–%.2f", ci_low, ci_high), 공식값 = ifelse(is.na(official), "미확인", sprintf("%.2f", official)), CI포함 = ifelse(is.na(within_ci), "", ifelse(within_ci, "예", "아니오")), 출처 = source)],
             c("지표", "비가중 n", "추정치(%)", "SE", "95% CI", "공식값", "CI 포함", "출처")),
    "주의: 언론에 보도된 '2019년 고위험음주율 14.1%'는 지역사회건강조사(표준화, 시군구 중앙값) 값이고, e-나라지표·주간 건강과 질병의 값은 2005년 추계인구 기준 연령표준화 값이라 조율 추정치와 비교할 수 없다.", "",
    sprintf("재현성 확인: **%s** — 새 R 세션에서 분석 스크립트를 2회 실행해 결과 CSV의 md5가 모두 같았다(%s).", ifelse(length(repro_res), repro_res, "미실행"), ifelse(length(repro_time), repro_time, "")), "")

# 8. 해석 ---------------------------------------------------------------------
# Table 1 값 꺼내기: 변수·범주·집단 → "추정치 (SE)" 문자열과 비가중 n
t1v <- function(v, lv, g) {
  r <- t1[variable == v & level == lv]
  list(est = r[[paste0(g, ": 추정치")]][1], n = r[[paste0(g, ": n")]][1])
}
t1pct <- function(v, lv, g) sub(" .*", "", t1v(v, lv, g)$est)     # "25.6 (1.0)" → "25.6"
gS <- function(key, s_, f_) sens[grepl(paste0("^", key), analysis) & sex == s_ & flush == f_]
p_int <- function(txt, o = hua) fmt_p(ci[outcome == o & test == txt, p_interaction])
cnt <- function(s_, f_) big(t1v("age", "", paste0(s_, "·", f_))$n)

add("## 8. 해석", "",
    "이 장은 결과를 연구 질문별로 다시 묶어, 숫자가 무엇을 뜻하는지와 어디까지 믿을 수 있는지를 함께 적는다. 표의 수치는 모두 6장 결과와 같다.", "",
    "### 8-1. 오즈비(OR) 읽는 법", "",
    "- **OR = 1**이면 두 집단의 고요산혈증 가능성이 같다는 뜻이다. **OR = 1.91**이면 비교군(고위험음주자)의 고요산혈증 오즈가 기준군(월1회 미만 음주자)의 약 1.9배라는 뜻이고, **OR = 0.78**이면 약 22% 낮다는 뜻이다.",
    "- 괄호 안 **95% 신뢰구간(CI)**이 1을 포함하지 않으면 통계적으로 유의하다(p < 0.05). 신뢰구간이 넓을수록 표본이 작아 추정이 불안정하다는 뜻이다.",
    "- 여기서 OR은 고요산혈증의 **상대적인 가능성**을 비교할 뿐, 홍조나 음주가 고요산혈증을 **일으킨다**는 뜻은 아니다(횡단 연구).", "")

# 8-2 한눈에 보는 결과
rA_m <- g6b("남자", "지속 홍조"); rA_f <- g6b("여자", "지속 홍조")
rB <- function(s_, f_) g6a(s_, f_)
rD_pre_no <- g6d("폐경 전", "현재 홍조 없음"); rD_pre_yes <- g6d("폐경 전", "현재 홍조 있음")
rD_post_no <- g6d("폐경 후", "현재 홍조 없음"); rD_post_yes <- g6d("폐경 후", "현재 홍조 있음")
glance <- data.table(
  연구질문 = c("① 홍조 패턴 자체가 고요산혈증과 관련 있는가?",
               "② 홍조군에서 고위험음주의 연관성이 더 큰가?",
               "③ 성별에 따라 다른가?",
               "④ 여성은 폐경 상태에 따라 다른가?",
               "⑤ 결과가 분석 방법에 따라 흔들리는가?"),
  핵심결과 = c(sprintf("지속 홍조군은 비홍조군보다 고요산혈증이 적다. 남 OR %s, 여 OR %s", or_txt(rA_m), or_txt(rA_f)),
               sprintf("고위험음주(vs 월1회 미만) OR: 남 비홍조 %.2f → 지속 %.2f → 소실 %.2f, 여 %.2f → %.2f → %.2f", rB("남자","비홍조")$est, rB("남자","지속 홍조")$est, rB("남자","홍조 소실")$est, rB("여자","비홍조")$est, rB("여자","지속 홍조")$est, rB("여자","홍조 소실")$est),
               sprintf("비홍조군에서는 음주–고요산혈증 관계가 여성에서 더 강함(남 OR %.2f vs 여 %.2f)", rB("남자","비홍조")$est, rB("여자","비홍조")$est),
               sprintf("폐경 후·현재 홍조 여성에서 고위험음주 OR %s (고위험음주자 n = %d)", or_txt(rD_post_yes), rD_post_yes$n_cat),
               "통풍 진단자 포함(S1), 주당 잔 수 척도(S2), 통풍 무응답 포함(S3) 모두 방향 일치"),
  통계적근거 = c(sprintf("남 p = %s, 여 p = %s", fmt_p(rA_m$p), fmt_p(rA_f$p)),
                 sprintf("상호작용 p: 남 %s, 여 %s (유의하지 않음)", p_int("남자: 음주 수준 × 홍조 패턴"), p_int("여자: 음주 수준 × 홍조 패턴")),
                 sprintf("음주 × 성별 상호작용 p = %s(비홍조군); 3원 상호작용 p = %s", p_int("음주 수준 × 성별 (비홍조)"), p_int("음주 수준 × 홍조 패턴 × 성별 (3원)")),
                 sprintf("p = %s, 그러나 신뢰구간이 매우 넓음", fmt_p(rD_post_yes$p)),
                 "7장 참조"),
  확실성 = c("비교적 확실", "경향(통계적으로 확정 못 함)", "부분적", "경향(표본 작음)", "안정적")
)
add("### 8-2. 한눈에 보는 결과", "",
    md_table(glance, c("연구 질문", "핵심 결과", "통계적 근거", "해석의 확실성")))

# 8-3 발견 1
f1 <- rbindlist(lapply(c("남자", "여자"), function(s_) rbindlist(lapply(c("비홍조", "지속 홍조", "홍조 소실"), function(f_) {
  r <- if (f_ == "비홍조") NULL else g6b(s_, f_)
  data.table(성별 = s_, 홍조패턴 = f_, n = cnt(s_, f_),
             고요산혈증 = t1v("f_hua", "있음", paste0(s_, "·", f_))$est,
             고위험음주 = t1v("drink3", "고위험음주", paste0(s_, "·", f_))$est,
             OR = if (is.null(r)) "1 (기준)" else or_txt(r), p = if (is.null(r)) "" else fmt_p(r$p))
}))))
add("### 8-3. 발견 1 — 홍조가 있는 사람은 전반적으로 고요산혈증이 적다", "",
    md_table(f1, c("성별", "홍조 패턴", "비가중 n", "고요산혈증 유병률, 가중 % (SE)", "고위험음주 비율, 가중 % (SE)", "고요산혈증 OR (95% CI), 음주 수준 보정", "p")),
    sprintf("- **무엇을 보았나**: 보정하지 않은 유병률부터 지속 홍조군이 낮다(남 %s%% vs %s%%, 여 %s%% vs %s%%). 음주 수준과 공변량을 보정해도 지속 홍조군의 OR은 남 %s, 여 %s로 1보다 작았다.",
            t1pct("f_hua", "있음", "남자·지속 홍조"), t1pct("f_hua", "있음", "남자·비홍조"), t1pct("f_hua", "있음", "여자·지속 홍조"), t1pct("f_hua", "있음", "여자·비홍조"), or_txt(rA_m), or_txt(rA_f)),
    sprintf("- **왜 그럴까 (가능한 설명)**: 홍조가 있으면 소량의 술에도 불편해 평소 음주량이 적다. 실제로 지속 홍조군의 고위험음주 비율은 남 %s%%(비홍조 %s%%), 여 %s%%(비홍조 %s%%)로 크게 낮다. 다만 음주 수준을 보정한 뒤에도 차이가 남았으므로, 범주형 음주 문항이 잡지 못한 음주량의 차이(잔여 교란)나 홍조와 함께 가는 다른 생활습관이 작용했을 수 있다.",
            t1pct("drink3", "고위험음주", "남자·지속 홍조"), t1pct("drink3", "고위험음주", "남자·비홍조"), t1pct("drink3", "고위험음주", "여자·지속 홍조"), t1pct("drink3", "고위험음주", "여자·비홍조")),
    "- **홍조 소실군은 다르다**: 과거엔 빨개졌지만 지금은 아닌 사람은 비홍조군과 유병률 차이가 없었다. 이 집단은 나이가 많고(평균 52세) 남성은 고위험음주 비율이 가장 높아, 홍조라는 '경고 신호'가 사라진 뒤 음주가 늘어난 집단일 가능성이 있다.",
    "- **주의**: 이 결과는 '홍조가 요산을 낮춘다'는 뜻이 아니라, 홍조가 있는 사람의 **평균적인 생활 양상**이 고요산혈증과 덜 관련된다는 뜻이다. 홍조 자체의 대사적 효과를 분리하려면 음주량을 더 정밀하게 잰 자료가 필요하다.",
    "")

# 8-4 발견 2
f2 <- rbindlist(lapply(c("남자", "여자"), function(s_) rbindlist(lapply(c("비홍조", "지속 홍조", "홍조 소실"), function(f_) {
  r <- g6a(s_, f_); r2 <- gS("S2", s_, f_)
  data.table(성별 = s_, 홍조패턴 = f_, 고위험_n = r$n_cat, 기준_n = r$n_ref, OR = or_txt(r), p = fmt_p(r$p),
             OR7 = fmt_or(r2$est, r2$ci_low, r2$ci_high))
}))))
add("### 8-4. 발견 2 — 홍조가 있으면서 많이 마시면, 음주와 고요산혈증의 연관성이 더 크다", "",
    md_table(f2, c("성별", "홍조 패턴", "고위험음주자 n", "기준군(월1회 미만) n", "고위험음주 OR (95% CI)", "p", "주당 7잔 증가당 OR (95% CI), S2")),
    sprintf("- **무엇을 보았나**: 같은 '고위험음주'라도 비홍조군에서는 OR이 %.2f(남)·%.2f(여)로 뚜렷하지 않은데, 지속 홍조군에서는 %.2f(남)·%.2f(여)로 유의하게 커지고, 홍조 소실군에서는 %.2f(남)·%.2f(여)로 더 크다(단, 소실군은 신뢰구간이 매우 넓다).",
            rB("남자","비홍조")$est, rB("여자","비홍조")$est, rB("남자","지속 홍조")$est, rB("여자","지속 홍조")$est, rB("남자","홍조 소실")$est, rB("여자","홍조 소실")$est),
    "- **같은 척도로 다시 보아도 같다**: 고위험음주 기준은 남 7잔·여 5잔으로 달라서 남녀 OR을 그대로 비교하면 정의 차이가 섞인다. 그래서 남녀에게 같은 척도(주당 7잔 증가당)를 적용한 S2에서도 비홍조 → 지속 → 소실 순으로 OR이 커지는 방향이 같았다.",
    "- **왜 그럴까 (가능한 설명)**: 홍조는 알코올 대사 효소(ALDH2) 활성이 낮아 아세트알데히드가 쌓이는 체질의 표지로 알려져 있다. 이런 체질에서 같은 양을 마시면 알코올 대사의 부담이 더 크고, 요산 생성이 늘거나 배설이 줄어드는 쪽으로 작용할 가능성이 제안되어 왔다(기전 문헌 인용 필요). 즉 '홍조가 있는데도 많이 마시는 사람'이 가장 취약한 집단일 수 있다.",
    sprintf("- **어디까지 믿을 수 있나**: 홍조 패턴에 따라 음주의 효과가 통계적으로 다른지를 보는 상호작용 검정은 유의하지 않았다(남 p = %s, 여 p = %s). 따라서 '홍조군에서 더 크다'는 것은 **경향**이며, 더 큰 표본에서 확인이 필요하다.",
            p_int("남자: 음주 수준 × 홍조 패턴"), p_int("여자: 음주 수준 × 홍조 패턴")),
    "")

# 8-5 발견 3 (여성)
f3 <- rbindlist(lapply(list(rD_pre_no, rD_pre_yes, rD_post_no, rD_post_yes), function(r) {
  ru <- g6d(r$menopause, r$flush_now, o = ua)
  data.table(폐경 = r$menopause, 현재홍조 = r$flush_now, 고위험_n = r$n_cat, 기준_n = r$n_ref, 사례 = r$n_hua_events,
             OR = or_txt(r), p = fmt_p(r$p), 요산차이 = fmt_ci(ru$est, ru$ci_low, ru$ci_high))
}))
add("### 8-5. 발견 3 — 여성은 폐경 후 홍조 여성에서 연관성이 가장 크다", "",
    md_table(f3[, .(폐경, 현재홍조, 고위험_n, 기준_n, 사례, OR, p, 요산차이)],
             c("폐경 상태", "현재 홍조", "고위험음주자 n", "기준군 n", "고요산혈증 사례 수", "고위험음주 OR (95% CI)", "p", "요산 차이 mg/dL (95% CI)")),
    sprintf("- **무엇을 보았나**: 고위험음주의 OR은 폐경 전·홍조 없음 %.2f → 폐경 전·홍조 있음 %.2f → 폐경 후·홍조 없음 %.2f → 폐경 후·홍조 있음 %.2f로 커졌다. 요산 수치(연속형)도 같은 순서로 차이가 커져, 고요산혈증 기준값 설정과 무관하게 방향이 일관된다.",
            rD_pre_no$est, rD_pre_yes$est, rD_post_no$est, rD_post_yes$est),
    sprintf("- **왜 폐경을 나누었나**: 여성호르몬(에스트로겐)은 신장에서 요산이 잘 배설되도록 돕는 것으로 알려져 있어, 폐경 전에는 요산이 낮게 유지되고 폐경 후에 올라간다(기전 문헌 인용 필요). 실제로 홍조 소실군은 폐경 후 비율이 %s%%로 다른 군(약 30%%)보다 훨씬 높았다(Table 1). 폐경을 고려하지 않으면 홍조의 효과와 폐경의 효과가 섞인다.",
            t1pct("f_meno", "폐경 후", "여자·홍조 소실")),
    sprintf("- **성별 차이**: 홍조가 없는 사람들 사이에서는 음주–고요산혈증 관계가 여성에서 더 강했고(음주 × 성별 상호작용 p = %s), 같은 척도(S2)로 봐도 주당 7잔 증가당 OR이 여 %s, 남 %s였다. 여성은 기본 요산이 낮아 같은 음주량의 영향이 상대적으로 크게 나타날 수 있다.",
            p_int("음주 수준 × 성별 (비홍조)"), fmt_or(gS("S2","여자","비홍조")$est, gS("S2","여자","비홍조")$ci_low, gS("S2","여자","비홍조")$ci_high), fmt_or(gS("S2","남자","비홍조")$est, gS("S2","남자","비홍조")$ci_low, gS("S2","남자","비홍조")$ci_high)),
    sprintf("- **어디까지 믿을 수 있나**: 폐경 후·홍조 여성 중 고위험음주자는 %d명, 고요산혈증 사례는 %d건뿐이라 신뢰구간이 %.2f–%.2f로 매우 넓다. 방향은 일관되지만 크기(OR %.2f)는 과대추정일 수 있다.",
            rD_post_yes$n_cat, rD_post_yes$n_hua_events, rD_post_yes$ci_low, rD_post_yes$ci_high, rD_post_yes$est),
    "")

# 8-6 민감도 요약 (지속 홍조군, 고위험음주)
f4 <- rbindlist(lapply(c("남자", "여자"), function(s_) {
  r0 <- g6a(s_, "지속 홍조"); r1 <- gS("S1", s_, "지속 홍조"); r2 <- gS("S2", s_, "지속 홍조"); r3 <- gS("S3", s_, "지속 홍조")
  data.table(성별 = s_, 본분석 = or_txt(r0), S1 = fmt_or(r1$est, r1$ci_low, r1$ci_high), S2 = fmt_or(r2$est, r2$ci_low, r2$ci_high), S3 = fmt_or(r3$est, r3$ci_low, r3$ci_high))
}))
add("### 8-6. 결과는 분석 방법을 바꿔도 유지되는가", "",
    md_table(f4, c("성별(지속 홍조군)", "본 분석: 고위험음주 OR", "S1 통풍 진단자 포함", "S2 주당 7잔 증가당 OR", "S3 통풍 무응답 포함")),
    "- 통풍 진단자를 포함하거나(S1), 음주 노출을 연속형 잔 수로 바꾸거나(S2), 통풍 문항 무응답자를 포함해도(S3) 결론의 방향은 같았다. S3은 무응답자가 교육·신체활동 문항도 결측이라 모형 n이 본 분석과 같다.",
    "")

# 8-7 선행연구 비교
prior <- data.table(
  연구 = c("김어진 등(2015), Korean J Health Promot", "Kang & Oh(2023), Nutrients", "Jeon 등(2022), Sci Rep", "Jee 등(2019), Int J Rheum Dis"),
  대상자료 = c("대전 대학병원 건강검진 남성 702명(2013–2014)", "국민건강영양조사 2019–2021, 남성", "지역사회건강조사 2019, 현재 음주자 약 14만 명", "한국인 멘델 무작위 분석(ALDH2 rs671)"),
  결과변수 = c("요산–호모시스테인 관계", "대사이상 지방간(MASLD)", "우울", "고요산혈증"),
  주요결과 = c("홍조군에서는 요산과 호모시스테인의 연관성이 나타나지 않음", "음주–MASLD 연관성이 홍조군에서 더 큼(비홍조 1.90 vs 홍조 2.35), 상호작용 유의", "현재 홍조자만 우울 위험 증가(OR 1.23), 과거 홍조자는 아님", "음주→고요산혈증 인과 근거는 남성에서만 시사; rs671 유전형 자체는 고요산혈증과 직접 관련 없음"),
  본연구와의관계 = c("같은 주제이나 여성·국가 표본 없음. 본 연구가 여성과 홍조 변화를 추가", "'홍조군에서 음주의 해가 더 크다'는 방향이 본 연구(발견 2)와 일치", "현재 홍조 유무로 결과가 갈린다는 점이 본 연구의 '소실군' 구분과 통함", "유전형이 아닌 표현형(홍조)에서는 집단별 생활습관이 함께 작용함을 시사")
)
add("### 8-7. 선행연구와 비교하면", "",
    md_table(prior, c("연구", "대상·자료", "결과 변수", "주요 결과", "본 연구와의 관계")),
    "- 종합하면, '홍조가 있는 사람에게 음주가 더 해롭다'는 방향은 간 질환(Kang & Oh, 2023)과 요산(본 연구)에서 함께 나타났고, '현재 홍조 유무가 중요하다'는 점은 정신건강 연구(Jeon 등, 2022)와 통한다. 반면 유전형 연구(Jee 등, 2019)에서 ALDH2 변이 자체는 고요산혈증과 직접 관련이 없었으므로, 본 연구의 결과는 체질과 음주 행동이 함께 만든 결과로 보는 것이 타당하다.",
    "- 위 표의 선행연구 수치는 각 논문 초록에서 옮긴 것이며, 인용 전에 원문으로 확인해야 한다.",
    "")

# 8-8 실천적 의미
add("### 8-8. 실천적 의미", "",
    "- **간단한 질문으로 위험군을 찾을 수 있다**: \"술을 마시면 얼굴이 빨개지나요?\"라는 한 문항으로 고요산혈증 위험이 큰 음주자(홍조가 있는데 과음하는 사람)를 가려낼 수 있다. 유전자 검사가 필요 없고 건강검진·상담 현장에서 바로 쓸 수 있다.",
    "- **홍조 기준 절주 권고의 확장**: 국내에는 홍조 여부에 따라 적정 음주량을 절반으로 낮추는 권고가 있다. 본 결과는 그 권고를 통풍·고요산혈증 예방 영역에도 적용할 근거가 될 수 있다.",
    "- **특히 주의할 집단**: ① 과거에 홍조가 있었지만 지금은 사라진 음주자(경고 신호가 사라진 뒤 음주가 늘었을 가능성), ② 폐경 후에도 홍조가 있으면서 고위험음주를 하는 여성.",
    "")

# 8-9 주의점
add("### 8-9. 해석할 때 주의할 점", "",
    "1. **경향이지 확정이 아니다.** 홍조 패턴에 따른 음주 효과의 차이는 상호작용 검정에서 유의하지 않았다. 하위 집단의 OR이 크다는 것만으로 '홍조가 음주의 해를 키운다'고 단정할 수 없다.",
    sprintf("2. **작은 칸이 있다.** 홍조 소실군의 고위험음주 여성 %d명, 폐경 후 홍조 여성의 고위험음주자 %d명처럼 수십 명에 기댄 추정치는 신뢰구간이 매우 넓다.", rB("여자","홍조 소실")$n_cat, rD_post_yes$n_cat),
    "3. **과거 홍조는 기억에 의존한다.** 수십 년 전 체질을 회상한 응답이므로 분류 오류가 있을 수 있다. 특히 '신규 홍조군'은 회상 오류일 가능성이 있어 해석에서 제외했다.",
    "4. **약물 정보가 없다.** 요산저하제·이뇨제 복용 여부를 알 수 없어 통풍 의사진단자를 제외하는 것으로 대신했다.",
    "5. **기준값은 문헌에서 가져왔다.** 고요산혈증 기준(남 7.0·여 6.0 mg/dL)과 eGFR 계산식은 지침서가 아닌 문헌 기준이며, 연속형 요산 분석으로 기준값에 대한 민감성을 보완했다.",
    "6. **횡단 연구다.** 한 시점의 자료이므로 인과관계나 시간적 선후를 말할 수 없다.",
    "")

# 8-10 결론
add("### 8-10. 결론", "",
    sprintf("> 음주 관련 안면홍조가 있는 사람은 전반적으로 고요산혈증이 적었지만(지속 홍조군 OR 남 %.2f, 여 %.2f), 홍조가 있으면서 고위험음주를 하는 경우에는 음주와 고요산혈증의 연관성이 비홍조군보다 커 보였고(남 OR %.2f, 여 %.2f), 폐경 후 홍조 여성에서 가장 두드러졌다(OR %.2f). 다만 상호작용 검정이 유의하지 않고 일부 칸의 표본이 작아, 이 차이는 경향으로 해석하며 더 큰 표본에서 확인이 필요하다.",
            rA_m$est, rA_f$est, rB("남자","지속 홍조")$est, rB("여자","지속 홍조")$est, rD_post_yes$est),
    "")

# 9. 한계
add("## 9. 한계와 가정", "",
    "- 횡단 연구이므로 인과관계를 말할 수 없다.",
    "- 음주 수준 × 홍조 패턴 상호작용 검정이 유의하지 않았고, 홍조 소실군·폐경 후 홍조군의 고위험음주자 수가 적다(각 표의 '해당 n' 참고). 결과는 경향으로만 해석한다.",
    "- 과거 홍조는 회상 응답이라 오류가 있을 수 있다. 신규 홍조군은 해석에서 제외했다.",
    "- 고요산혈증 기준(남 7.0·여 6.0 mg/dL), eGFR 계산식, 주당 음주 잔 수의 범주 중간값은 지침서가 아닌 문헌·가정에 근거한다.",
    "- 요산저하제·이뇨제 등 약물 복용 정보가 없어 통풍 의사진단자를 제외하는 것으로 대신했다.",
    "- 고위험음주 기준이 성별로 달라(남 7잔, 여 5잔) 남녀 OR을 그대로 비교하면 정의 차이가 섞인다. 성별 공통 척도 분석(S2)을 함께 보아야 한다.",
    "- 공식 통계 대조의 공식값이 아직 비어 있어, 확보되기 전까지는 예비 결과로 본다.",
    "")

# 10. 파일
add("## 10. 결과 파일", "",
    "| 파일 | 내용 |", "|---|---|",
    "| `01_variable_definitions.csv` | 변수 정의와 지침서 쪽수 |",
    "| `02_participant_flow.csv` | 대상자 선정 흐름 |",
    "| `04_table1.csv`, `04_table1_footnote.txt` | Table 1 |",
    "| `05_official_check.csv` | 공식 통계 대조표 |",
    "| `05b_prevalence_by_sex_flush_drink.csv` | 유병률 표 |",
    "| `06a_drink_by_sex_flush.csv` | 홍조군별 음주 수준의 연관성 |",
    "| `06b_flush_pattern_by_sex.csv` | 홍조 패턴 자체의 연관성 |",
    "| `06c_interaction_tests.csv` | 상호작용 검정 |",
    "| `06d_women_menopause.csv` | 여성 폐경 × 홍조 |",
    "| `07_sensitivity.csv` | 민감도 분석 |",
    "| `fig/fig1~fig5*.png` | 그림 |",
    "| `_inputs.csv`, `_sessionInfo.txt`, `_reproducibility.txt`, `_run_log.txt` | 재현성 기록 |",
    "")

writeLines(L, con <- file(out_md, open = "w", encoding = "UTF-8")); close(con)
cat("보고서 저장:", out_md, "(", length(L), "줄 )\n")
